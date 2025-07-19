import assert from "assert"
import { after, describe, it, before, beforeEach } from "node:test"
import HyperBEAM from "../../src/hyperbeam.js"
import AO from "../../src/ao.js"
import TAO from "../../src/tao.js"

const src_data = `
local count = 0
Handlers.add("Add", "Add", function (msg)
  count = count + tonumber(msg.Plus)
end)

Handlers.add("Get", "Get", function (msg)
  msg.reply({ Data = tostring(count) })
end)
`

const src_data2 = `
Handlers.add("Hello2", "Hello2", function (msg)
  local name = Send({ Target = ao.id, Action = "Reply" }).receive().Data
  msg.reply({ Hello = "Hello, " .. name .. "!" })
end)

Handlers.add("Reply", "Reply", function (msg)
  msg.reply({ Data = "Japan" })
end)
`

describe("Hyperbeam Legacynet", function () {
  let hbeam, ao, ao2
  before(async () => (hbeam = await new HyperBEAM({ reset: true }).ready()))

  beforeEach(async () => {
    ao = await new AO({ module_type: "mainnet", hb: hbeam.url }).init(hbeam.jwk)
    ao2 = await new AO({ module_type: "mainnet", hb: hbeam.url }).init(
      hbeam.jwk
    )
  })
  after(async () => {
    hbeam.kill()
  })

  it("should interact with hyperbeam using WAO SDK", async () => {
    const { pid, p } = await ao.deploy({ src_data })
    const { out } = await p.msg("Get")
    assert.equal(out, "0")
    await p.msg("Add", { Plus: "3" })
    const { out: out2 } = await p.msg("Get")
    assert.equal(out2, "3")
  })
  it("should spawn a message from a handler with receive", async () => {
    const { p, pid } = await ao.deploy({ boot: true, src_data: src_data2 })
    assert.equal(
      await p.m("Hello2", { get: "Hello", timeout: 3000 }),
      "Hello, Japan!"
    )
  })
  it("should get with optional match", async () => {
    const src_data = `
local json = require("json")
Handlers.add("Hello", "Hello", function (msg)
  Send({Target = msg.To, Data = "Hello" })
  Send({Target = msg.From, Data = json.encode({ Hello = "World", Age = 5 })})
  Send({Target = msg.From, Tag = json.encode({ Hello = "AO" }), Data = json.encode({ Hello = "World!" })})
end)
`
    const { p, pid } = await ao.deploy({ src_data })
    assert.deepEqual(
      await p.d("Hello", {
        get: {
          json: true,
          data: true,
          match: (v, i, r) => v.Hello !== "World",
        },
      }),
      { Hello: "World!" }
    )
    assert.deepEqual(
      await p.d("Hello", {
        get: {
          name: "Tag",
          json: true,
          match: v => v.Hello === "AO",
        },
      }),
      { Hello: "AO" }
    )
    assert.deepEqual(
      await p.d("Hello", {
        get: {
          data: true,
          json: true,
          match: v => v.Age < 10,
        },
      }),
      { Hello: "World", Age: 5 }
    )
  })

  it("should handle replies between multiple processes", async () => {
    const src_data = `
Handlers.add("Hello", "Hello", function (msg)
  local name = Send({ Target = msg.To, To = ao.id, Action = "Reply" }).receive().Data
  msg.reply({ Hello = "Hello, " .. name .. "!" })
end)

Handlers.add("Reply", "Reply", function (msg)
  msg.reply({ Data = "Japan" })
end)
`
    const { p, pid } = await ao.deploy({ src_data })
    const { p: p2, pid: pid2 } = await ao2.deploy({ src_data })
    assert.equal(
      await p.m("Hello", { To: pid2 }, { get: "Hello" }),
      "Hello, Japan!"
    )
  })
})
