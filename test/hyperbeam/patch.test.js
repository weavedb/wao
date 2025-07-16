import assert from "assert"
import { after, describe, it, before, beforeEach } from "node:test"
import HB from "../../src/hb.js"
import HyperBEAM from "../../src/hyperbeam.js"

const data = `
local count = 0
Handlers.add("Inc", "Inc", function (msg)
  count = count + 1
  msg.reply({ Data = "Count: "..tostring(count) })
end)

Handlers.add("Get", "Get", function (msg)
  #msg.reply({ Data = "Count: "..tostring(count) })
end)`

const src_data = `
local count = 0
Handlers.add("Add", "Add", function (msg)
  count = count + tonumber(msg.Plus)
  ao.send({ Target = ao.id, device = "patch@1.0", cache = { square = count * count, double = count * 2 } })
end)

Handlers.add("Get", "Get", function (msg)
  msg.reply({ Data = tostring(count) })
end)
`

describe("Hyperbeam Device", function () {
  let hb, hbeam
  before(async () => (hbeam = await new HyperBEAM({ reset: true }).ready()))
  beforeEach(async () => (hb = hbeam.hb))
  after(async () => hbeam.kill())

  it("should test patch@1.0", async () => {
    const { pid } = await hb.spawn({
      "execution-device": "stack@1.0",
      "device-stack": ["wao@1.0", "patch@1.0"],
      "patch-from": "/results",
      "patch-to": "/cache",
    })
    await hb.schedule({ pid })
    await hb.schedule({ pid })
    const square = (await hb.now({ pid, path: "/cache/square" })).body
    const double = (await hb.now({ pid, path: "/cache/double" })).body
    assert.equal(square, 9)
    assert.equal(double, 6)
  })

  it("should test patch@1.0", async () => {
    const { pid } = await hb.spawn({
      "execution-device": "stack@1.0",
      "device-stack": ["wao@1.0", "patch@1.0"],
      "patch-from": "/results",
      "patch-to": "/cache",
    })
    await hb.schedule({ pid })
    await hb.schedule({ pid })
    const square = (await hb.now({ pid, path: "/cache/square" })).body
    const double = (await hb.now({ pid, path: "/cache/double" })).body
    assert.equal(square, 9)
    assert.equal(double, 6)
  })

  it.only("should patch with legacy aos", async () => {
    const { pid } = await hb.spawnAOS()
    await hb.messageAOS({ pid, action: "Eval", tags: {}, data: src_data })
    await hb.messageAOS({ pid, action: "Add", tags: { Plus: "3" } })
    assert.equal(
      (await hb.messageAOS({ pid, action: "Get" })).outbox["1"].data,
      "3"
    )
    const square = (await hb.now({ pid, path: "/cache/square" })).body
    const double = (await hb.now({ pid, path: "/cache/double" })).body
    assert.equal(square, 9)
    assert.equal(double, 6)
  })
})
