import assert from "assert"
import { describe, it, before, beforeEach } from "node:test"
import { blueprint, mu, AO, connect, acc, scheduler } from "../src/test.js"
import AR from "../src/ar.js"
import ArMem from "../src/armem.js"
import { setup } from "../src/helpers.js"
import { tags, wait } from "../src/utils.js"

const { mem, spawn, message, dryrun } = connect()
const [{ signer, jwk }] = acc

const src_data = `
Handlers.add("Hello", "Hello", function (msg)
  msg.reply({ Data = "Hello, World!" })
end)
`

const src_data2 = `
Handlers.add("Hello2", "Hello2", function (msg)
  local name = Send({ Target = ao.id, Action = "Reply" }).receive().Data
  msg.reply({ Data = "Hello, " .. name .. "!" })
end)

Handlers.add("Reply", "Reply", function (msg)
  msg.reply({ Data = "Japan" })
end)
`

const src_data3 = `
Handlers.add("Hello3", "Hello3", function (msg)
   Spawn(msg.module, { Data = msg.Data, ["On-Boot"] = "Data" })
end)
`

const src_data4 = `
local count = 0
Handlers.add("Hello4", "Hello4", function (msg)
   Assign({ Message = msg.Message, Processes = { msg.Process } })
end)

Handlers.add("Add", "Add", function (msg)
  count = count + tonumber(msg.Plus)
end)

Handlers.add("Get", "Get", function (msg)
  msg.reply({ Data = tostring(count) })
end)
`

const src_data5 = `
_G.package.loaded.foo = function ()
  return "Hello, World!"
end

local foo = require("foo")
 
Handlers.add("Hello", "Hello", function (msg)
  msg.reply({ Data = tostring(foo()) })
end)
`

const src_data6 = `
Handlers.add("Hello", "Hello", function (msg)
  local file = io.open('/data/'.. msg.msg)
  if not file then
    return nil, "File not found!"
  end
  local contents = file:read(
    file:seek('end')
  )
  file:close()
  msg.reply({ Data = contents })
end)
`
const src_data7 = `
Drive = require('@rakis/WeaveDrive')

Handlers.add("Hello", "Hello", function (msg)
  --local data = Drive.getData(msg.txid)
  local file = io.open('/data/'.. msg.txid)
  if not file then
    return nil, "File not found!"
  end
  local contents = file:read(
    file:seek('end')
  )
  file:close()
  msg.reply({ Data = contents })
end)

Handlers.add("Hello2", "Hello2", function (msg)
end)
`

describe("AOS Tests", () => {
  it("should work with pre-loaded packages", async () => {
    const mem = new ArMem()
    const ar = new AR({ port: 4000, mem })
    await ar.post({ data: "abc", tags: { test: 3 }, jwk })
  })
  it("should spawn a process send messages", async () => {
    const mem = new ArMem()
    const { armem, spawn, message, dryrun } = connect(mem)

    const pid = await spawn({
      signer,
      scheduler,
      module: mem.modules.aos2_0_1,
    })
    await message({
      process: pid,
      tags: [{ name: "Action", value: "Eval" }],
      data: src_data,
      signer,
    })
    const res = await dryrun({
      process: pid,
      tags: [{ name: "Action", value: "Hello" }],
      signer,
    })
    assert.equal(res.Messages[0].Data, "Hello, World!")
  })
})

describe("SDK", function () {
  let ao
  beforeEach(async () => (ao = await new AO().init(acc[0])))

  it("should spawn a process and send messages", async () => {
    const { p } = await ao.deploy({ src_data })
    assert.equal(await p.d("Hello"), "Hello, World!")
  })

  it("should spawn a process with On-Boot tag", async () => {
    const { p, pid } = await ao.deploy({ boot: true, src_data })
    assert.equal(await p.d("Hello"), "Hello, World!")
    const { p: p2 } = await ao.deploy({ boot: pid })
    assert.equal(await p2.d("Hello"), "Hello, World!")
  })

  it("should spawn a message from a handler with receive", async () => {
    const { p, pid } = await ao.deploy({ boot: true, src_data: src_data2 })
    assert.equal(await p.m("Hello2"), "Hello, Japan!")
  })

  it("should spawn a process from a handler", async () => {
    const { p, pid } = await ao.deploy({ boot: true, src_data: src_data3 })
    await p.m(
      "Hello3",
      { module: ao.ar.mem.modules.aos2_0_1, auth: mu.addr },
      { data: src_data },
    )
    const prs = ao.ar.mem.env
    let p2 = null
    for (let k in prs) {
      if (tags(prs[k].opt.tags)["From-Process"] === pid) p2 = ao.p(k)
    }
    assert.notEqual(p2, null)
  })

  it("should assign a process from a handler", async () => {
    const { p, pid } = await ao.deploy({ boot: true, src_data: src_data4 })
    const { mid } = await p.msg("Add", { Plus: "3" })
    const { p: p2, pid: pid2 } = await ao.deploy({
      boot: true,
      src_data: src_data4,
    })
    await p.m("Hello4", { Message: mid, Process: pid2 })
    assert.equal(await p2.d("Get", { get: false }), "3")
  })

  it("should work with pre-loaded packages", async () => {
    const { p } = await ao.deploy({ src_data: src_data5 })
    assert.equal(await p.d("Hello"), "Hello, World!")
  })

  it("should work with weavedrive", async () => {
    const { p, pid } = await ao.deploy({
      src_data: src_data6,
      data: "Hello, World!",
    })
    assert.equal(await p.d("Hello", { msg: pid }), "Hello, World!")
  })

  it("should load apm mock", async () => {
    const { p, pid } = await ao.deploy({
      boot: true,
      src_data: await blueprint("apm"),
    })
    await ao.load({ pid, data: `apm.install('@rakis/WeaveDrive')` })
    await ao.load({ pid, data: src_data7 })
    const { mid } = await p.msg("Hello2", { data: "Hello, World!" })
    assert.equal(await p.d("Hello", { txid: mid }), "Hello, World!")
  })

  it("should post tx", async () => {
    const { id } = await ao.ar.post({
      data: "Hello, World!",
      tags: { one: "1" },
      jwk,
    })
    assert.equal(ao.ar.data(id), "Hello, World!")
  })

  it("should monitor crons", async done => {
    const { p, pid } = await ao.deploy({
      boot: true,
      src_data: src_data4,
      tags: {
        "Cron-Tag-Action": "Add",
        "Cron-Tag-Plus": "3",
        "Cron-Interval": "1-second",
      },
    })
    await ao.monitor({ process: pid, signer })
    await wait(3000)
    await ao.unmonitor({ process: pid, signer })
    assert.equal(await p.d("Get"), "6")
  })
})
