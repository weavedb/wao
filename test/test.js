import assert from "assert"
import { resolve } from "path"
import { unlinkSync } from "fs"
import { afterEach, after, describe, it, before, beforeEach } from "node:test"
import { blueprint, mu, AO, connect, acc, scheduler } from "../src/test.js"
import Server from "../src/server.js"
import MAO from "../src/ao.js"
import AR from "../src/ar.js"
import GQL from "../src/gql.js"
import ArMem from "../src/armem.js"
import { setup, Src } from "../src/helpers.js"
import { optAO, optServer, tags, wait } from "../src/utils.js"

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
  msg.reply({ Hello = "Hello, " .. name .. "!" })
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
Handlers.add("Get-Data", "Get-Data", function (msg)
  msg.reply({ Data = Drive.getData(msg.id) })
end)
`

describe("GraphQL", () => {
  it.skip("should query with graphql", async () => {
    const gql = new GQL({ url: "http://localhost:4000/graphql" })
    const height = (await gql.blocks({ first: 1 }))[0].height
    const ar = new AR({ port: 4000 })
    const { id } = await ar.post({
      tags: { test: 2, "Content-Type": "text/plain" },
    })
    const height2 = (await gql.blocks({ first: 1 }))[0].height
    assert.equal(height, height2 - 2)
    const id2 = (
      await gql.txs({ first: 1, tags: { test: "2" }, fields: ["id"] })
    )[0].id
    assert.equal(id, id2)
  })

  it("should query with in-memory graphql", async () => {
    const ao = new AO()
    await ao.ar.post({ tags: { test: "1" }, jwk })
    await ao.ar.post({ tags: { test: "2" }, jwk })
    await ao.ar.post({ tags: { test: "3" }, jwk })
    const txs = await ao.ar.gql.txs({
      first: 1,
    })
    assert.equal(txs[0].tags[0].value, "3")
    const txs2 = await ao.ar.gql.txs({
      next: true,
      after: txs[0].cursor,
      first: 1,
      fields: ["id", { owner: { key: false } }, { tags: ["value"] }],
      block: [0, 4],
    })
    assert.equal(txs2.data[0].tags[0].value, "2")
    const txs3 = await txs2.next()
    assert.equal(txs3.data[0].tags[0].value, "1")
    const blocks = await ao.ar.gql.blocks({
      first: 2,
      asc: true,
      fields: { id: true, previous: true, height: true },
    })
    assert.equal(blocks[0].height, 0)
  })
})

describe("Aoconnect", () => {
  it("should work with pre-loaded packages", async () => {
    const mem = new ArMem()
    const ar = new AR({ mem })
    await ar.post({ data: "abc", tags: { test: 3 }, jwk })
  })
  it("should spawn a process and send messages", async () => {
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
  let ao, server, mem
  before(async () => {
    ao = await new AO().init(acc[0])
    mem = ao.mem
  })
  after(async () => {
    if (server) await server.end()
  })

  it("should run", async () => {
    const src = new Src({ dir: resolve(import.meta.dirname, "../src/lua") })
    const data = src.data("process", "wasm")
    const { id: modid } = await ao.postModule({ data, jwk })
    const { p, pid } = await ao.deploy({ module: modid })
    const { res } = await ao.msg({ pid, data: "ping" })
  })

  it("should publish custom modules", async () => {
    const src = new Src({ dir: resolve(import.meta.dirname, "../src/lua") })
    const data = src.data("aos2_0_1", "wasm")
    const { id: modid } = await ao.postModule({ data, jwk })
    const { p, err } = await ao.deploy({ src_data, module: modid })
    assert.equal(await p.d("Hello"), "Hello, World!")
  })

  it("should spawn a process and send messages", async () => {
    const { p } = await ao.deploy({ src_data })
    assert.equal(await p.d("Hello"), "Hello, World!")
  })

  it("should spawn a process with On-Boot tag", async () => {
    const { err, p, pid } = await ao.deploy({ boot: true, src_data })
    assert.equal(await p.d("Hello"), "Hello, World!")
    const { pid: pid2, p: p2 } = await ao.deploy({ boot: pid })
    return
    assert.equal(await p2.d("Hello"), "Hello, World!")
  })

  it("should spawn a message from a handler with receive", async () => {
    const { p, pid } = await ao.deploy({ boot: true, src_data: src_data2 })
    assert.equal(
      await p.m("Hello2", { get: "Hello", timeout: 3000 }),
      "Hello, Japan!",
    )
  })
  it("should spawn a process from a handler", async () => {
    const { p, pid } = await ao.deploy({ boot: true, src_data: src_data3 })
    await p.msg(
      "Hello3",
      { module: mem.modules.aos2_0_1, auth: mu.addr },
      { data: src_data },
    )
    const prs = mem.env
    let p2 = null
    for (let k in prs) {
      for (let k2 of prs[k].results) {
        for (let v of mem.msgs[k2].res.Spawns || []) {
          if (tags(v.Tags)["From-Process"] === pid) p2 = ao.p(k)
        }
      }
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

  it("should work with weavedrive: Assignments", async () => {
    const { p, pid } = await ao.deploy({
      src_data: src_data6,
      tags: { Extension: "WeaveDrive", Attestor: ao.ar.addr },
      data: "Hello, World!",
    })
    await ao.attest({ id: pid })
    assert.equal(await p.d("Hello", { msg: pid }), "Hello, World!")
  })

  it("should work with weavedrive: Individual", async () => {
    const src = new Src({ dir: resolve(import.meta.dirname, "../src/lua") })
    const data = src.data("aos2_0_1", "wasm")
    const { id: modid } = await ao.postModule({
      data,
      jwk,
      tags: { "Availability-Type": "Individual" },
    })

    const { p, pid } = await ao.deploy({
      module: modid,
      src_data: src_data6,
      tags: { Extension: "WeaveDrive", Attestor: ao.ar.addr },
      data: "Hello, World!",
    })
    await ao.avail({ ids: [pid] })
    assert.equal(await p.d("Hello", { msg: pid }), "Hello, World!")
  })

  it("should load apm mock", async () => {
    const { p } = await ao.deploy({
      tags: { Extension: "WeaveDrive", Attestor: ao.ar.addr },
      loads: [
        await blueprint("apm"),
        `apm.install('@rakis/WeaveDrive')`,
        src_data7,
      ],
    })
    const { id } = await ao.ar.post({ data: "Hello, World!" })
    await ao.attest({ id })
    assert.equal(await p.d("Get-Data", { id }), "Hello, World!")
    return
  })

  it("should post tx", async () => {
    const { id } = await ao.ar.post({
      data: "Hello, World!",
      tags: { one: "1" },
      jwk,
    })
    assert.equal(await ao.ar.data(id, true), "Hello, World!")
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

const attestor = acc[0]
const handler = `
apm.install('@rakis/WeaveDrive')
Drive = require('@rakis/WeaveDrive')
Handlers.add("Get", "Get", function (msg)
  msg.reply({ Data = Drive.getData(msg.id) })
end)`

describe("WeaveDrive", () => {
  it("should load Arweave tx data", async () => {
    const ao = await new AO().init(attestor)

    const { p, err } = await ao.deploy({
      tags: { Extension: "WeaveDrive", Attestor: attestor.addr },
      loads: [await blueprint("apm"), handler],
    })

    const { id } = await ao.ar.post({ data: "Hello" })
    await ao.attest({ id })

    assert.equal(await p.d("Get", { id }), "Hello")
  })
})

const src_counter = `
local count = 0

Handlers.add("Add", "Add", function (msg)
  count = count + tonumber(msg.Plus)
end)

Handlers.add("Get", "Get", function (msg)
  msg.reply({ Data = tostring(count) })
end)
`

describe("Fork", function () {
  it("should fork wasm memory", async () => {
    const ao = await new AO().init(acc[0])
    const { p, pid } = await ao.deploy({ boot: true, src_data: src_counter })
    await p.m("Add", { Plus: 3 })
    assert.equal(await p.d("Get"), "3")
    const memory = ao.mem.env[pid].memory
    const ao2 = await new AO().init(acc[0])
    const { p: p2 } = await ao2.spwn({ memory })
    assert.equal(await p2.d("Get"), "3")
    await p2.m("Add", { Plus: 2 })
    assert.equal(await p2.d("Get"), "5")
  })
})

describe("Persistency", function () {
  it("should persist the data", async () => {
    const cache = resolve(import.meta.dirname, ".cache")
    try {
      unlinkSync(cache)
    } catch (e) {}
    const ao = await new AO({ cache }).init(acc[0])
    const { p, pid } = await ao.deploy({ boot: true, src_data: src_counter })
    await p.m("Add", { Plus: 3 })
    assert.equal(await p.d("Get"), "3")
    const ao2 = await new AO({
      cache: resolve(import.meta.dirname, ".cache"),
    }).init(acc[0])
    const p2 = ao2.p(pid)
    assert.equal(await p2.d("Get"), "3")
    await p2.m("Add", { Plus: 2 })
    assert.equal(await p2.d("Get"), "5")
    return
  })
})

const src_data_r1 = `
local json = require("json")
Handlers.add("Hello", "Hello", function (msg)
  Send({Target = msg.To, Action = "Hello2", To2 = msg.To2, To3 = ao.id, Data = "data", Test = "test1"})
  local res = Receive({Data = "Hello"})
  msg.reply({ Data = "Hello, World!", Test2 = "test", JSON = json.encode( { a = 3, b = 4 } ) })
end)
`
const src_data_r2 = `
Handlers.add("Hello2", "Hello2", function (msg)
  Send({Target = msg.To2, Action = "Hello3", To = msg.To3})
end)
`

const src_data_r3 = `
Handlers.add("Hello3", "Hello3", function (msg)
  Send({Target = msg.To, Data = "Hello"})
end)
`

describe("AOS1", function () {
  it("should wait reply from another process", async () => {
    const ao = await new AO({}).init(acc[0])
    const { p, pid } = await ao.deploy({ src_data: src_data_r1 })
    const ao2 = await new AO({ mem: ao.mem }).init(acc[0])
    const { pid: pid2 } = await ao2.deploy({ src_data: src_data_r2 })
    const ao3 = await new AO({ mem: ao.mem }).init(acc[0])
    const { pid: pid3 } = await ao3.deploy({ src_data: src_data_r3 })
    assert.deepEqual(
      await p.m(
        "Hello",
        { To: pid2, To2: pid3 },
        {
          timeout: 2000,
          check: [
            {
              from: pid3,
              data: "Hello, World!",
              tags: { Test2: /test/, JSON: { json: { a: 3, b: () => true } } },
            },
          ],
          get: { test: { name: "Test2", from: pid3 } },
        },
      ),
      { test: "test" },
    )
  })
})
