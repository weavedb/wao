import assert from "assert"
import base64url from "base64url"
import { after, describe, it, before, beforeEach } from "node:test"
import { acc, mu, AO, toAddr } from "../../src/test.js"
import HB from "../../src/hb.js"
import AOHB from "../../src/ao.js"
import { isNotNil, filter, isNil } from "ramda"
import { randomBytes } from "node:crypto"
import { wait } from "../../src/utils.js"
import Server from "../../src/server.js"
import HyperBEAM from "../../src/hyperbeam.js"
import { readFileSync } from "fs"
import { resolve } from "path"

import AO2 from "../../src/ao.js"

const src_data = `
local count = 0
Handlers.add("Add", "Add", function (msg)
  count = count + tonumber(msg.Plus)
end)

Handlers.add("Get", "Get", function (msg)
  msg.reply({ Data = tostring(count) })
end)
`

const data = `
local count = 0
Handlers.add("Inc", "Inc", function (msg)
  count = count + 1
  msg.reply({ Data = "Count: "..tostring(count) })
end)

Handlers.add("Get", "Get", function (msg)
  msg.reply({ Data = "Count: "..tostring(count) })
end)`

const URL = "http://localhost:10001"

describe("Hyperbeam Legacynet", function () {
  let hb, hbeam, server
  before(async () => {
    server = new Server({ port: 6359, log: true, hb_url: URL })
    hbeam = await new HyperBEAM({ reset: true }).ready()
  })
  beforeEach(async () => (hb = hbeam.hb))
  after(async () => {
    hbeam.kill()
    server.end()
  })

  it("should interact with a hyperbeam node", async () => {
    const { pid } = await hb.spawnLegacy()
    const { slot } = await hb.scheduleLegacy({ pid, data })
    const r = await hb.computeLegacy({ pid, slot })
    const { slot: slot2 } = await hb.scheduleLegacy({ pid, action: "Inc" })
    const r2 = await hb.computeLegacy({ pid, slot: slot2 })
    assert.equal(r2.Messages[0].Data, "Count: 1")
    const { slot: slot3 } = await hb.scheduleLegacy({ pid, action: "Inc" })
    const r4 = await hb.computeLegacy({ pid, slot: slot3 })
    const r3 = await hb.dryrun({ pid, action: "Get" })
    assert.equal(r3.Messages[0].Data, "Count: 2")
  })

  it("should get messages and recover them", async () => {
    const address = (await hb.get({ path: "/~meta@1.0/info/address" })).body
    assert.equal(address, hb._info.address)
    const { pid } = await hb.spawnLegacy()
    const { slot } = await hb.scheduleLegacy({ pid, data })
    const r = await hb.computeLegacy({ pid, slot })
    let i = 0
    while (i < 10) {
      const { slot: slot2 } = await hb.scheduleLegacy({ pid, action: "Inc" })
      const r3 = await hb.computeLegacy({ pid, slot: slot2 })
      assert.equal(r3.Messages[0].Data, `Count: ${++i}`)
    }
    const res4 = await hb.messages({ pid, from: 0 })
    assert.equal(res4.edges.length, i + 2)

    // recover process
    const ao = await new AO({ hb_url: URL }).init(hbeam.jwk)
    assert.equal((await ao.recover(pid)).recovered, 12)

    const d4 = await ao.hb.dryrun({ pid, action: "Get" })
    assert.equal(d4.Messages[0].Data, `Count: ${i}`)
    // skip recovery if messages already exists
    assert.equal((await ao.recover(pid)).recovered, 0)
    // add 2 messages
    while (i < 12) {
      const { slot: slot2 } = await hb.scheduleLegacy({ pid, action: "Inc" })
      const r3 = await hb.computeLegacy({ pid, slot: slot2 })
      assert.equal(r3.Messages[0].Data, `Count: ${++i}`)
    }
    return
    // continue recovery from the last message
    assert.equal((await ao.recover(pid)).recovered, 2)

    await server.end()

    // restart a new server and check recovery
    const server2 = new Server({ port: 4000, log: true, hb_url: URL })
    const { slot: slot2 } = await hb.scheduleLegacy({ pid, action: "Inc" })
    const r3 = await hb.computeLegacy({ pid, slot: slot2 })
    assert.equal(r3.Messages[0].Data, `Count: ${++i}`)
  })

  it("should test test device", async () => {
    const { pid } = await hb.spawn({ "execution-device": "test-device@1.0" })
    const { slot } = await hb.schedule({ pid })
    const res = await hb.compute({ pid, slot })
    assert.equal(res.results["assignment-slot"], 1)
    const {
      edges: [
        _edge0,
        {
          node: { assignment, message },
        },
      ],
    } = await hb.messages({ pid })
    assert.equal(message.Target, pid)
  })

  it("should test add@1.0", async () => {
    const res = await hb.post({ path: "/~add@1.0/add", a: 2, b: 3 })
    assert.equal(res.headers.sum, "5")
  })

  it("should test mul@1.0", async () => {
    const res = await hb.post({ path: "/~mul@1.0/mul", a: 2, b: 3 })
    assert.equal(res.headers.product, "6")
  })

  it("should upload module #2", async () => {
    const { pid } = await hb.spawn({ "execution-device": "wao@1.0" })
    await hb.schedule({ pid })
    await hb.schedule({ pid })
    await hb.schedule({ pid })
    await hb.schedule({ pid })
    assert.equal((await hb.now({ pid })).count, 5)
    assert.equal((await hb.now({ pid })).count, 5)
  })
})

describe("Hyperbeam Legacynet", function () {
  let hb, hbeam
  before(async () => (hbeam = await new HyperBEAM({ reset: true }).ready()))
  beforeEach(async () => (hb = hbeam.hb))
  after(async () => hbeam.kill())

  it("should deploy a process", async () => {
    const address = (await hb.get({ path: "/~meta@1.0/info/address" })).body
    assert.equal(address, hb._info.address)
    const { pid } = await hb.spawnLegacy()
    const { slot } = await hb.scheduleLegacy({ pid, data })
    const r = await hb.computeLegacy({ pid, slot })
    assert.equal(r.Output.data, "")
    const { slot: slot2 } = await hb.scheduleLegacy({ pid, action: "Inc" })
    const r3 = await hb.computeLegacy({ pid, slot: slot2 })
    assert.equal(r3.Messages[0].Data, "Count: 1")
    const { slot: slot3 } = await hb.scheduleLegacy({ pid, action: "Inc" })
    const r4 = await hb.computeLegacy({ pid, slot: slot3 })
    assert.equal(r4.Messages[0].Data, "Count: 2")
    const d4 = await hb.dryrun({ pid, action: "Get" })
    assert.equal(d4.Messages[0].Data, "Count: 2")
  })

  it("should run hyper Lua", async () => {
    const { pid } = await hb.spawnLua()
    await hb.scheduleLua({ pid, action: "Eval", data })
    await hb.scheduleLua({ pid, action: "Inc" })
    const { slot } = await hb.scheduleLua({ pid, action: "Get" })
    const { outbox } = await hb.computeLua({ pid, slot })
    assert.equal(outbox[0].Data, "Count: 1")
    await hb.scheduleLua({ pid, action: "Inc" })
    const { slot: slot2 } = await hb.scheduleLua({ pid, action: "Get" })
    const { outbox: outbox2 } = await hb.computeLua({ pid, slot: slot2 })
    assert.equal(outbox2[0].Data, "Count: 2")
    console.log(await hb.computeLua({ pid, slot }))
  })

  it("should interact with a hyperbeam node", async () => {
    const { pid } = await hb.spawnLegacy()
    const { slot } = await hb.scheduleLegacy({ pid, data })
    const r = await hb.computeLegacy({ pid, slot })
    const { slot: slot2 } = await hb.scheduleLegacy({ pid, action: "Inc" })
    const r2 = await hb.computeLegacy({ pid, slot: slot2 })
    assert.equal(r2.Messages[0].Data, "Count: 1")
    const { slot: slot3 } = await hb.scheduleLegacy({ pid, action: "Inc" })
    const r4 = await hb.computeLegacy({ pid, slot: slot3 })
    const r3 = await hb.dryrun({ pid, action: "Get" })
    assert.equal(r3.Messages[0].Data, "Count: 2")
  })

  it("should handle counter with Add and Get handlers", async () => {
    const { pid } = await hb.spawnAOS()
    await hb.messageAOS({ pid, action: "Eval", tags: {}, data: src_data })
    await hb.messageAOS({ pid, action: "Add", tags: { Plus: "3" } })
    assert.equal(
      (await hb.messageAOS({ pid, action: "Get" })).outbox["1"].data,
      "3"
    )
  })

  it("should execute AOS with WAMR", async () => {
    const { pid } = await hb.spawnAOS()
    await hb.messageAOS({ pid, action: "Eval", tags: {}, data: src_data })
    await hb.messageAOS({ pid, action: "Add", tags: { Plus: "3" } })
    assert.equal(
      (await hb.messageAOS({ pid, action: "Get" })).outbox["1"].data,
      "3"
    )
    await hb.messageAOS({ pid, action: "Add", tags: { Plus: "3" } })
    assert.equal(
      (await hb.messageAOS({ pid, action: "Get" })).outbox["1"].data,
      "6"
    )
  })

  it("should test WAMR", async () => {
    const { pid } = await hb.spawnAOS()
    await hb.scheduleAOS({ pid, action: "Eval", data: src_data })
    await hb.scheduleAOS({ pid, action: "Add", tags: { Plus: "3" } })
    await hb.scheduleAOS({ pid, action: "Get" })
    console.log("compute: 0", await hb.computeAOS({ pid, slot: 0 }))
    console.log("compute: 1", await hb.computeAOS({ pid, slot: 1 }))
    console.log("compute: 3", await hb.computeAOS({ pid, slot: 3 }))
    console.log("compute: 3", await hb.computeAOS({ pid, slot: 3 }))
    console.log("compute: 2", await hb.computeAOS({ pid, slot: 2 }))
  })

  it("should receive msg from another process", async () => {
    const src_data = `
local count = 0
Handlers.add("Add", "Add", function (msg)
  count = count + tonumber(msg.Plus)
end)

Handlers.add("Get", "Get", function (msg)
  msg.reply({ Data = tostring(count) })
end)

Handlers.add("Query", "Query", function (msg)
  local data = Send({ Target = msg.To, Action = "Get" }).receive().Data
  msg.reply({ Data = tostring(data) })
end)
`
    console.log(hbeam.url)
    const ao = await new AOHB({ module_type: "mainnet", hb: hbeam.url }).init(
      hbeam.jwk
    )
    const ao2 = await new AOHB({ module_type: "mainnet", hb: hbeam.url }).init(
      hbeam.jwk
    )
    const { pid, p } = await ao.deploy({ src_data })
    const { pid: pid2, p: p2 } = await ao2.deploy({ src_data })
    await p.m("Add", { Plus: "3" })
    assert.equal(await p2.m("Query", { To: pid }), "3")
  })

  it("should test oracle", async () => {
    const ao = await new AO2({ module_type: "mainnet", hb: hbeam.url }).init(
      hbeam.jwk
    )
    const ao2 = await new AO2({ module_type: "mainnet", hb: hbeam.url }).init(
      hbeam.jwk
    )
    const src_data = `
local count = 0
Handlers.add("Add", "Add", function (msg)
  local data = Send({ Target = msg.To, Action = "Plus" }).receive().Data
  count = count + tonumber(data)
end)

Handlers.add("Get", "Get", function (msg)
  msg.reply({ Data = tostring(count) })
end)
`
    const src_data2 = `
Handlers.add("Plus", "Plus", function (msg)
  msg.reply({ Data = tostring(3) })
end)
`
    const { p, pid } = await ao.deploy({ src_data })
    const { p: p2, pid: pid2 } = await ao2.deploy({ src_data: src_data2 })
    await p.m("Add", { To: pid2 })
    console.log(await p.m("Get"))
  })

  it.only("should test oracle", async () => {
    const src_data = `
local count = 0
json = require("json")
Handlers.add("Add", "Add", function (msg)
  local data = Send({ Target = msg.To, Url = msg.Url }).receive().Data
  count = count + tonumber(json.decode(data).version)
end)

Handlers.add("Get", "Get", function (msg)
  msg.reply({ Data = tostring(count) })
end)
`
    const { pid } = await hb.spawn({ "execution-device": "oracle@1.0" })
    console.log(await hb.message({ pid }))
    console.log(pid)

    const ao = await new AO2({ module_type: "mainnet", hb: hbeam.url }).init(
      hbeam.jwk
    )
    const { p } = await ao.deploy({ src_data })
    await p.m("Add", { To: pid, Url: "https://arweave.net/" })
    console.log(await p.m("Get"))
  })
})
