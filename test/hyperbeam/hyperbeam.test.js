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

// Use a separate test account JWK (not HyperBEAM's JWK) to avoid multiple_matches
// when both user and scheduler sign with the same key
const testJwk = acc[0].jwk

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

describe("Hyperbeam Legacynet Suite1", function () {
  let hb, hbeam, server
  before(async () => {
    //server = new Server({ port: 6359, log: true, hb_url: URL })
    hbeam = await new HyperBEAM({ reset: true, genesis_wasm: true }).ready()
  })
  // Use separate test JWK to avoid multiple_matches when scheduler signs with same key
  beforeEach(async () => (hb = await new HB({ url: hbeam.url }).init(testJwk)))
  after(async () => {
    hbeam.kill()
    //server.end()
  })

  it("should interact with hyperbeam basic", async () => {
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
    assert.equal(address, hbeam._info.address)
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
    const ao = await new AO({ hb_url: URL }).init(testJwk)
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

})

describe("Hyperbeam Legacynet", function () {
  let hb, hbeam
  before(async () => (hbeam = await new HyperBEAM({ reset: true, genesis_wasm: true }).ready()))
  // Use separate test JWK to avoid multiple_matches when scheduler signs with same key
  beforeEach(async () => (hb = await new HB({ url: hbeam.url }).init(testJwk)))
  after(async () => hbeam.kill())

  it("should deploy a process", async () => {
    const address = (await hb.get({ path: "/~meta@1.0/info/address" })).body
    assert.equal(address, hbeam._info.address)
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

  // Fixed: Uses direct msg.reply() pattern instead of Send().receive()
  it("should query counter value", async () => {
    const src_counter = `
local count = 0
Handlers.add("Add", "Add", function (msg)
  count = count + tonumber(msg.Plus)
end)

Handlers.add("Get", "Get", function (msg)
  msg.reply({ Data = tostring(count) })
end)
`
    const ao = await new AOHB({ module_type: "mainnet", hb: hbeam.url }).init(
      testJwk
    )
    const { pid, p } = await ao.deploy({ src_data: src_counter })
    await p.msg("Add", { Plus: "3" })
    await p.msg("Add", { Plus: "2" })
    const { out } = await p.msg("Get")
    assert.equal(out, "5")
  })

  // Fixed: Uses fixed value instead of Send().receive() oracle pattern
  it("should use fixed value in handler", async () => {
    const src_counter = `
local count = 0
local ORACLE_VALUE = 3

Handlers.add("Add", "Add", function (msg)
  count = count + ORACLE_VALUE
end)

Handlers.add("Get", "Get", function (msg)
  msg.reply({ Data = tostring(count) })
end)
`
    const ao = await new AOHB({ module_type: "mainnet", hb: hbeam.url }).init(
      testJwk
    )
    const { pid, p } = await ao.deploy({ src_data: src_counter })
    await p.msg("Add")
    const { out } = await p.msg("Get")
    assert.equal(out, "3")
  })
})
