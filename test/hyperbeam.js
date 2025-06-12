import assert from "assert"
import { after, describe, it, before, beforeEach } from "node:test"
import { acc, mu, AO, toAddr } from "../src/test.js"
import { getJWK } from "./lib/test-utils.js"
import HB from "../src/hb.js"
import { isNotNil, filter } from "ramda"
import { randomBytes } from "node:crypto"
import { wait } from "../src/utils.js"
import Server from "../src/server.js"
import HyperBEAM from "../src/hyperbeam.js"

const data = `
local count = 0
Handlers.add("Inc", "Inc", function (msg)
  count = count + 1
  msg.reply({ Data = "Count: "..tostring(count) })
end)

Handlers.add("Get", "Get", function (msg)
  msg.reply({ Data = "Count: "..tostring(count) })
end)
`

const URL = "http://localhost:10001"

describe("Hyperbeam Legacynet", function () {
  let hb, hbeam, jwk, server
  before(async () => {
    server = new Server({ port: 4000, log: true, hb_url: URL })
    hbeam = new HyperBEAM({
      c: "12",
      cmake: "3.5",
      gateway: 4000,
      legacy: true,
    })
    jwk = getJWK("../../HyperBEAM/.wallet.json")
    await wait(5000)
  })
  beforeEach(async () => (hb = await new HB({}).init(jwk)))
  after(async () => hbeam.kill())

  it("should interact with a hyperbeam node", async () => {
    const { pid, res } = await hb.spawnLegacy()
    const { slot } = await hb.scheduleLegacy({ pid, data })
    const r = await hb.computeLegacy({ pid, slot })
    const { slot: slot2 } = await hb.scheduleLegacy({ pid, action: "Inc" })
    const r2 = await hb.computeLegacy({ pid, slot: slot2 })
    assert.equal(r2.Messages[0].Data, "Count: 1")
    const r3 = await hb.dryrun({ pid, action: "Get" })
    assert.equal(r3.Messages[0].Data, "Count: 1")
  })

  it("should get messages and recover them", async () => {
    const res = await hb.get({ path: "~meta@1.0/info/address" })
    const address = res
    assert.equal(address, hb._info.address)
    const { pid } = await hb.spawnLegacy()
    const { slot } = await hb.scheduleLegacy({
      pid,
      data,
    })
    const r = await hb.computeLegacy({ pid, slot })
    let i = 0
    while (i < 10) {
      const { slot: slot2 } = await hb.scheduleLegacy({
        pid,
        action: "Inc",
      })
      const r3 = await hb.computeLegacy({ pid, slot: slot2 })
      assert.equal(r3.Messages[0].Data, `Count: ${++i}`)
    }
    const res4 = await hb.messages({ pid, from: 0 })
    assert.equal(res4.edges.length, i + 2)

    // recover process
    const ao = await new AO({ hb_url: URL }).init(jwk)
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

  it("should deploy a process", async () => {
    const address = await hb.get({ path: "~meta@1.0/info/address" })
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
    return
  })

  it("should test test device", async () => {
    const info = await hb.meta.info()
    assert.equal(info.address, toAddr(jwk.n))
    const { pid } = await hb.spawn({ "execution-device": "test-device@1.0" })
    const { slot } = await hb.schedule({ pid })
    const res = await hb.compute({ pid, slot })
    assert.equal(res.results["assignment-slot"], 1)
    const {
      edges: [
        edge0,
        {
          node: { assignment, message },
        },
      ],
    } = await hb.messages({ pid })
    assert.equal(message.Target, pid)
  })

  it("should query test-device@1.0", async () => {
    const res = await hb.send({ path: "/~meta@1.0/info", configA: "valA" })
    const configA = await hb.text("meta", "info/configA")
    assert.equal(configA, "valA")
  })
})
