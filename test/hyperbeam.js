import assert from "assert"
import { after, describe, it, before, beforeEach } from "node:test"
import { acc, mu, AO } from "../src/test.js"
import HB from "../src/hb.js"
import { isNotNil, filter } from "ramda"
const [{ jwk, addr }] = acc
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
  let hb, hbeam
  before(async () => {
    hbeam = new HyperBEAM({
      c: "12",
      cmake: "3.5",
      gateway: 4000,
      legacy: true,
    })
    await wait(5000)
  })
  after(async () => hbeam.kill())

  it("should interact with a hyperbeam node", async () => {
    const server = new Server({ port: 4000, log: true, hb_url: URL })
    const hb = await new HB({ url: "http://localhost:10001" }).init(jwk)
    const process = await hb.process({ tags: { authority: mu.addr } })
    const { slot } = await hb.schedule({
      pid: process,
      data,
      tags: { Action: "Eval" },
    })
    const r = await hb.compute(process, slot)
    const { slot: slot2 } = await hb.schedule({
      pid: process,
      tags: { Action: "Inc" },
    })
    const r2 = await hb.compute(process, slot2)
    assert.equal(r2.results.outbox["1"].data, "Count: 1")
    const r3 = await hb.dryrun({ process, tags: { Action: "Get" } })
    assert.equal(r3.Messages[0].Data, "Count: 1")
  })

  it("should get messages and recover them", async () => {
    const server = new Server({ port: 4000, log: true, hb_url: URL })
    const hb = await new HB().init(jwk)
    const res = await hb.get({ path: "~meta@1.0/info/address" })
    const address = res
    assert.equal(address, hb._info.address)
    const process = await hb.process()
    const { slot } = await hb.schedule({
      pid: process,
      data,
      tags: { Action: "Eval" },
    })
    const r = await hb.computeLegacy(process, slot)
    let i = 0
    while (i < 10) {
      const { slot: slot2 } = await hb.schedule({
        pid: process,
        tags: { Action: "Inc" },
      })
      const r3 = await hb.computeLegacy(process, slot2)
      assert.equal(r3.Messages[0].Data, `Count: ${++i}`)
    }
    const res4 = await hb.messages({ target: process, from: 0 })
    assert.equal(res4.edges.length, i + 2)
    return
    // recover process
    const ao = await new AO({ hb_url: URL }).init(jwk)
    assert.equal((await ao.recover(process)).recovered, 12)

    const d4 = await ao.hb.dryrun({ process, action: "Get" })
    assert.equal(d4.Messages[0].Data, `Count: ${i}`)

    // skip recovery if messages already exists
    assert.equal((await ao.recover(process)).recovered, 0)

    // add 2 messages
    while (i < 12) {
      const { slot: slot2 } = await hb.schedule({ pid: process, action: "Inc" })
      const r3 = await hb.computeLegacy(process, slot2)
      assert.equal(r3.Messages[0].Data, `Count: ${++i}`)
    }
    // continue recovery from the last message
    assert.equal((await ao.recover(process)).recovered, 2)

    await server.end()

    // restart a new server and check recovery
    const server2 = new Server({ port: 4000, log: true, hb_url: URL })
    const { slot: slot2 } = await hb.schedule({ pid: process, action: "Inc" })
    const r3 = await hb.computeLegacy(process, slot2)
    assert.equal(r3.Messages[0].Data, `Count: ${++i}`)
  })

  it.only("should deploy a process", async () => {
    const server = new Server({ port: 4000, log: true, hb_url: URL })
    const hb = await new HB().init(jwk)
    const address = await hb.get({ path: "~meta@1.0/info/address" })
    assert.equal(address, hb._info.address)
    const process = await hb.process()
    const { slot } = await hb.schedule({
      pid: process,
      data,
      tags: { Action: "Eval" },
    })
    const r = await hb.computeLegacy(process, slot)
    assert.equal(r.Output.data, "")
    const { slot: slot2 } = await hb.schedule({
      pid: process,
      tags: { Action: "Inc" },
    })
    const r3 = await hb.computeLegacy(process, slot2)
    assert.equal(r3.Messages[0].Data, "Count: 1")
    const { slot: slot3 } = await hb.schedule({
      pid: process,
      tags: { Action: "Inc" },
    })
    const r4 = await hb.computeLegacy(process, slot3)
    assert.equal(r4.Messages[0].Data, "Count: 2")
    const d4 = await hb.dryrun({ process, action: "Get" })
    assert.equal(d4.Messages[0].Data, "Count: 2")
    console.log(process)
    return
  })
})
