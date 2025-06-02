import assert from "assert"
import { after, describe, it, before, beforeEach } from "node:test"
import { acc, mu, AO } from "../src/test.js"
import HB from "../src/hb.js"
import { isNotNil, filter } from "ramda"
const [{ jwk, addr }] = acc
import { randomBytes } from "node:crypto"
import { wait } from "../src/utils.js"
import Server from "../src/server.js"
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
  after(() => setTimeout(() => process.exit(), 100))
  it.only("should interact with a hyperbeam node", async () => {
    const server = new Server({ port: 4000, log: true, hb_url: URL })
    const hb = await new HB({ url: "http://localhost:10001" }).init(jwk)
    const process = await hb.process({ tags: { authority: mu.addr } })
    console.log("[PID]", process)
    const slot = await hb.schedule({ process, data })
    const r = await hb.compute({ process, slot })
    const slot2 = await hb.schedule({ process, action: "Inc" })
    const r2 = await hb.compute({ process, slot: slot2 })
    assert.equal(r2.Messages[0].Data, "Count: 1")
    const r3 = await hb.dryrun({ process, action: "Get" })
    assert.equal(r3.Messages[0].Data, "Count: 1")
  })
  it("should get messages and recover them", async () => {
    const server = new Server({ port: 4000, log: true, hb_url: URL })
    const hb = await new HB().init(jwk)
    const res = await hb.get({ path: "~meta@1.0/info/address" })
    const address = res
    assert.equal(address, hb._info.address)
    const process = await hb.process()
    const slot = await hb.schedule({ process, data })
    const r = await hb.compute({ process, slot })
    assert.equal(r.Output.data, "")
    let i = 0
    while (i < 10) {
      const slot2 = await hb.schedule({ process, action: "Inc" })
      const r3 = await hb.compute({ process, slot: slot2 })
      assert.equal(r3.Messages[0].Data, `Count: ${++i}`)
    }
    const res4 = await hb.messages({ target: process, from: 0 })
    assert.equal(res4.edges.length, i + 2)

    // recover process
    const ao = await new AO({ hb_url: URL }).init(jwk)
    assert.equal((await ao.recover(process)).recovered, 12)

    const d4 = await ao.hb.dryrun({ process, action: "Get" })
    assert.equal(d4.Messages[0].Data, `Count: ${i}`)

    // skip recovery if messages already exists
    assert.equal((await ao.recover(process)).recovered, 0)

    // add 2 messages
    while (i < 12) {
      const slot2 = await hb.schedule({ process, action: "Inc" })
      const r3 = await hb.compute({ process, slot: slot2 })
      assert.equal(r3.Messages[0].Data, `Count: ${++i}`)
    }
    // continue recovery from the last message
    assert.equal((await ao.recover(process)).recovered, 2)

    await server.end()

    // restart a new server and check recovery
    const server2 = new Server({ port: 4000, log: true, hb_url: URL })
    const slot2 = await hb.schedule({ process, action: "Inc" })
    const r3 = await hb.compute({ process, slot: slot2 })
    assert.equal(r3.Messages[0].Data, `Count: ${++i}`)
  })

  it("should deploy a process", async () => {
    const hb = await new HB().init(jwk)
    const address = await hb.get({ path: "~meta@1.0/info/address" })
    assert.equal(address, hb._info.address)
    const p = await hb.process()
    const process = await p.process.text()
    const slot = await hb.schedule({ process, data })
    const r = await hb.compute({ process, slot })
    assert.equal(r.Output.data, "")
    const slot2 = await hb.schedule({ process, action: "Inc" })
    const r3 = await hb.compute({ process, slot: slot2 })
    assert.equal(r3.Messages[0].Data, "Count: 1")
    const slot3 = await hb.schedule({ process, action: "Inc" })
    const r4 = await hb.compute({ process, slot: slot3 })
    assert.equal(r4.Messages[0].Data, "Count: 2")
    const d4 = await hb.dryrun({ process, action: "Get" })
    assert.equal(d4.Messages[0].Data, "Count: 2")
    console.log(process)
    return
  })
})
