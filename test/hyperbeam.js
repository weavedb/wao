import assert from "assert"
import { after, describe, it, before, beforeEach } from "node:test"
import { acc, mu } from "../src/test.js"
import HB from "../src/hb.js"

const [{ jwk, addr }] = acc
import { randomBytes } from "node:crypto"

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
const URL = "http://localhost:10000"

describe("Hyperbeam", function () {
  after(() => setTimeout(() => process.exit(), 100))

  it("should get metrics", async () => {
    const hb = new HB()
    const met = await hb.metrics()
    assert.ok(met.process_uptime_seconds)
  })

  it("should get info", async () => {
    const hb = new HB()
    const info = await hb.info()
    assert.equal(info.port, 10000)
  })

  it.only("should deploy a process", async () => {
    const hb = await new HB().init(jwk)

    const res = await hb.get({ path: "~meta@1.0/info/address" })
    const address = res
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
    const d4 = await hb.dryrun({
      process,
      action: "Get",
    })
    assert.equal(d4.Messages[0].Data, "Count: 2")
    return
  })
})
