import assert from "assert"
import { after, describe, it, before, beforeEach } from "node:test"
import HyperBEAM from "../../src/hyperbeam.js"
import HB from "../../src/hb.js"

const data = `
local count = 0
Handlers.add("Inc", "Inc", function (msg)
  count = count + 1
  msg.reply({ Data = "Count: "..tostring(count) })
end)

Handlers.add("Get", "Get", function (msg)
  msg.reply({ Data = "Count: "..tostring(count) })
end)`

describe("Hyperbeam Device", function () {
  let hb, hbeam
  before(async () => {
    const _hbeam = new HyperBEAM({
      reset: true,
      bundler_ans104: false,
      genesis_wasm: true,
    })
    hbeam = await _hbeam.ready()
  })
  beforeEach(async () => (hb = hbeam.hb))
  after(async () => {
    hbeam.kill()
  })

  it("should test process #2 - genesis-wasm@1.0 with ans104 format", async () => {
    const hb2 = new HB({ jwk: hb.jwk, format: "ans104" })
    const { out } = await hb2.get({ path: "~meta@1.0/info" })
    console.log("Scheduler address:", out.address)
    const { pid } = await hb2.spawn({
      "execution-device": "genesis-wasm@1.0",
      "Data-Protocol": "ao",
      Variant: "ao.TN.1",
      Module: "ISShJH1ij-hPPt9St5UFFr_8Ys3Kj5cyg7zrMGt7H9s",
      Scheduler: out.address,
    })
    console.log("Spawned process:", pid)
    const { slot } = await hb2.scheduleLegacy({ pid, data })
    console.log("Eval slot:", slot)
    const { slot: slot2 } = await hb2.scheduleLegacy({ pid, action: "Inc" })
    console.log("Inc slot:", slot2)
    const res = await hb2.computeLegacy({ pid, slot: slot2 })
    console.log("compute result:", res)
    // Verify the Inc handler responded
    assert.ok(res.Messages && res.Messages.length > 0, "Should have messages")
    assert.equal(res.Messages[0].Data, "Count: 1")
  })
})
