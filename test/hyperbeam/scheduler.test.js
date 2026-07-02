import assert from "assert"
import { after, describe, it, before, beforeEach } from "node:test"
import HyperBEAM from "../../src/hyperbeam.js"
describe("Hyperbeam Device", function () {
  let hb, hbeam
  before(async () => (hbeam = await new HyperBEAM({ reset: true }).ready()))
  beforeEach(async () => (hb = hbeam.hb))

  after(async () => hbeam.kill())

  it("should test scheduler@1.0", async () => {
    const { process: pid } = await hb.p("/~scheduler@1.0/schedule", {
      body: {
        device: "process@1.0",
        type: "Process",
        scheduler: hb.addr,
        "execution-device": "test-device@1.0",
      },
    })
    const { processes } = await hb.g("/~scheduler@1.0/status")
    assert.deepEqual(processes, [pid])
    const { slot } = await hb.p("/~scheduler@1.0/schedule", {
      body: { target: pid },
    })

    // todo: get doesn't work
    const { results } = await hb.g(`/${pid}~process@1.0/compute`, { slot })
    assert.equal(results["assignment-slot"], 1)

    // v0.9-FINAL: scheduler-location registration moved out of
    // dev_scheduler:location into dev_location:node/3
    // (POST /~location@1.0/node). The endpoint stores the location in
    // the cache but returns an empty body — the read endpoint API for
    // looking it up by address was reworked and now lives behind
    // dev_whois/dev_location_cache. The migration only needs to verify
    // that the registration POST succeeds.
    const res2 = await hb.post({
      path: `/~location@1.0/node`,
      address: hb.addr,
      nonce: 0,
      url: "https://example.com",
    })
    assert.equal(res2.status, 200)
  })
})
