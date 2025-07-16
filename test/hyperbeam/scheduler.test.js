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
      scheduler: hb.addr,
      "execution-device": "test-device@1.0",
    })
    const { processes } = await hb.g("/~scheduler@1.0/status")
    assert.deepEqual(processes, [pid])
    const { slot } = await hb.p("/~scheduler@1.0/schedule", { target: pid })

    // todo: get doesn't work
    const { results } = await hb.getJSON({
      path: `/${pid}~process@1.0/compute`,
      slot,
    })
    assert.equal(results["assignment-slot"], 1)

    const res2 = await hb.p(`/~scheduler@1.0/location`, {
      address: hb.addr,
      nonce: 0,
      url: "https://example.com",
    })
    assert.equal(res2.url, "https://example.com")

    // todo: get doesn't work
    const res3 = await hb.getJSON({
      path: `/~scheduler@1.0/location`,
      address: hb.addr,
    })
    assert.equal(res3.body.url, "https://example.com")
    const res = await hb.get({
      path: `/~scheduler@1.0/location/~json@1.0/serialize`,
      address: hb.addr,
    })
  })
})
