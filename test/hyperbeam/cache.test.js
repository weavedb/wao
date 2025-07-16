import assert from "assert"
import { after, describe, it, before, beforeEach } from "node:test"
import HyperBEAM from "../../src/hyperbeam.js"

describe("Hyperbeam Device", function () {
  let hb, hbeam
  before(async () => (hbeam = await new HyperBEAM({ reset: true }).ready()))
  beforeEach(async () => (hb = hbeam.hb))
  after(async () => hbeam.kill())

  it("should test cache@1.0", async () => {
    await hb.p("/~meta@1.0/info", { cache_writers: [hb.addr] })
    const { cache_writers } = await hb.g("/~meta@1.0/info")
    assert.deepEqual(cache_writers, [hb.addr])
    const bin = Buffer.from("abc")
    const { path } = await hb.p("/~cache@1.0/write", { body: bin })
    assert.equal(await hb.g("/~cache@1.0/read", { target: path }), "abc")
    await hb.p("/~cache@1.0/link", {
      source: path,
      destination: "new_location",
    })
    assert.equal(
      await hb.g("/~cache@1.0/read", { target: "new_location" }),
      "abc"
    )
  })
})
