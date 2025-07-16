import assert from "assert"
import { after, describe, it, before, beforeEach } from "node:test"
import HyperBEAM from "../../src/hyperbeam.js"

describe("Hyperbeam Device", function () {
  let hb, hbeam
  before(async () => (hbeam = await new HyperBEAM({ reset: true }).ready()))
  beforeEach(async () => (hb = hbeam.hb))
  after(async () => hbeam.kill())

  it("should test local-name@1.0", async () => {
    await hb.p("/~local-name@1.0/register", {
      value: "value",
      key: "key",
    })

    const val = await hb.g("/~local-name@1.0/lookup", { key: "key" })
    assert.equal(val, "value")
    const out = await hb.p("/~local-name@1.0/register", {
      value: { a: 123 },
      key: "map",
    })

    const val2 = await hb.g("/~local-name@1.0/lookup", { key: "map" })
    assert.deepEqual(val2, { a: 123 })

    const val3 = await hb.g("/~local-name@1.0/lookup", { key: "random" })
    assert.equal(val3, '"not_found"')
  })
})
