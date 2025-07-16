import assert from "assert"
import { after, describe, it, before, beforeEach } from "node:test"
import { pick } from "ramda"
import HyperBEAM from "../../src/hyperbeam.js"

describe("Hyperbeam Device", function () {
  let hb, hbeam
  before(async () => (hbeam = await new HyperBEAM({ reset: true }).ready()))
  beforeEach(async () => (hb = hbeam.hb))

  after(async () => hbeam.kill())

  it("should test meta@1.0", async () => {
    const info = await hb.g("/~meta@1.0/info")
    assert.deepEqual(pick(["initialized", "port", "address"])(info), {
      port: 10001,
      address: hb.addr,
      initialized: true,
    })
    await hb.p("/~meta@1.0/info", {
      test_config: 123,
      initialized: "permanent",
    })
    const info2 = await hb.g("/~meta@1.0/info")
    assert.deepEqual(info2.test_config, 123)
    await assert.rejects(hb.post({ path: "/~meta@1.0/info", test_config: 124 }))
    const info3 = await hb.g("/~meta@1.0/info")
    assert.deepEqual(info3.test_config, 123)

    const build = await hb.g("/~meta@1.0/build")
    assert.equal(build.node, "HyperBEAM")
    assert.equal((await hb.g("/~meta@1.0/info")).port, 10001)
  })
})
