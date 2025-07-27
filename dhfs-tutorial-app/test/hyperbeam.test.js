import assert from "assert"
import { describe, it, before, after } from "node:test"
import { HyperBEAM } from "wao/test"

describe("HyperBEAM", function () {
  let hbeam, hb

  // start a hyperbeam node and wait till it's ready, reset storage for test
  before(async () => {
    hbeam = await new HyperBEAM({ reset: true }).ready()
    hb = hbeam.hb
  })

  // kill the node after testing
  after(async () => hbeam.kill())

  it("should run a HyperBEAM node", async () => {
    // change config
    await hb.post({ path: "/~meta@1.0/info", test_config: "abc" })

    // get config
    const { out } = await hb.get({ path: "/~meta@1.0/info" })
    assert.equal(out.test_config, "abc")
  })
})
