import assert from "assert"
import { describe, it, before, after } from "node:test"
import { acc } from "wao/test"
import { HB } from "wao"

describe("HyperBEAM", function () {
  let hb

  // using one of the pre-generated accounts from acc for test
  before(async () => {
    hb = new HB({ jwk: acc[0].jwk, url: "http://localhost:10001" })
  })

  it.skip("should connect to a HyperBEAM node", async () => {
    // get build info
    const build = await hb.g("/~meta@1.0/build")
    assert.equal(build.node, "HyperBEAM")
  })
})
