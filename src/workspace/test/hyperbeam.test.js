import assert from "assert"
import { describe, it, before, after, beforeEach } from "node:test"
import { HyperBEAM } from "wao/test"

/*
  The link to your HyperBEAM node directory.
  It's relative to your app root folder, not the test folder.
*/
const cwd = "../HyperBEAM"

describe("HyperBEAM", function () {
  let hbeam, hb

  // start a hyperbeam node and wait till it's ready
  before(async () => (hbeam = await new HyperBEAM({ cwd }).ready()))

  beforeEach(async () => (hb = hbeam.hb))

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
