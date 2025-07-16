import assert from "assert"
import { describe, it, before, after, beforeEach } from "node:test"
import { HyperBEAM } from "wao/test"
const cwd = "../../HyperBEAM"

describe("HyperBEAM", function () {
  let hbeam, hb
  before(
    async () => (hbeam = await new HyperBEAM({ cwd, reset: true }).ready())
  )
  beforeEach(async () => (hb = hbeam.hb))
  after(async () => hbeam.kill())

  it("should run a HyperBEAM node", async () => {
    const build = await hb.g("/~meta@1.0/build")
    assert.equal(build.node, "HyperBEAM")
  })
})
