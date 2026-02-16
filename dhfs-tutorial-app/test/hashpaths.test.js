import assert from "assert"
import { describe, it, before, after } from "node:test"
import { HyperBEAM } from "wao/test"
import { id } from "hbsig"

describe("Hashpaths", function () {
  let hbeam, hb
  before(async () => {
    hbeam = await new HyperBEAM({ reset: true }).ready()
    hb = hbeam.hb
  })
  after(async () => hbeam.kill())

  it("should resolve", async () => {
    await hb.p("/~mydev@1.0/resolve")
  })

  it("should resolve #2", async () => {
    await hb.p("/~mydev@1.0/resolve2")
  })

  it("should resolve #3", async () => {
    const out = await hb.p("/~mydev@1.0/resolve3")
    console.log("resolve3 result:", out)
    // Verify the resolve chain produced the correct final result (0+1+2+3=6)
    assert.equal(out.num, 6)
  })

  it("should extract hashpath", async () => {
    const { out } = await hb.post({ path: "/~mydev@1.0/forward" })
    const { msg1, msg2 } = JSON.parse(out)
    // Verify ids can be computed from the message parts
    const id1 = id(msg1)
    const id2 = id(msg2)
    console.log("msg1 id:", id1)
    console.log("msg2 id:", id2)
    assert.ok(id1, "msg1 should have a computable id")
    assert.ok(id2, "msg2 should have a computable id")
  })
})
