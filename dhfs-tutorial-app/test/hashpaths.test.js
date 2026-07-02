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
    // v0.9-FINAL linkifies large nested objects in JSON responses — the
    // body shape is `{ "msg1+link": ID, "msg2+link": ID, "opts+link": ID }`
    // instead of inline message objects. The link value IS the message ID
    // (a hash-path resolvable via /<id>), so we can verify the response is
    // well-formed by checking we have valid ID strings.
    const parsed = JSON.parse(out)
    const id1 = parsed["msg1+link"] ?? (parsed.msg1 && id(parsed.msg1))
    const id2 = parsed["msg2+link"] ?? (parsed.msg2 && id(parsed.msg2))
    console.log("msg1 id:", id1)
    console.log("msg2 id:", id2)
    assert.ok(id1, "msg1 should have a computable id")
    assert.ok(id2, "msg2 should have a computable id")
  })
})
