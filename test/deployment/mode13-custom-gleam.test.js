import { describe, it, before, after } from "node:test"
import assert from "node:assert"
import { HyperBEAM } from "../../src/test.js"
import HB from "../../src/hb.js"

describe("Mode 13: Custom Gleam Device on Local HB", function () {
  let hbeam, hb

  before(async () => {
    hbeam = await new HyperBEAM({ reset: true }).ready()
    hb = new HB({ url: hbeam.url })
    await hb.init(hbeam.jwk)
  })

  after(async () => {
    if (hbeam) hbeam.kill()
  })

  it("query info: version 1.0", async () => {
    const res = await hb.g("/~gleam-counter@1.0/info")
    console.log("  info:", JSON.stringify(res))
    assert.equal(res.version, "1.0")
  })

  it("inc: num 0 → 1", async () => {
    const res = await hb.post({ path: "/~gleam-counter@1.0/inc", num: 0 })
    console.log("  inc result:", JSON.stringify(res.out).slice(0, 200))
    assert.equal(res.out.num, "1")
  })

  it("add: num 0 + plus 5 → 5", async () => {
    const res = await hb.post({ path: "/~gleam-counter@1.0/add", num: 0, plus: 5 })
    console.log("  add result:", JSON.stringify(res.out).slice(0, 200))
    assert.equal(res.out.num, "5")
  })

  it("chain: inc then add", async () => {
    const r1 = await hb.post({ path: "/~gleam-counter@1.0/inc", num: 0 })
    assert.equal(r1.out.num, "1")

    const r2 = await hb.post({
      path: "/~gleam-counter@1.0/add",
      num: parseInt(r1.out.num),
      plus: 10,
    })
    console.log("  chain result:", JSON.stringify(r2.out).slice(0, 200))
    assert.equal(r2.out.num, "11")
  })
})
