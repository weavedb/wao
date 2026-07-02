import assert from "assert"
import { after, describe, it, before, beforeEach } from "node:test"
import { acc } from "../../src/test.js"
import HB from "../../src/hb.js"
import HyperBEAM from "../../src/hyperbeam.js"

const testJwk = acc[0].jwk

describe("WAO Device Tests", function () {
  let hb, hbeam
  before(async () => {
    hbeam = await new HyperBEAM({ reset: true, genesis_wasm: true, linkify_mode: false }).ready()
  })
  beforeEach(async () => (hb = await new HB({ url: hbeam.url }).init(testJwk)))
  after(async () => {
    hbeam.kill()
  })

  it("should test add@1.0", async () => {
    const res = await hb.post({ path: "/~add@1.0/add", a: 2, b: 3 })
    assert.equal(res.headers.sum, "5")
  })

  it("should test mul@1.0", async () => {
    const res = await hb.post({ path: "/~mul@1.0/mul", a: 2, b: 3 })
    assert.equal(res.headers.product, "6")
  })

  it("should upload module #2", async () => {
    const { pid } = await hb.spawn({ "execution-device": "wao@1.0" })
    await hb.schedule({ pid })
    await hb.schedule({ pid })
    await hb.schedule({ pid })
    await hb.schedule({ pid })
    assert.equal((await hb.now({ pid })).count, 5)
    assert.equal((await hb.now({ pid })).count, 5)
  })
})
