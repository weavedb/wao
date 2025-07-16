import assert from "assert"
import { after, describe, it, before, beforeEach } from "node:test"
import { acc, toAddr } from "../src/test.js"
import { getJWK } from "./lib/test-utils.js"
import HB from "../src/hb.js"
import HyperBEAM from "../src/hyperbeam.js"

describe("Hyperbeam Device", function () {
  let hb, hbeam, jwk, addr
  before(async () => {
    jwk = getJWK("../../HyperBEAM/.wallet.json")
    addr = toAddr(jwk.n)
    hbeam = await new HyperBEAM({ clearCache: true }).ready()
  })

  beforeEach(async () => (hb = await new HB({}).init(jwk)))

  after(async () => hbeam.kill())

  it("should test lookup@1.0", async () => {
    await hb.post({ path: "/~meta@1.0/info", cache_writers: [addr] })
    const { cache_writers } = await hb.getJSON({ path: "/~meta@1.0/info" })
    assert.deepEqual(cache_writers, [addr])
    const { path } = await hb.postJSON({
      path: "/~cache@1.0/write",
      body: "abc",
    })
    const { out: val } = await hb.get({
      path: "/~lookup@1.0/read",
      target: path,
    })
    assert.equal(val, "abc")
  })
})
