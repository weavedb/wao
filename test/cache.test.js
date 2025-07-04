import aos_wamr from "../src/lua/aos_wamr.js"

import assert from "assert"
import { after, describe, it, before, beforeEach } from "node:test"
import { acc, toAddr } from "../src/test.js"
import { getJWK } from "./lib/test-utils.js"
import HB from "../src/hb.js"
import HyperBEAM from "../src/hyperbeam.js"

const URL = "http://localhost:10001"

describe("Hyperbeam Device", function () {
  let hb, hbeam, jwk, addr
  before(async () => {
    jwk = getJWK("../../HyperBEAM/.wallet.json")
    addr = toAddr(jwk.n)
    hbeam = await new HyperBEAM({
      clearCache: true,
      c: "12",
      cmake: "3.5",
    }).ready()
  })

  beforeEach(async () => {
    hb = await new HB({}).init(jwk)
  })

  after(async () => {
    hbeam.kill()
  })

  it("should test cache@1.0", async () => {
    await hb.post({ path: "/~meta@1.0/info", cache_writers: [addr] })
    const { cache_writers } = await hb.getJSON({ path: "/~meta@1.0/info" })
    assert.deepEqual(cache_writers, [addr])
    const bin = Buffer.from("abc")
    const { path } = await hb.postJSON({
      path: "/~cache@1.0/write",
      body: bin,
    })
    assert.equal(
      (await hb.get({ path: "/~cache@1.0/read", target: path })).body,
      "abc"
    )
    await hb.postJSON({
      path: "/~cache@1.0/link",
      source: path,
      destination: "new_location",
    })
    assert.equal(
      (await hb.get({ path: "/~cache@1.0/read", target: "new_location" })).body,
      "abc"
    )
  })
})
