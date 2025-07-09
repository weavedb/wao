import assert from "assert"
import { after, describe, it, before, beforeEach } from "node:test"
import { acc, toAddr } from "../src/test.js"
import { getJWK } from "./lib/test-utils.js"
import HB from "../src/hb.js"
import HyperBEAM from "../src/hyperbeam.js"
import { extractPublicKeyFromHeaders } from "../src/signer-utils.js"

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

  after(async () => hbeam.kill())

  it("should test message@1.0", async () => {
    assert.equal(
      (await hb.get({ path: `/~message@1.0&hello=world/hello` })).body,
      "world"
    )
    assert.equal(
      (await hb.get({ path: `/~message@1.0/set`, hello: "world" })).headers
        .hello,
      "world"
    )
    assert.equal(
      (await hb.post({ path: `/~message@1.0/set`, hello: "world" })).headers
        .hello,
      "world"
    )
    assert.equal(
      (await hb.post({ path: `/~message@1.0/set/hello`, hello: "world" })).body,
      "world"
    )

    const { headers } = await hb.get({
      path: `/~message@1.0/commit`,
      hello: "world",
    })

    const signerAddr = toAddr(
      extractPublicKeyFromHeaders(headers).toString("base64")
    )
    assert.equal(addr, signerAddr)
  })
})
