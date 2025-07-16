import assert from "assert"
import { after, describe, it, before, beforeEach } from "node:test"
import HyperBEAM from "../../src/hyperbeam.js"
import { extractPublicKeyFromHeaders } from "../../src/signer-utils.js"
import { toAddr } from "../../src/test.js"

describe("Hyperbeam Device", function () {
  let hb, hbeam
  before(async () => (hbeam = await new HyperBEAM({ reset: true }).ready()))
  beforeEach(async () => (hb = hbeam.hb))
  after(async () => hbeam.kill())

  it("should test message@1.0", async () => {
    assert.equal(await hb.g(`/~message@1.0&hello=world/hello`), "world")
    assert.equal(
      (await hb.g(`/~message@1.0/set`, { hello: "world" })).hello,
      "world"
    )
    assert.equal(
      (await hb.p(`/~message@1.0/set`, { hello: "world" })).hello,
      "world"
    )
    assert.equal(
      await hb.p(`/~message@1.0/set/hello`, { hello: "world" }),
      "world"
    )

    const { headers, out } = await hb.get({
      path: `/~message@1.0/commit`,
      hello: "world",
    })
    const signerAddr = toAddr(
      extractPublicKeyFromHeaders(headers).toString("base64")
    )
    assert.equal(hb.addr, signerAddr)
  })
})
