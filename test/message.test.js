import assert from "assert"
import { after, describe, it, before, beforeEach } from "node:test"
import { acc, toAddr } from "../src/test.js"
import { getJWK } from "./lib/test-utils.js"
import HB from "../src/hb.js"
import { pick } from "ramda"
import { wait } from "../src/utils.js"
import Server from "../src/server.js"
import HyperBEAM from "../src/hyperbeam.js"
import { extractPublicKeyFromHeaders } from "../src/signer.js"

const URL = "http://localhost:10001"

describe("Hyperbeam Device", function () {
  let hb, hb2, hbeam, jwk, server, addr, addr2
  before(async () => {
    server = new Server({ port: 6359, log: true, hb_url: URL })
    jwk = getJWK("../../HyperBEAM/.wallet.json")
    addr = toAddr(jwk.n)
    addr2 = toAddr(acc[0].jwk.n)
    hbeam = await new HyperBEAM({
      clearCache: true,
      gateway: 6359,
      c: "12",
      cmake: "3.5",
      operator: addr,
    }).ready()
  })

  beforeEach(async () => {
    hb = await new HB({}).init(jwk)
    hb2 = await new HB({}).init(acc[0].jwk)
  })

  after(async () => {
    hbeam.kill()
    server.end()
  })

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
