import assert from "assert"
import { after, describe, it, before, beforeEach } from "node:test"
import { acc, toAddr } from "../src/test.js"
import { getJWK } from "./lib/test-utils.js"
import HB from "../src/hb.js"
import { pick } from "ramda"
import { wait } from "../src/utils.js"
import Server from "../src/server.js"
import HyperBEAM from "../src/hyperbeam.js"

const URL = "http://localhost:10001"

describe("Hyperbeam Device", function () {
  let hb, hb2, hbeam, jwk, server, addr, addr2
  before(async () => {
    server = new Server({ port: 6359, log: true, hb_url: URL })
    jwk = getJWK("../../HyperBEAM/.wallet.json")
    addr = toAddr(jwk.n)
    addr2 = toAddr(acc[0].jwk.n)
    hbeam = new HyperBEAM({
      clearCache: true,
      gateway: 6359,
      c: "12",
      cmake: "3.5",
      operator: addr,
    })
    await wait(5000)
  })

  beforeEach(async () => {
    hb = await new HB({}).init(jwk)
    hb2 = await new HB({}).init(acc[0].jwk)
  })

  after(async () => {
    hbeam.kill()
    server.end()
  })

  it("should test json@1.0", async () => {
    const obj = { key: 1, key2: "2", key3: [1, 2], key4: { a: 3 } }
    const res = await hb.post({
      path: "/~json@1.0/serialize",
      ...obj,
    })

    // json serialize is not to be used externally, it's applied to the msg before httpsig codec
    assert.deepEqual(JSON.parse(res.body), {
      device: "json@1.0",
      key: "1",
      key2: "2",
      key3: "1, 2",
      key4: "a=3",
      method: "POST",
    })

    const { headers: h } = await hb.post({
      path: "/~json@1.0/deserialize",
      body: JSON.stringify({ a: 1, b: [1, 2], c: { d: 4 } }),
    })
    assert.deepEqual(h.a, "1")
    assert.deepEqual(h.b, '"(ao-type-integer) 1", "(ao-type-integer) 2"')
    assert.deepEqual(h["body-keys"], '"c"')

    const { headers: h2 } = await hb.post({
      path: "/~json@1.0/deserialize",
      target: "json",
      json: JSON.stringify({ a: 1, b: [1, 2], c: { d: 4 } }),
    })

    assert.deepEqual(h2.a, "1")
    assert.deepEqual(h2.b, '"(ao-type-integer) 1", "(ao-type-integer) 2"')
    assert.deepEqual(h2["body-keys"], '"c"')
  })
})
