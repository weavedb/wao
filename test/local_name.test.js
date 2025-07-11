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

  beforeEach(async () => (hb = await new HB({}).init(jwk)))

  after(async () => hbeam.kill())

  it("should test local-name@1.0", async () => {
    await hb.post({
      path: "/~local-name@1.0/register",
      value: "value",
      key: "key",
    })

    const { out: val } = await hb.get({
      path: "/~local-name@1.0/lookup",
      key: "key",
    })
    assert.equal(val, "value")
    const { out } = await hb.post({
      path: "/~local-name@1.0/register",
      value: { a: 123 },
      key: "map",
    })

    const { out: val2 } = await hb.get({
      path: "/~local-name@1.0/lookup",
      key: "map",
    })
    assert.deepEqual(val2, { a: 123 })

    const { out: val3 } = await hb.get({
      path: "/~local-name@1.0/lookup",
      key: "random",
    })
    assert.equal(val3, '"not_found"')
  })
})
