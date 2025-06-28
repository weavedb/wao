import assert from "assert"
import { send as _send } from "../src/signer.js"
import { after, describe, it, before, beforeEach } from "node:test"
import { acc, mu, AO, toAddr } from "../src/test.js"
import { getJWK } from "./lib/test-utils.js"
import HB from "../src/hb.js"
import { isNotNil, filter, isNil } from "ramda"
import { randomBytes } from "node:crypto"
import { wait } from "../src/utils.js"
import Server from "../src/server.js"
import HyperBEAM from "../src/hyperbeam.js"
import { readFileSync } from "fs"
import { resolve } from "path"

const URL = "http://localhost:10001"

describe("Hyperbeam Legacynet", function () {
  let hb, hb2, hbeam, jwk, server, addr, store_prefix, addr2
  before(async () => {
    store_prefix = "cache-mainnet-" + Math.floor(Math.random() * 10000000)
    server = new Server({ port: 4000, log: true, hb_url: URL })
    jwk = getJWK("../../HyperBEAM/.wallet.json")
    addr = toAddr(jwk.n)
    addr2 = toAddr(acc[0].jwk.n)
    hbeam = await new HyperBEAM({
      store_prefix,
      c: "12",
      cmake: "3.5",
      operator: addr,
      simplePay: true,
      simplePayPrice: 2,
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

  it("should test simple pay", async () => {
    await hb.send({
      path: "/~simple-pay@1.0/topup",
      amount: 10,
      recipient: addr2,
    })
    const { pid } = await hb2.spawn()
    const res = await hb2.send({ path: "/~simple-pay@1.0/balance" })
    assert.equal(res.body, "6")
  })
})
