import assert from "assert"
import { after, describe, it, before, beforeEach } from "node:test"
import { acc, mu, AO, toAddr } from "../src/test.js"
import { getJWK } from "./lib/test-utils.js"
import HB from "../src/hb.js"
import HyperBEAM from "../src/hyperbeam.js"

const URL = "http://localhost:10001"

describe("Hyperbeam Legacynet", function () {
  let hb, hb2, hbeam, jwk, addr, addr2
  before(async () => {
    jwk = getJWK("../../HyperBEAM/.wallet.json")
    addr = toAddr(jwk.n)
    addr2 = toAddr(acc[0].jwk.n)
    hbeam = await new HyperBEAM({
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
  })

  it("should test simple pay", async () => {
    await hb.post({
      path: "/~simple-pay@1.0/topup",
      amount: 10,
      recipient: addr2,
    })
    const { pid } = await hb2.spawn()
    const res = await hb2.post({ path: "/~simple-pay@1.0/balance" })
    assert.equal(res.body, "6")
  })
})
