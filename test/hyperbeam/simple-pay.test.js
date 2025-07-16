import assert from "assert"
import { after, describe, it, before, beforeEach } from "node:test"
import { acc } from "../../src/test.js"
import HB from "../../src/hb.js"
import HyperBEAM from "../../src/hyperbeam.js"
let operator = null
let user = acc[0]

describe("Hyperbeam Legacynet", function () {
  let hbeam
  before(async () => {
    hbeam = await new HyperBEAM({
      operator: HyperBEAM.OPERATOR,
      simple_pay: true,
      simple_pay_price: 2,
      reset: true,
    }).ready()
  })

  beforeEach(async () => {
    operator = hbeam
    user.hb = await new HB({}).init(user.jwk)
  })
  after(async () => hbeam.kill())

  it("should test simple pay", async () => {
    // cost = simplePayPrice * 3
    const msg = ["/~message@1.0/set/hello", { hello: "world" }]

    // balance is non_chargable
    const balance = "/~simple-pay@1.0/balance"

    // topup user
    await operator.hb.p("/~simple-pay@1.0/topup", {
      amount: 15,
      recipient: user.addr,
    })
    assert.equal(await user.hb.p(balance), "15")
    assert(await user.hb.p(...msg)) // cost = 2 * 3 = 6
    assert.equal(await user.hb.p(balance), "9")

    const info1 = await operator.hb.g("/~meta@1.0/info")
    assert.equal(info1.simple_pay_price, 2)

    // change simple_pay_price
    assert(await operator.hb.p("/~meta@1.0/info", { simple_pay_price: 3 }))

    const info2 = await operator.hb.g("/~meta@1.0/info")
    assert.equal(info2.simple_pay_price, 3)

    assert(await user.hb.p(...msg)) // cost = 3 * 3 = 9
    assert.equal(await user.hb.p(balance), "0")

    // this should fail for insufficient fund
    await assert.rejects(user.hb.p(...msg)) // cost = 3 * 3 = 9
  })
})
