import assert from "assert"
import { after, describe, it, before, beforeEach } from "node:test"
import { acc, toAddr } from "../src/test.js"
import HB from "../src/hb.js"
import HyperBEAM from "../src/hyperbeam.js"
import { readFileSync } from "fs"
import { resolve } from "path"

const cwd = "./HyperBEAM"
const wallet = resolve(process.cwd(), cwd, ".wallet.json")
const jwk = JSON.parse(readFileSync(wallet, "utf8"))

let operator = { jwk, addr: toAddr(jwk.n) }
let user = acc[0]

describe("Hyperbeam Legacynet", function () {
  let hbeam
  before(async () => {
    hbeam = await new HyperBEAM({
      operator: operator.addr,
      simple_pay: true,
      simple_pay_price: 2,
    }).ready()
  })

  beforeEach(async () => {
    operator.hb = await new HB({}).init(operator.jwk)
    user.hb = await new HB({}).init(user.jwk)
  })

  after(async () => {
    hbeam.kill()
  })

  it("should test simple pay", async () => {
    // cost = simplePayPrice * 3
    const msg = { path: "/~message@1.0/set/hello", hello: "world" }

    // balance is non_chargable
    const balance = { path: "/~simple-pay@1.0/balance" }

    // info is non_chargable
    const info = { path: "/~meta@1.0/info" }

    // topup user
    await operator.hb.post({
      path: "/~simple-pay@1.0/topup",
      amount: 15,
      recipient: user.addr,
    })
    assert.equal((await user.hb.post(balance)).out, "15")
    assert(await user.hb.post(msg)) // cost = 2 * 3 = 6
    assert.equal((await user.hb.post(balance)).out, "9")
    return
    const { out: info1 } = await operator.hb.get(info)
    assert.equal(info1.simple_pay_price, 2)

    // change simple_pay_price
    assert(await operator.hb.post({ ...info, simple_pay_price: 3 }))

    const { out: info2 } = await operator.hb.get(info)
    assert.equal(info2.simple_pay_price, 3)

    assert(await user.hb.post(msg)) // cost = 3 * 3 = 9
    assert.equal((await user.hb.post(balance)).out, "0")

    // this should fail for insufficient fund
    await assert.rejects(user.hb.post(msg)) // cost = 3 * 3 = 9
  })
})
