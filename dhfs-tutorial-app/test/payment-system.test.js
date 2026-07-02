import assert from "assert"
import { describe, it, before, after } from "node:test"
import { readFileSync } from "fs"
import { resolve } from "path"
import { HyperBEAM, acc, toAddr } from "wao/test"
import { HB } from "wao"

describe("Payment System faff@1.0", function () {
  let hbeam, hb, operator
  let allowed_user = acc[0]
  let disallowed_user = acc[1]

  before(async () => {
    hbeam = await new HyperBEAM({
      reset: true,
      faff: [HyperBEAM.OPERATOR, allowed_user.addr],
    }).ready()
    operator = hbeam
    allowed_user.hb = new HB({ jwk: allowed_user.jwk })
    disallowed_user.hb = new HB({ jwk: disallowed_user.jwk })
  })
  after(async () => hbeam.kill())

  it("should test faff@1.0", async () => {
    const msg = ["/~message@1.0/set/hello", { hello: "world" }]

    // GET
    assert(await operator.hb.g(...msg))
    assert(await allowed_user.hb.g(...msg))
    assert(await disallowed_user.hb.g(...msg))

    // POST
    assert(await operator.hb.p(...msg))
    assert(await allowed_user.hb.p(...msg))
    await assert.rejects(disallowed_user.hb.p(...msg))

    const info = await operator.hb.g("/~meta@1.0/info")
    assert.deepEqual(info["faff-allow-list"], [operator.addr, allowed_user.addr])

    // remove allowed_user
    await operator.hb.p("/~meta@1.0/info", { "faff-allow-list": [operator.addr] })
    const info2 = await operator.hb.g("/~meta@1.0/info")
    assert.deepEqual(info2["faff-allow-list"], [operator.addr])

    // now previously allowed_user fails too
    await assert.rejects(allowed_user.hb.p(...msg))
  })
})

describe("Payment System simple-pay@1.0", function () {
  let hbeam, hb, operator
  let user = acc[0]
  before(async () => {
    hbeam = await new HyperBEAM({
      reset: true,
      operator: HyperBEAM.OPERATOR,
      simple_pay: true,
      simple_pay_price: 2,
    }).ready()
    operator = hbeam
    user.hb = await new HB({}).init(user.jwk)
  })

  after(async () => hbeam.kill())

  it("should test simple-pay@1.0", async () => {
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
    assert.equal(info1["simple-pay-price"], 2)

    // change simple-pay-price (v0.9-FINAL: binary key with hyphen)
    assert(await operator.hb.p("/~meta@1.0/info", { "simple-pay-price": 3 }))

    const info2 = await operator.hb.g("/~meta@1.0/info")
    assert.equal(info2["simple-pay-price"], 3)

    assert(await user.hb.p(...msg)) // cost = 3 * 3 = 9
    assert.equal(await user.hb.p(balance), "0")

    // this should fail for insufficient fund
    await assert.rejects(user.hb.p(...msg)) // cost = 3 * 3 = 9
  })
})

describe("Payment System p4@1.0", function () {
  let hbeam, user

  before(async () => {
    user = acc[0]
    const hbDir = resolve(import.meta.dirname, "../../HyperBEAM")
    // Read the operator wallet to get admin address
    const operatorJwk = JSON.parse(
      readFileSync(resolve(hbDir, ".wallet.json"), "utf8")
    )
    const operatorAddr = toAddr(operatorJwk.n)

    // hyper-token scripts: processor = [hyper-token.lua, hyper-token-p4.lua]
    const tokenScript = readFileSync(
      resolve(hbDir, "scripts/hyper-token.lua"),
      "utf8"
    )
    const p4Script = readFileSync(
      resolve(hbDir, "scripts/hyper-token-p4.lua"),
      "utf8"
    )
    // client = hyper-token-p4-client.lua (has balance + charge functions)
    const clientScript = readFileSync(
      resolve(hbDir, "scripts/hyper-token-p4-client.lua"),
      "utf8"
    )
    hbeam = await new HyperBEAM({
      reset: true,
      operator: HyperBEAM.OPERATOR,
      p4_lua: {
        processor: [
          { body: tokenScript, name: "hyper-token.lua" },
          { body: p4Script, name: "hyper-token-p4.lua" },
        ],
        client: { body: clientScript, name: "hyper-token-p4-client.lua" },
        admin: operatorAddr,
        balance: { [user.addr]: 1000 },
      },
    }).ready()
    user.hb = await new HB({ url: hbeam.url }).init(user.jwk)
  })
  after(async () => hbeam.kill())

  it("should handle p4@1.0 payment with lua", async () => {
    const operator = hbeam

    // user has pre-loaded balance of 1000
    const balance = await operator.hb.g(
      `/ledger~node-process@1.0/now/balance/${user.addr}`
    )
    assert.equal(Number(balance), 1000)

    // POST costs 3 (default pricing)
    assert(await user.hb.p("/~message@1.0/set/hello", { hello: "world" }))

    const balance2 = await operator.hb.g(
      `/ledger~node-process@1.0/now/balance/${user.addr}`
    )
    assert(Number(balance2) < 1000)
  })
})
