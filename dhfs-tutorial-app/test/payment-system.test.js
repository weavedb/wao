import assert from "assert"
import { describe, it, before, after } from "node:test"
import { HyperBEAM, acc } from "wao/test"
import { HB } from "wao"
import { rsaid, hmacid } from "hbsig"

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
    assert.deepEqual(info.faff_allow_list, [operator.addr, allowed_user.addr])

    // remove allowed_user
    await operator.hb.p("/~meta@1.0/info", { faff_allow_list: [operator.addr] })
    const info2 = await operator.hb.g("/~meta@1.0/info")
    assert.deepEqual(info2.faff_allow_list, [operator.addr])

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

describe("Payment System p4@1.0", function () {
  let hb, hbeam
  before(async () => {
    hbeam = await new HyperBEAM({
      reset: true,
      operator: HyperBEAM.OPERATOR,
    }).ready()
    hb = hbeam.hb
  })
  after(async () => hbeam.kill())

  it("should handle payment with lua", async () => {
    const process = hbeam.file("scripts/p4-payment-process.lua")
    const { pid: cache_pid } = await hb.spawn({})
    const { slot } = await hb.schedule({
      pid: cache_pid,
      data: process,
      "content-type": "application/lua",
    })
    const { body } = await hb.g("/~scheduler@1.0/schedule", {
      target: cache_pid,
      from: slot,
      accept: "application/aos-2",
    })
    const {
      edges: [msg],
    } = JSON.parse(body)
    const pid = msg.node.message.Id
    assert(pid)
    const client = hbeam.file("scripts/p4-payment-client.lua")

    const { slot: slot2 } = await hb.schedule({
      pid: cache_pid,
      data: client,
      "content-type": "application/lua",
    })
    const { body: body2 } = await hb.g("/~scheduler@1.0/schedule", {
      target: cache_pid,
      from: slot2,
      accept: "application/aos-2",
    })
    const {
      edges: [msg2],
    } = JSON.parse(body2)
    const cid = msg2.node.message.Id
    assert(cid)
  })

  it("should handle payment with lua", async () => {
    const process = hbeam.file("scripts/p4-payment-process.lua")
    const pid = await hb.cacheScript(process)
    const client = hbeam.file("scripts/p4-payment-client.lua")
    const cid = await hb.cacheScript(client)

    const hbeam2 = await new HyperBEAM({
      port: 10002,
      operator: hb.addr,
      p4_lua: { processor: pid, client: cid },
    }).ready()

    const operator = hbeam2
    const user = acc[0]
    user.hb = await new HB({ url: hbeam2.url }).init(user.jwk)
    const obj = { path: "credit-notice", quantity: 100, recipient: user.addr }
    const lua_msg = await operator.hb.sign(obj)
    const hmacId = hmacid(lua_msg.headers)
    const rsaId = rsaid(lua_msg.headers)
    const committed_lua_msg = {
      commitments: {
        [rsaId]: {
          alg: "rsa-pss-sha512",
          "commitment-device": "httpsig@1.0",
          committer: operator.addr,
          signature: lua_msg.headers.signature,
          "signature-input": lua_msg.headers["signature-input"],
        },
        [hmacId]: {
          alg: "hmac-sha256",
          "commitment-device": "httpsig@1.0",
          signature: lua_msg.headers.signature,
          "signature-input": lua_msg.headers["signature-input"],
        },
      },
      ...obj,
    }

    await operator.hb.p("/ledger~node-process@1.0/schedule", {
      body: committed_lua_msg,
    })
    const balance = await operator.hb.g(
      `/ledger~node-process@1.0/now/balance/${user.addr}`
    )
    assert.equal(balance, "100")
    const now = await operator.hb.g(`/ledger~node-process@1.0/now/balance`)
    assert.deepEqual(now, { [user.addr]: "100" })
    assert(await user.hb.p("/~message@1.0/set/hello", { hello: "world" }))
    const balance2 = await operator.hb.g(
      `/ledger~node-process@1.0/now/balance/${user.addr}`
    )
    assert.equal(balance2, "97")
    hbeam2.kill()
  })
})
