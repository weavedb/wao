import assert from "assert"
import { after, describe, it, before, beforeEach } from "node:test"
import { acc, toAddr } from "../../src/test.js"
import HB from "../../src/hb.js"
import HyperBEAM from "../../src/hyperbeam.js"

describe("Hyperbeam", function () {
  let hb, hbeam
  before(async () => {
    hbeam = await new HyperBEAM({
      reset: true,
      operator: HyperBEAM.OPERATOR,
    }).ready()
  })
  beforeEach(async () => (hb = hbeam.hb))
  after(async () => hbeam.kill())

  it("should handle payment with lua", async () => {
    const port = 10002
    const addr2 = toAddr(acc[0].jwk.n)
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
    const port = 10002
    const addr2 = toAddr(acc[0].jwk.n)
    const process = hbeam.file("scripts/p4-payment-process.lua")
    const pid = await hb.cacheScript(process)
    const client = hbeam.file("scripts/p4-payment-client.lua")
    const cid = await hb.cacheScript(client)

    const hbeam2 = await new HyperBEAM({
      port,
      operator: hb.addr,
      p4_lua: { processor: pid, client: cid },
    }).ready()

    const hb3 = await new HB({ url: `http://localhost:${port}` }).init(hb.jwk)
    const hb4 = await new HB({ url: `http://localhost:${port}` }).init(
      acc[0].jwk
    )
    const lua_msg = await hb3.commit({
      path: "credit-notice",
      quantity: 100,
      recipient: addr2,
    })
    await hb3.p("/ledger~node-process@1.0/schedule", { body: lua_msg })
    const balance = await hb3.g(`/ledger~node-process@1.0/now/balance/${addr2}`)
    assert.equal(balance, "100")
    const now = await hb3.g(`/ledger~node-process@1.0/now/balance`)
    assert.deepEqual(now, { [addr2]: "100" })
    assert(await hb4.p("/~message@1.0/set/hello", { hello: "world" }))
    const balance2 = await hb3.g(
      `/ledger~node-process@1.0/now/balance/${addr2}`
    )
    assert.equal(balance2, "97")
    hbeam2.kill()
  })
})
