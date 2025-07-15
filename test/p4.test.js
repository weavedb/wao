import { rsaid, hmacid } from "../src/utils.js"

import assert from "assert"
import { after, describe, it, before, beforeEach } from "node:test"
import { acc, mu, AO, toAddr } from "../src/test.js"
import { getJWK } from "./lib/test-utils.js"
import HB from "../src/hb.js"
import { isNotNil, filter, isNil } from "ramda"
import HyperBEAM from "../src/hyperbeam.js"
import { readFileSync } from "fs"
import { resolve } from "path"

const URL = "http://localhost:10001"

import crypto from "crypto"

describe("Hyperbeam Legacynet", function () {
  let hb, hbeam, jwk, server, addr
  before(async () => {
    jwk = getJWK("../../HyperBEAM/.wallet.json")
    addr = toAddr(jwk.n)
    hbeam = await new HyperBEAM({
      clearCache: true,
      c: "12",
      cmake: "3.5",
      operator: addr,
    }).ready()
  })

  beforeEach(async () => (hb = await new HB({}).init(jwk)))

  after(async () => hbeam.kill())
  it("should handle payment with lua", async () => {
    const port = 10002
    const addr2 = toAddr(acc[0].jwk.n)
    const process = readFileSync(
      resolve(
        import.meta.dirname,
        "../HyperBEAM/scripts/p4-payment-process.lua"
      )
    )
    const { pid: cache_pid } = await hb.spawn({})
    const { slot } = await hb.schedule({
      pid: cache_pid,
      data: process,
      "content-type": "application/lua",
    })
    const {
      out: { body },
    } = await hb.get({
      path: "/~scheduler@1.0/schedule",
      target: cache_pid,
      from: slot,
      accept: "application/aos-2",
    })
    const {
      edges: [msg],
    } = JSON.parse(body)
    const pid = msg.node.message.Id
    console.log(pid)
    const client = readFileSync(
      resolve(import.meta.dirname, "../HyperBEAM/scripts/p4-payment-client.lua")
    )
    const { slot: slot2 } = await hb.schedule({
      pid: cache_pid,
      data: client,
      "content-type": "application/lua",
    })
    const {
      out: { body: body2 },
    } = await hb.get({
      path: "/~scheduler@1.0/schedule",
      target: cache_pid,
      from: slot2,
      accept: "application/aos-2",
    })
    const {
      edges: [msg2],
    } = JSON.parse(body2)
    const cid = msg2.node.message.Id
    console.log(cid)
    return
  })
  it.only("should handle payment with lua", async () => {
    const port = 10002
    const addr2 = toAddr(acc[0].jwk.n)
    const process = readFileSync(
      resolve(
        import.meta.dirname,
        "../HyperBEAM/scripts/p4-payment-process.lua"
      )
    )
    const pid = await hb.cacheScript(process)
    const client = readFileSync(
      resolve(import.meta.dirname, "../HyperBEAM/scripts/p4-payment-client.lua")
    )
    const cid = await hb.cacheScript(client)

    const hbeam2 = await new HyperBEAM({
      c: "12",
      cmake: "3.5",
      port,
      operator: addr,
      p4_lua: { processor: pid, client: cid },
    }).ready()

    const hb3 = await new HB({ url: `http://localhost:${port}` }).init(jwk)
    const hb4 = await new HB({ url: `http://localhost:${port}` }).init(
      acc[0].jwk
    )
    const obj = {
      path: "credit-notice",
      quantity: 100,
      recipient: addr2,
    }
    const lua_msg = await hb3.commit(obj, { path: true })
    await hb3.post({
      path: "/ledger~node-process@1.0/schedule",
      body: lua_msg,
    })
    const { out: balance } = await hb3.get({
      path: `/ledger~node-process@1.0/now/balance/${addr2}`,
    })
    assert.equal(balance, "100")
    const now = (
      await hb3.get({
        path: `/ledger~node-process@1.0/now/balance`,
      })
    ).body

    const hello = { path: "/~message@1.0/set/hello", hello: "world" }
    assert(await hb4.post(hello))

    const { out: balance2 } = await hb3.get({
      path: `/ledger~node-process@1.0/now/balance/${addr2}`,
    })
    assert.equal(balance2, "97")
    hbeam2.kill()
  })
})
