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
    const pid = await hb.cacheBinary(process, "application/lua")
    const client = readFileSync(
      resolve(import.meta.dirname, "../HyperBEAM/scripts/p4-payment-client.lua")
    )
    const cid = await hb.cacheBinary(client, "application/lua")
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
      path: "/credit-notice",
      quantity: 100,
      recipient: addr2,
    }
    const meta = { alg: "rsa-pss-sha512", "commitment-device": "httpsig@1.0" }
    const meta2 = { alg: "hmac-sha256", "commitment-device": "httpsig@1.0" }

    const msg = await hb3.sign(obj, { path: true })
    const hmacMessage = msg.headers

    const hmacId = hmacid(hmacMessage)
    const rsaId = rsaid(msg.headers)
    const committer = addr
    const sigs = {
      signature: msg.headers.signature,
      "signature-input": msg.headers["signature-input"],
    }
    const res4 = {
      commitments: {
        [rsaId]: { ...meta, committer: addr, ...sigs },
        [hmacId]: { ...meta2, ...sigs },
      },
      ...obj,
    }
    const { slot } = await hb3.postJSON({
      path: "/ledger~node-process@1.0/schedule",
      body: res4,
    })
    const msg4 = await hb3.postJSON({
      path: "/ledger~node-process@1.0/compute",
      params: { slot },
    })
    const balance = (
      await hb3.get({
        path: `/ledger~node-process@1.0/now/balance/${addr2}`,
      })
    ).body
    assert.equal(balance, "100")
    const now = (
      await hb3.get({
        path: `/ledger~node-process@1.0/now/balance`,
      })
    ).body
    hbeam2.kill()
  })
})
