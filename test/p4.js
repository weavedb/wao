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
  let hb, hb2, hbeam, jwk, server, addr, store_prefix
  before(async () => {
    store_prefix = "cache-mainnet-" + Math.floor(Math.random() * 10000000)
    server = new Server({ port: 4000, log: true, hb_url: URL })
    jwk = getJWK("../../HyperBEAM/.wallet.json")
    addr = toAddr(jwk.n)
    hbeam = new HyperBEAM({
      store_prefix,
      c: "12",
      cmake: "3.5",
      gateway: 4000,
      legacy: true,
      operator: addr,
    })

    await wait(5000)
  })

  beforeEach(async () => {
    hb = await new HB({}).init(jwk)
    hb2 = await new HB({}).init(acc[0].jwk)
  })

  after(async () => hbeam.kill())

  it.only("should handle payment with lua", async () => {
    const process = readFileSync(
      resolve(
        import.meta.dirname,
        "../HyperBEAM/scripts/p4-payment-process.lua"
      )
    )
    const pid = await hb.cacheModule(process, "application/lua")
    const client = readFileSync(
      resolve(import.meta.dirname, "../HyperBEAM/scripts/p4-payment-client.lua")
    )
    const cid = await hb.cacheModule(client, "application/lua")
    const hbeam2 = new HyperBEAM({
      store_prefix,
      c: "12",
      cmake: "3.5",
      port: 10004,
      gateway: 4000,
      legacy: true,
      operator: addr,
      p4_lua: { processor: pid, client: cid },
    })
    await wait(5000)
    const hb3 = await new HB({ url: `http://localhost:10004` }).init(jwk)
    console.log(
      await fetch(`http://localhost:10004/~wao@1.0/balance`).then(r => r.json())
    )
    console.log(await hb3.spawn())
    return
  })
})
