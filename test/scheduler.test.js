import assert from "assert"
import { after, describe, it, before, beforeEach } from "node:test"
import { acc, toAddr } from "../src/test.js"
import { getJWK } from "./lib/test-utils.js"
import HB from "../src/hb.js"
import { pick } from "ramda"
import { wait } from "../src/utils.js"
import Server from "../src/server.js"
import HyperBEAM from "../src/hyperbeam.js"
import { extractPublicKeyFromHeaders } from "../src/signer.js"
const seed = num => {
  const array = new Uint8Array(num)
  return crypto.getRandomValues(array).toString()
}

const URL = "http://localhost:10001"
const toJSON = res => JSON.parse(res.body)
describe("Hyperbeam Device", function () {
  let hb, hb2, hbeam, jwk, server, addr, addr2
  before(async () => {
    server = new Server({ port: 6359, log: true, hb_url: URL })
    jwk = getJWK("../../HyperBEAM/.wallet.json")
    addr = toAddr(jwk.n)
    addr2 = toAddr(acc[0].jwk.n)
    hbeam = new HyperBEAM({
      clearCache: true,
      gateway: 6359,
      c: "12",
      cmake: "3.5",
      operator: addr,
    })
    await wait(5000)
  })

  beforeEach(async () => {
    hb = await new HB({}).init(jwk)
    hb2 = await new HB({}).init(acc[0].jwk)
  })

  after(async () => hbeam.kill())

  it("should test scheduler@1.0", async () => {
    const { process: pid } = await hb.postJSON({
      path: "/~scheduler@1.0/schedule",
      scheduler: addr,
      "execution-device": "test-device@1.0",
    })
    const { processes } = await hb.getJSON({
      path: "/~scheduler@1.0/status",
    })
    assert.deepEqual(processes, [pid])

    const { slot } = await hb.postJSON({
      path: "/~scheduler@1.0/schedule",
      target: pid,
    })
    const { results: res } = await hb.getJSON({
      path: `/${pid}~process@1.0/compute`,
      slot,
    })
    assert.equal(res["assignment-slot"], 1)

    const res2 = await hb.postJSON({
      path: `/~scheduler@1.0/location`,
      address: addr,
      nonce: 0,
      url: "https://example.com",
    })
    assert.equal(res2.url, "https://example.com")

    const res3 = await hb.getJSON({
      path: `/~scheduler@1.0/location`,
      address: addr,
    })
    assert.equal(res3.body.url, "https://example.com")
  })
})
