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

  it("should test process@1.0", async () => {
    const { process: pid } = await hb.postJSON({
      path: "/~process@1.0/schedule",
      scheduler: addr,
      "execution-device": "test-device@1.0",
    })
    const { slot } = await hb.postJSON({ path: `/${pid}~process@1.0/schedule` })
    // bugs with compute and now
    /*
    const { slot: slot2 } = await hb.postJSON({
    path: `/${pid}~process@1.0/schedule`,
    })*/
    assert.equal(slot, 1)
    const { assignments } = await hb.getJSON({
      path: `/${pid}~process@1.0/schedule`,
    })
    assert.equal(assignments["0"].process, pid)
    assert.equal(assignments["1"].process, pid)

    const { results: res } = await hb.getJSON({
      path: `/${pid}~process@1.0/compute`,
      slot: 1,
    })
    assert.equal(res["assignment-slot"], 1)
    const { results: res3 } = await hb.getJSON({
      path: `/${pid}~process@1.0/now`,
    })
    assert.equal(res3["assignment-slot"], 1)
    const { current } = await hb.getJSON({
      path: `/${pid}~process@1.0/slot`,
    })
    assert.equal(current, slot)

    const { results: res4 } = await hb.getJSON({
      path: `/${pid}~process@1.0/now`,
    })
    assert.equal(res4["assignment-slot"], 1)
    const { method } = await hb.getJSON({
      path: `/${pid}~process@1.0/snapshot`,
    })
    assert.equal(method, "GET")
  })
})
