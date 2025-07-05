import assert from "assert"
import { after, describe, it, before, beforeEach } from "node:test"
import { acc, mu, AO, toAddr } from "../src/test.js"
import { getJWK, mod } from "./lib/test-utils.js"
import HB from "../src/hb.js"
import HyperBEAM from "../src/hyperbeam.js"
import keys from "./cases.js"
import { generateTestCases } from "./gen.js"
describe("Hyperbeam Signer", function () {
  let hb, hb2, hbeam, jwk, addr, store_prefix
  before(async () => {
    jwk = getJWK("../../HyperBEAM/.wallet.json")
    addr = toAddr(jwk.n)
    hbeam = await new HyperBEAM({ as: [], c: "12", cmake: "3.5" }).ready()
  })
  beforeEach(async () => (hb = await new HB({}).init(jwk)))
  after(async () => hbeam.kill())

  it.only("should test signer", async () => {
    //let cases = generateTestCases(1000)
    let cases = keys
    let err = []
    for (const v of cases) {
      const msg = await hb.sign({ path: "/~wao@1.0/httpsig", ...v })
      console.log(msg)
      try {
        const { body } = await hb.send(msg)
        const json = JSON.parse(body)
        const transformed = mod(v)
        assert.deepEqual(json, transformed)
      } catch (e) {
        console.log(e)
        err.push(v)
      }
    }
    console.log(`${err.length} failed!`)
    if (err) {
      for (let v of err) console.log(v)
    }
  })
})
