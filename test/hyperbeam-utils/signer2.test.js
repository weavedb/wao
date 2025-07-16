import assert from "assert"
import { after, describe, it, before, beforeEach } from "node:test"
import { getJWK, mod } from "../lib/test-utils.js"
import HyperBEAM from "../../src/hyperbeam.js"
import keys from "../lib/cases.js"
import { generateTestCases } from "../lib/gen.js"
describe("Hyperbeam Signer", function () {
  let hb, hb2, hbeam, jwk, addr, store_prefix
  before(async () => (hbeam = await new HyperBEAM({ reset: true }).ready()))
  beforeEach(async () => (hb = hbeam.hb))
  after(async () => hbeam.kill())

  it.only("should test signer", async () => {
    //let cases = generateTestCases(1000)
    let cases = keys
    let err = []
    let success = []
    for (const v of cases) {
      const msg = await hb.sign({ path: "/~wao@1.0/httpsig", ...v })
      console.log(msg)
      try {
        const { body } = await hb.send(msg)
        const json = JSON.parse(body)
        const transformed = mod(v)
        assert.deepEqual(json, transformed)
        success.push(v)
      } catch (e) {
        console.log(e)
        err.push(v)
      }
    }
    console.log(`${err.length} / ${cases.length} failed!`)
    if (err) {
      for (let v of err) console.log(v)
    }
  })
})
