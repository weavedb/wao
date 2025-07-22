import assert from "assert"
import { after, describe, it, before, beforeEach } from "node:test"
import { mod2 } from "../lib/test-utils.js"
import HyperBEAM from "../../src/hyperbeam.js"
import keys from "../lib/cases.js"
import { generateTestCases } from "../lib/gen.js"
import { structured_from, structured_to } from "../../src/structured.js"
import { httpsig_from, httpsig_to } from "../../src/httpsig.js"
import { erl_json_from, erl_json_to, normalize } from "../../src/erl_json.js"
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
      try {
        const encoded = httpsig_to(
          structured_from(normalize({ ...v, path: "/~wao@1.0/httpsig" }))
        )
        const msg = await hb.signEncoded(encoded)
        console.log(msg)
        const { body } = await hb.send(msg)
        const json = JSON.parse(body)
        const transformed = mod2(v)
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
