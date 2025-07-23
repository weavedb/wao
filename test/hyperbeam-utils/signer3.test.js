import assert from "assert"
import { after, describe, it, before, beforeEach } from "node:test"
import { mod2 } from "../lib/test-utils.js"
import HyperBEAM from "../../src/hyperbeam.js"
import ok, { simple, errors } from "../lib/cases2.js"
import { generateTestCases } from "../lib/gen.js"
import { structured_from, structured_to } from "../../src/structured.js"
import { httpsig_from, httpsig_to } from "../../src/httpsig.js"
import { erl_json_from, erl_json_to, normalize } from "../../src/erl_json.js"
import { sign } from "../../src/signer2.js"

describe("Hyperbeam Signer", function () {
  let hb, hb2, hbeam, jwk, addr, store_prefix
  before(async () => (hbeam = await new HyperBEAM({ reset: true }).ready()))
  beforeEach(async () => (hb = hbeam.hb))
  after(async () => hbeam.kill())

  it("should test signer", async () => {
    let cases = ok
    let err = []
    let success = []
    let i = 0
    for (const v of cases) {
      console.log(`[${++i}]...........................................`, v)
      try {
        const encoded = await sign({ ...v, path: "/~wao@1.0/httpsig" })
        const msg = await hb.signEncoded(encoded)
        console.log(msg)
        const { body } = await hb.send(msg)
        const json = JSON.parse(body)
        const transformed = mod2(v)
        console.log(v, transformed, json)
        assert.deepEqual(json, transformed)
        success.push(v)
      } catch (e) {
        console.log(e)
        err.push(v)
        break
      }
    }
    console.log(
      `success: ${success.length}, error: ${err.length}, total: ${cases.length}`
    )
    if (err) for (let v of err) console.log(v)
    assert.equal(err.length, 0)
  })
})
