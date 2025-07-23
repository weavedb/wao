import assert from "assert"
import { describe, it, before, after } from "node:test"
import HyperBEAM from "../../src/hyperbeam.js"
import { structured_from, structured_to } from "../../src/structured.js"
import { cases_from, cases_to } from "./structured_cases.js"
import cases from "../lib/cases.js"
import { erl_json_from, erl_json_to, normalize } from "../../src/erl_json.js"
import { erl_str_from, erl_str_to, parse_body } from "../../src/erl_str.js"

const test = async (hb, cases, path, mod) => {
  let err = []
  let success = []
  let i = 0
  for (const v of cases) {
    console.log(`[${++i}]...........................................`, v)
    const json = erl_json_to(v)
    try {
      const { out } = await hb.post({ path, body: JSON.stringify(json) })
      const expected = structured_from(normalize(v))
      const output = erl_str_from(out)
      console.log("erl_str_from(out)", output)
      console.log("structured_from(normalize(v))", expected)
      assert.deepEqual(expected, output)
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

  if (err) {
    console.log(`error with the case`)
    for (let v of err) console.log(JSON.stringify(v))
  }
  assert.equal(err.length, 0)
}

describe("Hyperbeam Signer", function () {
  let hb, hbeam
  before(async () => {
    hbeam = await new HyperBEAM({ reset: true }).ready()
    hb = hbeam.hb
  })
  after(async () => hbeam.kill())
  it("should test structured_from codec", async () => {
    await test(hb, cases_from, "/~encode@1.0/structured_from", structured_from)
  })
  it("should test structured_from codec", async () => {
    await test(hb, cases, "/~encode@1.0/structured_from", structured_from)
  })
})
