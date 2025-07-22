import assert from "assert"
import { describe, it, before, after } from "node:test"
import HyperBEAM from "../../src/hyperbeam.js"
import { erl_json_from, erl_json_to, normalize } from "../../src/erl_json.js"
import { erl_str_from, erl_str_to } from "../../src/erl_str.js"
//import { cases_from } from "./erl_json_cases.js"
import cases_from from "../lib/cases.js"
const test = async (hb, cases, path) => {
  let err = []
  let success = []
  for (const v of cases) {
    const json = erl_json_to(v)
    const msg = await hb.sign({ path, body: JSON.stringify(json) })
    try {
      console.log(v, json, normalize(v))
      const { body } = await hb.send(msg)
      console.log("response", body)
      console.log("erl_str_from(body)", erl_str_from(body))
      console.log("normalize(v)", normalize(v))
      assert.deepEqual(normalize(v), erl_str_from(body))
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
}

describe("Hyperbeam Signer", function () {
  let hb, hbeam
  before(async () => {
    hbeam = await new HyperBEAM({ reset: true }).ready()
    hb = hbeam.hb
  })
  after(async () => hbeam.kill())

  it("should test json_to_erl_from codec", async () => {
    await test(hb, cases_from, "/~wao@1.0/json_to_erl")
  })
})
