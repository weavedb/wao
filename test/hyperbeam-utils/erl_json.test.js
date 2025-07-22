import assert from "assert"
import { describe, it, before, after } from "node:test"
import HyperBEAM from "../../src/hyperbeam.js"
import { erl_json_from, erl_json_to, normalize } from "../../src/erl_json.js"
import { erl_str_from, erl_str_to } from "../../src/erl_str.js"
import { cases_from } from "./erl_json_cases.js"
import all_cases from "./all_cases.js"
import { gen } from "./gen.js"

const test = async (hb, cases, path) => {
  let err = []
  let success = []
  let i = 0
  for (const v of cases) {
    console.log(`[${++i}]...........................................`, v)
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
      break
    }
  }
  console.log(
    `success: ${success.length}, error: ${err.length}, total: ${cases.length}`
  )
  if (err) {
    for (let v of err) console.log(v)
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

  it("should test json_to_erl_from codec", async () => {
    await test(hb, cases_from, "/~encode@1.0/json_to_erl")
  })
  it("should test json_to_erl_from codec", async () => {
    await test(hb, all_cases, "/~encode@1.0/json_to_erl")
  })

  it("should fuzz test edge cases", async () => {
    await test(hb, gen(1000), "/~encode@1.0/json_to_erl")
  })
})
