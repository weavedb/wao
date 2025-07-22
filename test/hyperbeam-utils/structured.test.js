import assert from "assert"
import { describe, it, before, after } from "node:test"
import HyperBEAM from "../../src/hyperbeam.js"
import { structured_from, structured_to } from "../../src/structured.js"
import { cases_from, cases_to } from "./structured_cases.js"
import cases from "../lib/cases.js"
import { erl_json_from, erl_json_to, normalize } from "../../src/erl_json.js"
import { erl_str_from, erl_str_to } from "../../src/erl_str.js"

const test = async (hb, cases, path, mod) => {
  let err = []
  let success = []
  for (const v of cases.slice(0, 1)) {
    const json = erl_json_to(v)
    try {
      console.log(v, json, normalize(v))
      const { out } = await hb.post({ path, body: JSON.stringify(json) })
      console.log(out)
      console.log("response", out)
      console.log("structured_from(normalize(v))", mod(normalize(v)))
      console.log("erl_str_from(out)", erl_str_from(out))
      assert.deepEqual(mod(normalize(v)), erl_str_from(out))
      success.push(v)
    } catch (e) {
      console.log(e)
      err.push(v)
      break
    }
  }
  //console.log(`${err.length} / ${cases.length} failed!`)
  console.log(`${success.length} success`)
  if (err) {
    console.log(`error with the case`)
    for (let v of err) console.log(JSON.stringify(v))
  }
}

describe("Hyperbeam Signer", function () {
  let hb, hbeam
  before(async () => {
    hbeam = await new HyperBEAM({ reset: true }).ready()
    hb = hbeam.hb
  })
  after(async () => hbeam.kill())
  it("should test structured_from codec", async () => {
    await test(hb, cases, "/~wao@1.0/structured_from", structured_from)
  })

  /*it("should test structured_from codec", async () => {
    await test(hb, cases_from, "/~wao@1.0/structured_from", structured_from)
  })*/
  /*
  it("should test structured_to codec", async () => {
    await test(hb, cases_to, "/~mydev@1.0/structured_to", structured_to)
  })*/
  /*
  it.only("should test structured_to codec", async () => {
    console.log(
      await hb.p("/~wao@1.0/httpsig", {
        bin: btoa("Hello World"),
        structured_bin: `:${btoa("Hello World")}:`,
      })
    )
  })*/
})
