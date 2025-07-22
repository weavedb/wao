import assert from "assert"
import { describe, it } from "node:test"
import bin_to_str from "../../src/bin_to_str.js"
import { to_buf } from "../../src/erl_str.js"
import { cases } from "./bin_to_str_cases.js"
import { cases_bin } from "./bin_to_str_binary_cases.js"

const test = cases => {
  let err = []
  let success = []
  for (const v of cases) {
    try {
      const result = bin_to_str(v.input)
      assert.deepEqual(result, v.expected)
      success.push(v)
    } catch (e) {
      console.log(e)
      err.push(v)
    }
  }
  console.log(`${err.length} / ${cases.length} failed!`)
  if (err.length > 0) {
    for (let v of err) console.log(v)
  }
}

const test2 = cases => {
  let err = []
  let success = []
  for (const v of cases) {
    try {
      const result = bin_to_str(to_buf(v.input))
      console.log(to_buf(v.input), result)
      assert.deepEqual(result, v.expected)
      success.push(v)
    } catch (e) {
      console.log(e)
      err.push(v)
    }
  }
  console.log(`${err.length} / ${cases.length} failed!`)
  if (err.length > 0) {
    for (let v of err) console.log(v)
  }
}

describe("bin_to_str test", function () {
  it("should test bin_to_str conversion", () => {
    test(cases)
  })

  it("should test bin_to_str conversion #2", () => {
    test2(cases_bin)
  })
})
