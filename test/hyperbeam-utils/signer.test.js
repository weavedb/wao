import assert from "assert"
import { after, describe, it, before, beforeEach } from "node:test"
import { mod } from "../lib/test-utils.js"
import { generateTestCases } from "../lib/gen.js"
import HB from "../../src/hb.js"
import HyperBEAM from "../../src/hyperbeam.js"

describe("Hyperbeam Signer", function () {
  let hb, hbeam
  before(async () => (hbeam = await new HyperBEAM({ reset: true }).ready()))

  beforeEach(async () => (hb = hbeam.hb))

  after(async () => hbeam.kill())
  it("should generate valid signatures", async () => {
    const { pid } = await hb.spawn()
    const { slot } = await hb.schedule({ pid })
    const { results } = await hb.compute({ pid, slot })
    assert.deepEqual(results, { "assignment-slot": 1 })
  })
  it("should fuzz test random objects", async () => {
    let err = []
    const cases = generateTestCases(1000)
    let i = 0
    for (const v of cases) {
      console.log()
      console.log(
        `[${++i}] -------------------------------------------------------`
      )
      console.log(v)
      console.log()
      console.log(JSON.stringify(v))
      console.log()
      const msg = await hb.sign(
        { path: "/~wao@1.0/httpsig", ...v },
        { path: false }
      )
      console.log(msg)
      try {
        const { body } = await hb.send(msg)
        const json = JSON.parse(body)
        const transformed = mod(v)
        assert.deepEqual(json, transformed) // Replace with actual expected value
      } catch (e) {
        console.log(err.push(v))
        console.log(e)
      }
    }
    console.log(err)
  })
})
