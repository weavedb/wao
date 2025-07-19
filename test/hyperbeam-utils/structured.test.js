import assert from "assert"
import { describe, it, before, after } from "node:test"
import HyperBEAM from "../../src/hyperbeam.js"
import { structured_from, structured_to } from "../../src/structured.js"
import { cases_from, cases_to } from "./structured_cases.js"

const test = async (hb, cases, path, mod) => {
  let err = []
  let success = []
  for (const v of cases) {
    const msg = await hb.sign({ path, body: JSON.stringify(v) })
    try {
      const { body } = await hb.send(msg)
      const json = JSON.parse(body)
      assert.deepEqual(mod(v), json)
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

  it("should test structured_from codec", async () => {
    await test(hb, cases_from, "/~mydev@1.0/structured_from", structured_from)
  })

  it("should test structured_to codec", async () => {
    await test(hb, cases_to, "/~mydev@1.0/structured_to", structured_to)
  })
})
