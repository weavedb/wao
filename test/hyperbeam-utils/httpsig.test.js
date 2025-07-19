import assert from "assert"
import { describe, it, before, after } from "node:test"
import HyperBEAM from "../../src/hyperbeam.js"
import { httpsig_from, httpsig_to } from "../../src/httpsig.js"
import { cases_from, cases_to } from "./httpsig_cases.js"

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

  it("should test httpsig_from codec", async () => {
    await test(hb, cases_from, "/~mydev@1.0/httpsig_from", httpsig_from)
  })

  it("should test httpsig_to codec", async () => {
    await test(hb, cases_to, "/~mydev@1.0/httpsig_to", httpsig_to)
  })
})
