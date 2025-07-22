import assert from "assert"
import { describe, it, before, after } from "node:test"
import HyperBEAM from "../../src/hyperbeam.js"
import { flat_from, flat_to } from "../../src/flat.js"
import { structured_from, structured_to } from "../../src/structured.js"
import { httpsig_from, httpsig_to } from "../../src/httpsig.js"
import cases from "../lib/cases.js"

const test = async (hb, cases) => {
  let err = []
  let success = []
  for (const v of cases.slice(0, 1)) {
    try {
      const res = await hb.post({
        path: "/~mydev@1.0/structured_from",
        body: JSON.stringify(v),
      })
      const structured = JSON.parse(res.body)
      const res2 = await hb.post({
        path: "/~mydev@1.0/httpsig_to",
        body: JSON.stringify(structured),
      })
      const encoded = JSON.parse(res2.body)
      assert.deepEqual(httpsig_to(structured_from(v)), encoded)
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

  it("should test encoding", async () => {
    await test(hb, cases)
  })
})
