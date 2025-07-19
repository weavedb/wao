import assert from "assert"
import { after, describe, it, before, beforeEach } from "node:test"
import { mod } from "../lib/test-utils.js"
import HyperBEAM from "../../src/hyperbeam.js"
import keys from "../lib/cases.js"
import { generateTestCases } from "../lib/gen.js"
describe("Hyperbeam Signer", function () {
  let hb, hb2, hbeam, jwk, addr, store_prefix
  before(async () => (hbeam = await new HyperBEAM({ reset: true }).ready()))
  beforeEach(async () => (hb = hbeam.hb))
  after(async () => hbeam.kill())
  it.only("should test signer", async () => {
    // test fails... why?
    const msg = await hb.sign(
      { path: "/~wao@1.0/httpsig", testy: "3" },
      { path: true }
    )
    console.log(msg)
    const { body } = await hb.send(msg)
    //const json = JSON.parse(body)
    //console.log(json)
  })
})
