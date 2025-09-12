import assert from "assert"
import { after, describe, it, before, beforeEach } from "node:test"
import { modIn, modOut } from "./lib/test-utils.js"
import { verify } from "../src/signer-utils.js"
import { HyperBEAM } from "wao/test"
import cases, { errors } from "./lib/cases.js"
import { normalize } from "../src/erl_json.js"
import { createSigner } from "../src/signer.js"
import { send } from "../src/send.js"
import { erl_str_from, erl_str_to } from "../src/erl_str.js"
describe("Hyperbeam Signer", function () {
  let hbeam, sign
  before(async () => {
    hbeam = await new HyperBEAM({ reset: true }).ready()
    sign = createSigner(hbeam.jwk, hbeam.url)
  })
  after(async () => hbeam.kill())

  it("should test signer", async () => {
    let err = []
    let success = []
    let i = 0
    let _cases = cases
    for (const v of _cases) {
      console.log(`[${++i}]...........................................`, v)
      try {
        const signed = await sign(
          { path: "/~hbsig@1.0/msg2", ...v },
          { path: false }
        )
        const { out } = await send(signed)
        const output = modOut(out)
        const exp = modIn(normalize(v))
        assert.deepEqual(exp, output)
        success.push(v)
      } catch (e) {
        console.log(e)
        err.push(v)
      }
    }
    console.log(`${err.length} / ${_cases.length} failed!`)
    if (err) for (let v of err) console.log(v)
  })
})
