import assert from "assert"
import { after, describe, it, before, beforeEach } from "node:test"
import { acc, mu, AO, toAddr } from "../src/test.js"
import { mod, getJWK } from "./lib/test-utils.js"
import { generateTestCases } from "./gen.js"
import HB from "../src/hb.js"
import { isNotNil, filter, isNil, range } from "ramda"
import { randomBytes } from "node:crypto"
import { wait } from "../src/utils.js"
import HyperBEAM from "../src/hyperbeam.js"
import erl from "../src/toerl.js"

const data = `
local count = 0
Handlers.add("Inc", "Inc", function (msg)
  count = count + 1
  msg.reply({ Data = "Count: "..tostring(count) })
end)

Handlers.add("Get", "Get", function (msg)
  msg.reply({ Data = "Count: "..tostring(count) })
end)`

describe("Hyperbeam Signer", function () {
  let hb, hb2, hbeam, jwk, addr, store_prefix
  before(async () => {
    jwk = getJWK("../../HyperBEAM/.wallet.json")
    addr = toAddr(jwk.n)
    hbeam = await new HyperBEAM({
      as: [],
      c: "12",
      cmake: "3.5",
      //gateway: 4000,
      //operator: addr,
    }).ready()
  })

  beforeEach(async () => {
    hb = await new HB({}).init(jwk)
  })

  after(async () => {
    hbeam.kill()
  })
  it("should generate valid signatures", async () => {
    const { pid } = await hb.spawn()
    const { slot } = await hb.schedule({ pid })
    const { results } = await hb.compute({ pid, slot })
    assert.deepEqual(results, { "assignment-slot": 1 })
  })
  it("should sign a valid message", async () => {
    const { pid } = await hb.spawn()
    const keys = {
      path: `/${pid}~process@1.0/schedule/~json@1.0/serialize`,
    }
    const res = await hb.post(keys)
    const { slot } = JSON.parse(res.body)
    assert.equal(slot, 1)
  })
  it.only("should fuzz test random objects", async () => {
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
      const msg = await hb.sign({ path: "/~wao@1.0/httpsig", ...v })
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
