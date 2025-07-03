import assert from "assert"
import { after, describe, it, before, beforeEach } from "node:test"
import { acc, mu, AO, toAddr } from "../src/test.js"
import { getJWK } from "./lib/test-utils.js"
import { generateTestCases } from "./gen.js"
import HB from "../src/hb.js"
import { isNotNil, filter, isNil, range } from "ramda"
import { randomBytes } from "node:crypto"
import { wait } from "../src/utils.js"
import HyperBEAM from "../src/hyperbeam.js"

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
    store_prefix = "cache-mainnet-" + Math.floor(Math.random() * 10000000)
    jwk = getJWK("../../HyperBEAM/.wallet.json")
    addr = toAddr(jwk.n)
    hbeam = await new HyperBEAM({
      as: [],
      store_prefix,
      c: "12",
      cmake: "3.5",
      gateway: 4000,
      operator: addr,
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
  it.skip("should fuzz test random objects", async () => {
    for (const v of generateTestCases(20)) {
      console.log()
      console.log("--------------", v.header)
      console.log(JSON.stringify(v.header))
      const msg = await hb.sign({ path: "/~wao@1.0/httpsig", ...v.header })
      console.log(msg)
      const { body } = await hb.send(msg)
      const json = JSON.parse(body)
      console.log(json)
      console.log(v.returned)
      assert.deepEqual(json, v.returned)
    }
  })
})
