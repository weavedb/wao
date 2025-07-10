import assert from "assert"
import { after, describe, it, before, beforeEach } from "node:test"
import { acc, toAddr } from "../src/test.js"
import { getJWK } from "./lib/test-utils.js"
import HB from "../src/hb.js"
import HyperBEAM from "../src/hyperbeam.js"

const jwk = getJWK("../../HyperBEAM/.wallet.json")
const addr = toAddr(jwk.n)

describe("Hyperbeam Device", function () {
  let hb, hbeam
  before(async () => {
    hbeam = await new HyperBEAM({
      devices: ["meta", "httpsig", "structured", "flat", "json", "wao"],
      clearCache: true,
      c: "12",
      cmake: "3.5",
    }).ready()
  })

  beforeEach(async () => {
    hb = await new HB({}).init(jwk)
  })

  after(async () => hbeam.kill())

  it("should test devices", async () => {
    const res = await hb.get({ path: "/~meta@1.0/info" })
    console.log(res)

    const res2 = await hb.get({ path: "/~meta@1.0/info/address" })
    console.log(res2.body)

    const res3 = await hb.get({ path: "/~meta@1.0/info/~json@1.0/serialize" })
    console.log(JSON.parse(res3.body))

    const res4 = await hb.post({
      path: "/~meta@1.0/info",
      test_conf: "abc",
    })
    console.log(res4.body)
    const res5 = await hb.get({ path: "/~meta@1.0/info/test_conf" })
    console.log(res5)
  })
  it("should test json@1.0", async () => {
    const res = await hb.post({
      path: "/~json@1.0/deserialize",
      body: JSON.stringify({ key: 3, list: [1, 2, 3], obj: { a: 4 } }),
    })
    console.log(res)
    const res2 = await hb.post({
      path: "/~json@1.0/serialize",
      key3: [1, 2, [3, 4]],
    })
    console.log(JSON.parse(res2.body), res2.body)
  })

  it("should test flat@1.0", async () => {
    const { body } = await hb.post({
      path: "/~wao@1.0/flat_from",
      sample: 3,
      sample2: "abc",
      data: { abc: { def: { ghi: 3 } } },
      bool: true,
    })
    console.log(JSON.parse(body))
  })
})
