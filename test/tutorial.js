import assert from "assert"
import { after, describe, it, before, beforeEach } from "node:test"
import { acc, toAddr } from "../src/test.js"
import { omit } from "ramda"
import { getJWK } from "./lib/test-utils.js"
import HB from "../src/hb.js"
import { sign } from "../src/signer.js"
import { send } from "../src/send.js"
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

  it("should cache by hashpath", async () => {
    let {
      out: { new_hashpath, num },
    } = await hb.post({
      path: "/~wao@1.0/resolve/~wao@1.0/resolve/~wao@1.0/resolve",
    })

    while (num < 100) {
      console.log("new hash", new_hashpath)
      const { out } = await hb.post({
        path: "/~wao@1.0/resolve/~wao@1.0/resolve/~wao@1.0/resolve",
        init_hashpath: new_hashpath,
      })
      ;({ new_hashpath, num } = out)
    }
    assert.equal(num, 102)
    console.log(await hb.get({ path: `/${new_hashpath}/~wao@1.0/resolve` }))
  })

  it("should test function chain", async () => {
    const { hashpath, out } = await hb.post({
      path: "/~wao@1.0/chain3",
      num: 1,
    })
    const [hash, id] = hashpath.split("/")
    assert.deepEqual(out, {
      added: 10,
      device: "wao@1.0",
      num: 16,
      operation: "plus",
    })
  })

  it.only("should test function chain", async () => {
    const { hashpath, out } = await hb.post({
      path: "/~wao@1.0/inc/~wao@1.0/inc/~wao@1.0/inc",
    })
    assert.equal(out.chain.length, 3)
  })

  it("should test httpsig", async () => {
    const json = { list: [1, 2, 3] }
    console.log(json)
    const res = await hb.post({
      path: "/~wao@1.0/structured_from",
      body: JSON.stringify(json),
    })
    const structured = JSON.parse(res.out)
    console.log(structured)
    const res2 = await hb.post({
      path: "/~wao@1.0/httpsig_to",
      body: JSON.stringify(structured),
    })
    const encoded = JSON.parse(res2.out)
    const signed_msg = await sign({
      jwk,
      msg: encoded,
      path: "/~wao@1.0/forward",
      url: "http://localhost:10001",
    })
    const { out } = await send(signed_msg)
    console.log(JSON.parse(out))
    const res3 = await hb.post({
      path: "/~wao@1.0/httpsig_from",
      body: JSON.stringify(encoded),
    })
    const decoded = omit(["body-keys", "content-type"])(JSON.parse(res3.out))
    console.log(decoded)
    const res4 = await hb.post({
      path: "/~wao@1.0/structured_to",
      body: JSON.stringify(decoded),
    })
    const json2 = JSON.parse(res4.out)
    console.log(json2)
  })

  it("should test flat", async () => {
    const res = await hb.post({
      path: "/~wao@1.0/flat_from",
      body: JSON.stringify({ "a/b/c": 30 }),
    })
    console.log(JSON.parse(res.out))
    const res2 = await hb.post({
      path: "/~wao@1.0/flat_to",
      body: JSON.stringify({ a: { b: "ab" } }),
    })
    console.log(JSON.parse(res.out))
  })
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
})
