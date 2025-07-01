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
  it("should parse nested header values", async () => {
    const keys = {
      path: "/~wao@1.0/httpsig",
      body: { a: { b: 5, c: 3 } },
      data: { d: { e: 5, f: 3 } },
      key: { g: { h: 5, i: 3 } },
      num: 3,
      list: [1, 2, 3],
      str: "Hello",
    }
    const msg = await hb.sign(keys)
    const res = await hb.send(msg)
    const { path, body, data, key, num, list, str } = JSON.parse(res.body)
    assert.deepEqual(body, keys.body)
    assert.deepEqual(data, keys.data)
    assert.deepEqual(key, keys.key)
    assert.deepEqual(num, keys.num)
    assert.deepEqual(list, keys.list)
    assert.deepEqual(str, keys.str)
  })
  it("should sign data", async () => {
    const keys = {
      path: "/~wao@1.0/httpsig",
      data: { a: 3, b: 4 },
    }
    const msg = await hb.sign(keys)
    const { data } = JSON.parse((await hb.send(msg)).body)
    assert.deepEqual(data, keys.data)

    const keys2 = {
      path: "/~wao@1.0/httpsig",
      body: { a: { c: 3 }, b: 4 },
    }
    const msg2 = await hb.sign(keys2)
    const { body } = JSON.parse((await hb.send(msg2)).body)
    assert.deepEqual(body, keys2.body)
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
  it("should sign nested data and body", async () => {
    const keys = {
      path: "/~wao@1.0/httpsig",
      data: { a: 3, b: { c: { d: 4 } } },
      body: { a: 3, b: { c: { d: 4 } } },
    }
    const msg = await hb.sign(keys)
    const json = JSON.parse((await hb.send(msg)).body)
    assert.deepEqual(json.data, keys.data)
    assert.deepEqual(json.body, keys.body)
  })
  it("should sign atom", async () => {
    const keys = {
      path: "/~wao@1.0/httpsig",
      float: 1.23,
      int: 1,
      bool: true,
      atom: Symbol("atom"),
      nest: {
        bool: true,
        atom: Symbol("nested_atom"),
      },
      body: { bool: true, nest: { atom: Symbol("nested_atom") } },
      data: { bool: true, nest: { atom: Symbol("nested_atom") } },
    }
    const msg = await hb.sign(keys)
    const json = JSON.parse((await hb.send(msg)).body)
    assert.deepEqual(json.nest, { bool: true, atom: "nested_atom" })
    assert.deepEqual(json.body, { bool: true, nest: { atom: "nested_atom" } })
    assert.deepEqual(json.data, { bool: true, nest: { atom: "nested_atom" } })
    assert.deepEqual(json.float, keys.float)
    assert.deepEqual(json.int, keys.int)
    assert.deepEqual(json.bool, keys.bool)
  })

  it("should sign list", async () => {
    const keys = {
      path: "/~wao@1.0/httpsig",
      list: [1, "abc", true, Symbol("atom"), 1.23],
      body: [1, "abc", true, Symbol("atom"), 1.23],
      data: [
        1,
        "abc",
        true,
        Symbol("atom"),
        1.23,
        {
          nested: Symbol("nested"),
          int: 1,
          str: "abc",
          bool: false,
          float: 3.14,
        },
      ],
    }
    const msg = await hb.sign(keys)
    const json = JSON.parse((await hb.send(msg)).body)
    assert.deepEqual(json.data, [
      1,
      "abc",
      true,
      "atom",
      1.23,
      { bool: false, float: 3.14, int: 1, nested: "nested", str: "abc" },
    ])
  })
  it.only("should sign null/undefined & empty values", async () => {
    const keys = {
      path: "/~wao@1.0/httpsig",
      null: null,
      undefined: undefined,
      body: { list: [], map: {} },
      nested: { list: [], map: {} },
    }
    const msg = await hb.sign(keys)
    const json = JSON.parse((await hb.send(msg)).body)
    assert.deepEqual(json.nested, { list: [], map: {} })
    assert.deepEqual(json.body, { list: [], map: {} })
  })

  it("should sign empty binary & nested binary", async () => {
    const binary = Buffer.from([4, 5, 6])
    const empty = Buffer.from([])
    const keys = {
      path: "/~wao@1.0/httpsig",
      empty,
      nested: [binary, empty],
    }
    const msg = await hb.sign(keys)
    const json = JSON.parse((await hb.send(msg)).body)
    assert.equal(json.empty, "")
    assert.deepEqual(json.nested, ["BAUG", ""])
  })

  it("should sign single binary", async () => {
    const binary = Buffer.from([4, 5, 6])
    const empty = Buffer.from([])
    const keys = {
      path: "/~wao@1.0/httpsig",
      binary,
    }
    const msg = await hb.sign(keys)
    const json = JSON.parse((await hb.send(msg)).body)
    assert.deepEqual(json.binary, "BAUG")
  })
  it("should sign multiple binaries without body/data", async () => {
    const binary = Buffer.from([4, 5, 6])
    const empty = Buffer.from([])
    const keys = {
      path: "/~wao@1.0/httpsig",
      bin: binary,
      bin2: binary,
    }
    const msg = await hb.sign(keys)
    const json = JSON.parse((await hb.send(msg)).body)
    assert.deepEqual(json.bin, "BAUG")
    assert.deepEqual(json.bin2, "BAUG")
  })
  const bin = Buffer.from([1, 2, 3])
  let keys = [
    {
      str: "abc",
      bool: true,
      num: 123,
      float: 3.14,
      atom: Symbol("atom"),
      list: [1, 2, 3],
      binary: Buffer.from([1, 2, 3]),
    },
    { map: { a: 1, b: 2, c: { d: 3 } }, binary: bin },
    { bin, bin2: Buffer.from([1, 2, 3]), body: 3 },

    { list: [1, [2, 3]] },
    { map: { a: 3, b: "abc", c: { d: { e: 3 } } } },
    { list: [1, [2, 3]] },
    { bin: [bin, bin] },
    { bin: bin, bin2: bin },
    { map: { jntzf: 8.02 } },
    { list: [53.05] },
    { map: { float: 86.01, bool: true } },
    { key: [[8.02]] },
    { list: [Symbol("atom")] },
  ]
  //keys = []
  it("should test signer", async () => {
    for (const v of keys) {
      console.log()
      console.log("--------------", v)
      const msg = await hb.sign({ path: "/~wao@1.0/httpsig", ...v })
      console.log(msg)
      const { body } = await hb.send(msg)
      console.log(JSON.parse(body))
    }
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
