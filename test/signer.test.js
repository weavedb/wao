import assert from "assert"
import { send as _send } from "../src/signer.js"
import { after, describe, it, before, beforeEach } from "node:test"
import { acc, mu, AO, toAddr } from "../src/test.js"
import { getJWK } from "./lib/test-utils.js"
import HB from "../src/hb.js"
import { isNotNil, filter, isNil, range } from "ramda"
import { randomBytes } from "node:crypto"
import { wait } from "../src/utils.js"
import HyperBEAM from "../src/hyperbeam.js"
import { readFileSync } from "fs"
import { resolve } from "path"

function extractInnerBodyDynamic(signedRequest, commitmentId) {
  const { headers } = signedRequest

  // Extract commitment metadata
  const commitment = {
    alg: extractAlgorithm(headers["signature-input"]),
    "commitment-device": "httpsig@1.0",
    signature: headers.signature,
    "signature-input": headers["signature-input"],
  }

  // Parse the signed fields from signature-input
  const signedFields = extractSignedFields(headers["signature-input"])

  // Build inner body
  const innerBody = {
    commitments: {
      [commitmentId]: commitment,
    },
  }

  // Add all non-derived fields (those without @)
  signedFields.forEach(field => {
    if (!field.startsWith("@") && field !== "ao-types") {
      const value = headers[field]
      if (value !== undefined) {
        // Check ao-types to determine proper type conversion
        if (
          headers["ao-types"] &&
          headers["ao-types"].includes(`${field}="integer"`)
        ) {
          innerBody[field] = parseInt(value, 10)
        } else {
          innerBody[field] = value
        }
      }
    }
  })

  return innerBody
}

/**
 * Helper: Extract algorithm from signature-input
 */
function extractAlgorithm(signatureInput) {
  const match = signatureInput.match(/alg="([^"]+)"/)
  return match ? match[1] : "rsa-pss-sha512"
}

/**
 * Helper: Extract signed fields from signature-input
 */
function extractSignedFields(signatureInput) {
  // Extract the fields list from signature-input
  // Format: http-sig-xxx=("field1" "field2" ...);alg=...
  const match = signatureInput.match(/\(([^)]+)\)/)
  if (!match) return []

  // Parse the quoted fields
  const fieldsStr = match[1]
  const fields = fieldsStr.match(/"[^"]+"/g) || []

  return fields.map(f => f.replace(/"/g, ""))
}

const data = `
local count = 0
Handlers.add("Inc", "Inc", function (msg)
  count = count + 1
  msg.reply({ Data = "Count: "..tostring(count) })
end)

Handlers.add("Get", "Get", function (msg)
  msg.reply({ Data = "Count: "..tostring(count) })
end)`

const URL = "http://localhost:10001"

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
      path: "/~wao@1.0/httpsig_to_json",
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
    assert.deepEqual(path, "httpsig_to_json")
  })
  it("should sign data", async () => {
    const keys = {
      path: "/~wao@1.0/httpsig_to_json",
      data: { a: 3, b: 4 },
    }
    const msg = await hb.sign(keys)
    const { data } = JSON.parse((await hb.send(msg)).body)
    assert.deepEqual(data, keys.data)

    const keys2 = {
      path: "/~wao@1.0/httpsig_to_json",
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
      path: "/~wao@1.0/httpsig_to_json",
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
      path: "/~wao@1.0/httpsig_to_json",
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
      path: "/~wao@1.0/httpsig_to_json",
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
  it("should sign null/undefined & empty values", async () => {
    const keys = {
      path: "/~wao@1.0/httpsig_to_json",
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
      path: "/~wao@1.0/httpsig_to_json",
      empty,
      nested: [binary, empty],
    }
    const msg = await hb.sign(keys)
    const json = JSON.parse((await hb.send(msg)).body)
    assert.equal(json.empty, "")
    assert.deepEqual(json.nested, ["\x04\x05\x06", ""])
  })

  it("should sign single binary", async () => {
    const binary = Buffer.from([4, 5, 6])
    const empty = Buffer.from([])
    const keys = {
      path: "/~wao@1.0/httpsig_to_json",
      binary,
    }
    const msg = await hb.sign(keys)
    const json = JSON.parse((await hb.send(msg)).body)
    assert.deepEqual(json.binary, "\x04\x05\x06")
  })
  it("should sign multiple binaries without body/data", async () => {
    const binary = Buffer.from([4, 5, 6])
    const empty = Buffer.from([])
    const keys = {
      path: "/~wao@1.0/httpsig_to_json",
      bin: binary,
      bin2: binary,
    }
    const msg = await hb.sign(keys)
    const json = JSON.parse((await hb.send(msg)).body)
    assert.deepEqual(json.bin, "\x04\x05\x06")
    assert.deepEqual(json.bin2, "\x04\x05\x06")
  })
})
