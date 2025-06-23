import assert from "assert"
import { send as _send } from "../src/signer.js"
import { after, describe, it, before, beforeEach } from "node:test"
import { acc, mu, AO, toAddr } from "../src/test.js"
import { getJWK } from "./lib/test-utils.js"
import HB from "../src/hb.js"
import { isNotNil, filter, isNil } from "ramda"
import { randomBytes } from "node:crypto"
import { wait } from "../src/utils.js"
import Server from "../src/server.js"
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

describe("Hyperbeam Legacynet", function () {
  let hb, hb2, hbeam, jwk, server, addr, store_prefix
  before(async () => {
    store_prefix = "cache-mainnet-" + Math.floor(Math.random() * 10000000)
    server = new Server({ port: 4000, log: true, hb_url: URL })
    jwk = getJWK("../../HyperBEAM/.wallet.json")
    addr = toAddr(jwk.n)
    hbeam = new HyperBEAM({
      store_prefix,
      c: "12",
      cmake: "3.5",
      gateway: 4000,
      legacy: true,
      operator: addr,
    })

    await wait(5000)
  })

  beforeEach(async () => {
    hb = await new HB({}).init(jwk)
  })

  after(async () => hbeam.kill())
  it.only("should generate valid signatures", async () => {
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
    const msg = await hb._request2(keys)
    const { path, body, data, key, num, list, str } = JSON.parse(
      (await _send(msg)).body
    )
    assert.deepEqual(body, keys.body)
    assert.deepEqual(data, keys.data)
    assert.deepEqual(key, keys.key)
    assert.deepEqual(num, keys.num)
    assert.deepEqual(list, keys.list)
    assert.deepEqual(str, keys.str)
    assert.deepEqual(path, "httpsig_to_json")
  })
})
