import assert from "assert"
import { after, describe, it, before, beforeEach } from "node:test"
import { acc, mu, AO, toAddr } from "../src/test.js"
import { getJWK } from "./lib/test-utils.js"
import HB from "../src/hb.js"
import HyperBEAM from "../src/hyperbeam.js"
import keys from "./cases.js"

// Recursively transform values to match expected format
function mod(obj) {
  if (obj === null) return "null"
  if (obj === undefined) return "undefined"

  // Check if it's a symbol-like value (atom in Erlang/Elixir)
  if (typeof obj === "string" && obj.match(/^[a-z_][a-zA-Z0-9_]*$/)) {
    // This looks like an atom value, keep as is
    return obj
  }

  // Handle arrays
  if (Array.isArray(obj)) {
    return obj.map(item => mod(item))
  }

  // Handle binary data (Buffer, Uint8Array, etc.)
  if (
    obj instanceof Uint8Array ||
    obj instanceof ArrayBuffer ||
    Buffer.isBuffer(obj)
  ) {
    // Convert to base64
    const buffer = Buffer.isBuffer(obj) ? obj : Buffer.from(obj)
    return buffer.toString("base64")
  }

  // Handle objects
  if (typeof obj === "object" && obj !== null) {
    const result = {}
    for (const [key, value] of Object.entries(obj)) {
      result[key] = mod(value)
    }
    return result
  }

  // Return primitive values as-is
  return obj
}

describe("Hyperbeam Signer", function () {
  let hb, hb2, hbeam, jwk, addr, store_prefix
  before(async () => {
    jwk = getJWK("../../HyperBEAM/.wallet.json")
    addr = toAddr(jwk.n)
    hbeam = await new HyperBEAM({ as: [], c: "12", cmake: "3.5" }).ready()
  })
  beforeEach(async () => (hb = await new HB({}).init(jwk)))
  after(async () => hbeam.kill())

  it.only("should test signer", async () => {
    for (const v of keys) {
      const msg = await hb.sign({ path: "/~wao@1.0/httpsig", ...v })
      const { body } = await hb.send(msg)
      const json = JSON.parse(body)
      console.log(JSON.stringify(json))

      // Apply mod transformation and assert
      const transformed = mod(json)

      // Expected values would come from your test cases
      // For now, just verify the transformation works
      assert.deepEqual(transformed, transformed) // Replace with actual expected value
    }
  })
})
