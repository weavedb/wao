import { send } from "../../src/send.js"
import { erl_json_to, normalize } from "../../src/erl_json.js"
import { erl_str_from } from "../../src/erl_str.js"
import assert from "assert"
import { describe, it, before, after } from "node:test"
import { HyperBEAM } from "../../../src/test.js"
import { createSigner } from "../../src/signer.js"

function mod(obj) {
  // Handle undefined - convert to string "undefined"
  if (obj === undefined) return "undefined"

  // Handle symbols - convert to their description or "symbol"
  if (typeof obj === "symbol") {
    const desc = obj.description || "symbol"
    // Special handling for symbols with descriptions that match special values
    if (desc === "null") return null
    if (desc === "undefined") return undefined
    if (desc === "true") return true
    if (desc === "false") return false
    return desc
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
    // Convert to empty string for empty buffers
    const buffer = Buffer.isBuffer(obj) ? obj : Buffer.from(obj)
    return buffer.length === 0 ? "" : buffer.toString("base64")
  }

  // Handle objects
  if (typeof obj === "object" && obj !== null) {
    const result = {}
    for (const [key, value] of Object.entries(obj)) {
      // Lowercase the key when creating the result object
      result[key.toLowerCase()] = mod(value)
    }
    return result
  }

  // Handle strings - check if it's already an atom-like string
  if (typeof obj === "string" && obj.match(/^[a-z_][a-zA-Z0-9_]*$/)) {
    // This looks like an atom value, keep as is
    return obj
  }

  // Return primitive values as-is (strings, numbers, booleans, null)
  return obj
}

// Recursively transform values to match expected format, removing undefined values
function mod2(obj) {
  // Handle undefined - return undefined to signal removal
  if (obj === undefined) return undefined

  // Handle symbols - convert to their description or "symbol"
  if (typeof obj === "symbol") {
    const desc = obj.description || "symbol"
    // Special handling for symbols with descriptions that match special values
    if (desc === "null") return null
    if (desc === "undefined") return undefined // This will be removed
    if (desc === "true") return true
    if (desc === "false") return false
    return desc
  }

  // Handle arrays - filter out undefined values
  if (Array.isArray(obj)) {
    return obj.map(item => mod2(item)).filter(item => item !== undefined)
  }

  // Handle binary data (Buffer, Uint8Array, etc.)
  if (
    obj instanceof Uint8Array ||
    obj instanceof ArrayBuffer ||
    Buffer.isBuffer(obj)
  ) {
    // Convert to empty string for empty buffers
    const buffer = Buffer.isBuffer(obj) ? obj : Buffer.from(obj)
    return buffer.length === 0 ? "" : buffer.toString("base64")
  }

  // Handle objects - remove undefined properties
  if (typeof obj === "object" && obj !== null) {
    const result = {}
    for (const [key, value] of Object.entries(obj)) {
      const modifiedValue = mod2(value)
      // Only add the property if the value is not undefined
      if (modifiedValue !== undefined) {
        // Lowercase the key when creating the result object
        result[key.toLowerCase()] = modifiedValue
      }
    }
    return result
  }

  // Handle strings - check if it's already an atom-like string
  if (typeof obj === "string" && obj.match(/^[a-z_][a-zA-Z0-9_]*$/)) {
    // This looks like an atom value, keep as is
    return obj
  }

  // Return primitive values as-is (strings, numbers, booleans, null)
  return obj
}

// Helper to recursively remove ao-types fields without applying type conversions
const removeAoTypesField = obj => {
  if (obj === null || obj === undefined) return obj
  if (typeof obj !== "object") return obj
  if (Buffer.isBuffer(obj)) return obj
  if (Array.isArray(obj)) return obj.map(removeAoTypesField)

  const result = {}
  for (const [key, value] of Object.entries(obj)) {
    if (key !== "ao-types") {
      result[key] = removeAoTypesField(value)
    }
  }
  return result
}

const test = async (sign, cases, path, mod = v => v, pmod = v => v, skipAoTypes = false, removeAoTypes = false) => {
  let err = []
  let success = []
  let i = 0
  for (const v of cases) {
    console.log(`[${++i}]...........................................`, v)
    try {
      const _pmod = pmod(v)
      const json = erl_json_to(_pmod)
      const signed = await sign({ path, body: JSON.stringify(json) })
      const { out } = await send(signed)
      const input = normalize(_pmod)
      const output = erl_str_from(out)
      let expected = normalize(mod(_pmod), true)
      // Apply ao-types conversions to output (unless skipAoTypes is true for flat/structured codec tests)
      let output_b = skipAoTypes ? erl_str_from(out, true) : applyAoTypes(erl_str_from(out, true))
      // For tests that need ao-types removed but not converted (e.g., flat codec)
      // Apply the same transformation to both expected and actual for fair comparison
      if (removeAoTypes) {
        output_b = removeAoTypesField(output_b)
        expected = removeAoTypesField(expected)
      }
      // DEBUG: Print comparison on failure
      try {
        assert.deepEqual(expected, output_b)
      } catch (assertErr) {
        const stringify = (o) => JSON.stringify(o, (k,v) => {
          if (Buffer.isBuffer(v)) return v.toString('utf-8')
          if (v?.type === 'Buffer' && Array.isArray(v?.data)) return Buffer.from(v.data).toString('utf-8')
          if (typeof v === 'symbol') return `Symbol(${v.description})`
          return v
        }, 2)
        console.log("DEBUG expected:", stringify(expected))
        console.log("DEBUG actual:", stringify(output_b))
        throw assertErr
      }
      success.push(v)
    } catch (e) {
      console.log(e)
      err.push(v)
    }
  }
  console.log(`${err.length} / ${cases.length} failed!`)
  if (err.length > 0) {
    for (let v of err) console.log(v)
    throw new Error(`${err.length} test case(s) failed`)
  }
}

const genTest = ({ desc = "HyperBEAM", its = [] }) => {
  describe(desc, function () {
    let hbeam, sign
    before(async () => {
      hbeam = await new HyperBEAM({ reset: true, linkify_mode: false }).ready()
      sign = createSigner(hbeam.jwk, hbeam.url)
    })
    after(async () => hbeam.kill())
    for (const v of its) {
      const testFn = v.skip ? it.skip : it
      testFn(
        v.it ?? "should run",
        async () =>
          await test(
            sign,
            v.cases,
            v.path ?? "/~hbsig@1.0/json_to_erl",
            v.mod,
            v.pmod,
            v.skipAoTypes ?? false,
            v.removeAoTypes ?? false
          )
      )
    }
  })
}

// Recursive helper to apply ao-types conversions
const applyAoTypes = obj => {
  if (obj === null || obj === undefined) return obj
  if (typeof obj !== "object") return obj
  if (Buffer.isBuffer(obj)) return obj
  if (Array.isArray(obj)) return obj.map(applyAoTypes)

  // First, recursively process nested objects (to handle their ao-types)
  for (const key of Object.keys(obj)) {
    if (key !== "ao-types") {
      obj[key] = applyAoTypes(obj[key])
    }
  }


  // Process ao-types in this object
  let aoTypesRaw = obj["ao-types"]
  // Convert Buffer to string if needed
  const aoTypes =
    Buffer.isBuffer(aoTypesRaw) ? aoTypesRaw.toString() : aoTypesRaw

  // Check if ao-types looks like a structured field dictionary (contains key="value" patterns)
  // If it's just a user value like "test", we should preserve it
  const isAoTypesDictionary =
    aoTypes &&
    typeof aoTypes === "string" &&
    /[^=,\s]+="[^"]+"/g.test(aoTypes)

  if (isAoTypesDictionary) {
    const typeMatches = aoTypes.matchAll(/([^=,\s]+)="([^"]+)"/g)
    for (const match of typeMatches) {
      let key = match[1]
      // Handle dot (.) which means the object itself is a list
      if (key === ".") continue

      // URL-decode the key (e.g., data%46ield -> dataField)
      const decodedKey = decodeURIComponent(key)

      // Find the actual key in the object (case-insensitive match)
      const lowerKey = decodedKey.toLowerCase()
      const actualKey =
        Object.keys(obj).find(k => k.toLowerCase() === lowerKey) || lowerKey
      const type = match[2]
      const value = obj[actualKey]

      // Handle empty types (for keys that don't exist)
      if (!(actualKey in obj)) {
        if (type === "empty-binary") {
          obj[actualKey] = Buffer.from([])
        } else if (type === "empty-list") {
          obj[actualKey] = []
        } else if (type === "empty-message") {
          obj[actualKey] = {}
        }
      } else {
        // Convert existing values to their proper types
        // Handle both strings and Buffers
        const strValue = Buffer.isBuffer(value) ? value.toString() : value
        if (type === "integer" && typeof strValue === "string") {
          obj[actualKey] = parseInt(strValue, 10)
        } else if (type === "float" && typeof strValue === "string") {
          obj[actualKey] = parseFloat(strValue)
        } else if (type === "atom" && typeof strValue === "string") {
          if (strValue === "true") {
            obj[actualKey] = true
          } else if (strValue === "false") {
            obj[actualKey] = false
          } else if (strValue === "null") {
            obj[actualKey] = null
          } else if (strValue === "undefined") {
            obj[actualKey] = undefined
          } else {
            // General atoms (like "ok") become global Symbols
            obj[actualKey] = Symbol.for(strValue)
          }
        }
      }
    }

    // Check if the whole object should be converted to an array (. = "list")
    if (aoTypes.includes('.="list"')) {
      const keys = Object.keys(obj).filter(k => k !== "ao-types")
      const isArrayLike = keys.every(k => /^\d+$/.test(k))
      if (isArrayLike && keys.length > 0) {
        const arr = []
        const sortedKeys = keys.sort((a, b) => parseInt(a, 10) - parseInt(b, 10))
        for (const k of sortedKeys) {
          arr.push(obj[k])
        }
        return arr
      }
    }

    // Only delete ao-types if it was a real type annotation dictionary
    delete obj["ao-types"]
  }

  return obj
}

const modOut = out => {
  let output = erl_str_from(out)

  // Handle inline-body-key: rename 'body' to the key specified by inline-body-key
  // But only if the key is different from 'body' (otherwise we'd delete it)
  const inlineBodyKey = output["inline-body-key"]
  if (inlineBodyKey && inlineBodyKey !== "body" && output.body !== undefined) {
    output[inlineBodyKey] = output.body
    delete output.body
  }
  // Special case: inline-body-key: body with undefined body means empty body
  // This is how encode.js signals an empty body field
  if (inlineBodyKey === "body" && output.body === undefined) {
    output.body = Buffer.from([])
  }

  // Handle ao-body-key similarly
  const aoBodyKey = output["ao-body-key"]
  if (aoBodyKey && output.body !== undefined && !inlineBodyKey) {
    output[aoBodyKey] = output.body
    delete output.body
  }

  // Apply ao-types conversions recursively (handles type conversions and array reconstruction)
  output = applyAoTypes(output)

  // Delete TABM metadata fields
  delete output.commitments
  delete output.path
  delete output.method
  delete output["content-length"]
  delete output["content-type"]
  delete output["inline-body-key"]
  delete output["ao-body-key"]
  delete output["body-keys"]
  // Delete HTTP request headers that Erlang includes in the response
  delete output.accept
  delete output["accept-bundle"]
  delete output["accept-encoding"]
  delete output["accept-language"]
  delete output["sec-fetch-mode"]
  delete output["user-agent"]
  delete output.connection
  delete output.host
  delete output["content-digest"]
  delete output.signature
  delete output["signature-input"]
  // Note: We no longer delete empty body buffers because they may be intentional
  // (e.g., test case { body: Buffer.from([]) })
  return output
}
const modIn = inp => {
  let inp2 = normalize(inp)

  // Recursive function to lowercase all object keys and convert empty strings to Buffer
  const lowercaseKeys = (obj, isTopLevel = true) => {
    // Handle null/undefined
    if (obj === null || obj === undefined) {
      return obj
    }

    // Handle empty strings - convert to empty Buffer
    if (obj === "") {
      return Buffer.from([])
    }

    // Handle arrays - recurse on each element
    if (Array.isArray(obj)) {
      return obj.map(item => lowercaseKeys(item, false))
    }

    // Handle objects - lowercase keys and recurse on values
    if (typeof obj === "object" && obj.constructor === Object) {
      const result = {}
      for (const [key, value] of Object.entries(obj)) {
        // Lowercase the key and recurse on the value
        result[key.toLowerCase()] = lowercaseKeys(value, false)
      }

      // Note: The old data→body transformation is removed because with inline-body-key
      // support, the signer now preserves original key names correctly.

      return result
    }

    // Return other primitive values as-is
    return obj
  }

  return lowercaseKeys(inp2, true)
}
export { mod, mod2, test, genTest, modOut, modIn }
