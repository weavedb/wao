/**
 * Structured field codec for JavaScript-Erlang interoperability
 * Implements the same behavior as dev_codec_structured.erl
 */

import { erl_str_from } from "./erl_str.js"

/**
 * Convert from structured format
 * @param {string|object} input - Erlang term string or JavaScript object
 * @returns {object} - Processed object
 */
export function structured_from(input) {
  // If input is a string (Erlang response), parse it
  if (typeof input === "string") {
    return erl_str_from(input, false)
  }

  // Otherwise process the object like Erlang's from/1
  return from(input)
}

/**
 * Convert to structured format
 * @param {*} obj - JavaScript object
 * @returns {*} - Structured format object
 */
export function structured_to(obj) {
  // TODO: Implement the inverse of structured_from
  return obj
}

/**
 * Check if an array should be converted to numbered map
 * Rules based on Erlang behavior:
 * 1. Contains any objects/maps → convert
 * 2. Contains empty arrays (but NOT empty buffers) → convert
 * 3. All items are arrays (array of arrays) → convert
 * 4. Otherwise → encode as string
 */
function shouldConvertToNumberedMap(arr) {
  let allArrays = true
  let hasObjects = false
  let hasEmptyArrays = false

  for (const item of arr) {
    // Check for objects (not arrays or buffers)
    if (
      typeof item === "object" &&
      item !== null &&
      !Array.isArray(item) &&
      !Buffer.isBuffer(item)
    ) {
      hasObjects = true
    }
    // Check for empty arrays only (NOT empty buffers)
    else if (Array.isArray(item) && item.length === 0) {
      hasEmptyArrays = true
    }
    // Track if all items are arrays
    else if (!Array.isArray(item)) {
      allArrays = false
    }
  }

  // Convert if: has objects, has empty arrays, or all items are non-empty arrays
  return (
    hasObjects ||
    hasEmptyArrays ||
    (allArrays && arr.length > 0 && arr.every(item => Array.isArray(item)))
  )
}

/**
 * Implementation of Erlang's dev_codec_structured:from/1
 */
function from(msg) {
  // Handle non-map values
  if (
    msg instanceof Buffer ||
    typeof msg !== "object" ||
    msg === null ||
    Array.isArray(msg)
  ) {
    return msg
  }

  // Normalize keys first
  const normalizedMap = {}
  for (const [key, value] of Object.entries(msg)) {
    const normKey = key.toLowerCase()
    normalizedMap[normKey] = value
  }

  // Get sorted keys (normalized)
  const sortedKeys = Object.keys(normalizedMap).sort()

  const types = []
  const values = []

  // Process each key in sorted order
  for (const normKey of sortedKeys) {
    const value = normalizedMap[normKey]

    // Handle empty values
    if (value === "" || (value instanceof Buffer && value.length === 0)) {
      types.push([normKey, "empty-binary"])
      continue
    }

    if (Array.isArray(value) && value.length === 0) {
      types.push([normKey, "empty-list"])
      continue
    }

    if (
      typeof value === "object" &&
      value !== null &&
      !Array.isArray(value) &&
      !(value instanceof Buffer) &&
      Object.keys(value).length === 0
    ) {
      types.push([normKey, "empty-message"])
      continue
    }

    // Handle binary/string values
    if (value instanceof Buffer || value instanceof Uint8Array) {
      // Keep buffers as buffers
      values.push([normKey, value])
      continue
    }

    if (typeof value === "string") {
      // Keep strings as strings
      values.push([normKey, value])
      continue
    }

    // Handle nested maps
    if (typeof value === "object" && !Array.isArray(value) && value !== null) {
      values.push([normKey, from(value)])
      continue
    }

    // Handle arrays
    if (Array.isArray(value) && value.length > 0) {
      if (shouldConvertToNumberedMap(value)) {
        // Convert to numbered map (1-based indexing)
        const numberedMap = {}
        value.forEach((item, idx) => {
          numberedMap[(idx + 1).toString()] = item
        })
        types.push([normKey, "list"])
        values.push([normKey, from(numberedMap)])
      } else {
        // Encode as list string
        const [type, encoded] = encodeValue(value)
        types.push([normKey, type])
        values.push([normKey, encoded])
      }
      continue
    }

    // Handle typed values (need encoding)
    if (
      typeof value === "symbol" ||
      typeof value === "number" ||
      Array.isArray(value) ||
      typeof value === "boolean" ||
      value === null
    ) {
      const [type, encoded] = encodeValue(value)
      types.push([normKey, type])
      values.push([normKey, encoded])
      continue
    }
  }

  // Build result
  const result = {}

  // Add ao-types if present
  if (types.length > 0) {
    result["ao-types"] = types.map(([k, t]) => `${k}="${t}"`).join(", ")
  }

  // Add values (but NOT empty values)
  for (const [k, v] of values) {
    result[k] = v
  }

  return result
}

/**
 * Encode a value with its type
 */
function encodeValue(value) {
  // Null (as atom)
  if (value === null) {
    return ["atom", '"null"']
  }

  // Integer
  if (typeof value === "number" && Number.isInteger(value)) {
    return ["integer", value.toString()]
  }

  // Float
  if (typeof value === "number") {
    // Format like Erlang with scientific notation
    let str = value.toExponential(20)
    // Remove trailing zeros but keep at least one
    str = str.replace(/(\.\d*?)0+e/, "$1e").replace(/\.e/, ".0e")
    // Ensure 2-digit exponent
    str = str.replace(/e([+-])(\d)$/, "e$10$2")
    return ["float", str]
  }

  // Boolean (as atom)
  if (typeof value === "boolean") {
    return ["atom", `"${value}"`]
  }

  // Symbol (as atom)
  if (typeof value === "symbol") {
    const name = Symbol.keyFor(value) || value.description || ""
    return ["atom", `"${name}"`]
  }

  // List
  if (Array.isArray(value)) {
    const parts = []

    for (const item of value) {
      if (item instanceof Buffer) {
        // Empty buffer => empty string
        parts.push(item.length === 0 ? '""' : `"${item.toString()}"`)
      } else if (typeof item === "string") {
        parts.push(`"${item}"`)
      } else {
        const [itemType, itemEncoded] = encodeValue(item)

        if (itemType === "list") {
          // Escape nested list quotes
          const escaped = itemEncoded
            .replace(/\\/g, "\\\\")
            .replace(/"/g, '\\"')
          parts.push(`"(ao-type-list) ${escaped}"`)
        } else if (itemType === "atom") {
          // Escape atom quotes
          const escaped = itemEncoded.replace(/"/g, '\\"')
          parts.push(`"(ao-type-atom) ${escaped}"`)
        } else {
          parts.push(`"(ao-type-${itemType}) ${itemEncoded}"`)
        }
      }
    }

    return ["list", parts.join(", ")]
  }

  if (value instanceof Buffer) {
    return ["binary", value]
  }

  return ["unknown", String(value)]
}
