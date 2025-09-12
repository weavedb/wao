/**
 * Structured field codec for JavaScript-Erlang interoperability
 * Implements the same behavior as dev_codec_structured.erl
 */

import { erl_str_from } from "./erl_str.js"

/**
 * Convert from structured format (rich message to TABM)
 * Mirrors Erlang's from/1 function
 * @param {*} obj - Rich message object
 * @returns {*} - TABM object
 */
export function structured_from(obj) {
  // Handle binary input
  if (
    typeof obj === "string" ||
    obj instanceof Buffer ||
    obj instanceof Uint8Array
  ) {
    return obj
  }

  // Handle non-object input
  if (typeof obj !== "object" || obj === null || Array.isArray(obj)) {
    return obj
  }

  // Convert rich message to TABM
  return from(obj)
}

/**
 * Convert to structured format (TABM to rich message)
 * Mirrors Erlang's to/1 function
 * @param {string|object} input - Erlang term string or TABM object
 * @returns {object} - Rich message object
 */
export function structured_to(input) {
  // If input is a string (Erlang response), parse it
  if (typeof input === "string") {
    return erl_str_from(input, false)
  }

  // Otherwise convert TABM to rich message
  return to(input)
}

/**
 * Convert a TABM into a rich message (mirrors Erlang's to/1)
 * @param {*} tabm - Type-Annotated-Binary-Message
 * @returns {*} - Rich message
 */
function to(tabm) {
  // Handle binary input
  if (
    typeof tabm === "string" ||
    tabm instanceof Buffer ||
    tabm instanceof Uint8Array
  ) {
    return tabm
  }

  // Handle non-object input
  if (typeof tabm !== "object" || tabm === null || Array.isArray(tabm)) {
    return tabm
  }

  // Parse ao-types if present
  const aoTypesStr = tabm["ao-types"] || ""
  const types = parseAoTypes(aoTypesStr)

  // Build result with empty values first
  const result = {}

  // Add empty values based on their types
  for (const [key, type] of Object.entries(types)) {
    if (type === "empty-binary") {
      result[key] = ""
    } else if (type === "empty-list") {
      result[key] = []
    } else if (type === "empty-message") {
      result[key] = {}
    }
  }

  // Process all other key-value pairs
  for (const [rawKey, value] of Object.entries(tabm)) {
    // Skip ao-types field
    if (rawKey === "ao-types") {
      continue
    }

    const normalizedKey = rawKey.toLowerCase()

    if (
      typeof value === "string" ||
      value instanceof Buffer ||
      value instanceof Uint8Array
    ) {
      const type = types[normalizedKey]
      if (type) {
        // Decode according to type
        result[rawKey] = decodeValue(type, value)
      } else {
        // No type info, keep as binary/string
        result[rawKey] = value
      }
    } else if (
      typeof value === "object" &&
      value !== null &&
      !Array.isArray(value)
    ) {
      // Recursively decode child TABM
      const childDecoded = to(value)
      const type = types[normalizedKey]

      if (type === "list") {
        // Convert numbered map back to ordered list
        result[rawKey] = messageToOrderedList(childDecoded)
      } else {
        result[rawKey] = childDecoded
      }
    } else {
      // Value already has converted type
      result[rawKey] = value
    }
  }

  return filterDefaultKeys(result)
}

/**
 * Parse ao-types field and return map of keys to types
 * @param {string} aoTypesStr - ao-types field content
 * @returns {object} - Map of normalized keys to types
 */
function parseAoTypes(aoTypesStr) {
  if (!aoTypesStr) {
    return {}
  }

  const types = {}

  // Simple parser for "key1=\"type1\", key2=\"type2\"" format
  const pairs = aoTypesStr.split(", ")

  for (const pair of pairs) {
    const match = pair.match(/^(.+?)="(.+?)"$/)
    if (match) {
      const [, key, type] = match
      // Decode escaped key and normalize
      const decodedKey = decodeEscapedKey(key)
      types[decodedKey.toLowerCase()] = type
    }
  }

  return types
}

/**
 * Decode escaped key (simplified version)
 * @param {string} key - Escaped key
 * @returns {string} - Decoded key
 */
function decodeEscapedKey(key) {
  // This is a simplified decoder - in practice you'd want more robust escaping
  return key.replace(/\\"/g, '"').replace(/\\\\/g, "\\")
}

/**
 * Convert numbered map back to ordered list
 * @param {object} numberedMap - Map with numeric string keys
 * @returns {Array} - Ordered array
 */
function messageToOrderedList(numberedMap) {
  const keys = Object.keys(numberedMap)
    .filter(key => /^\d+$/.test(key))
    .map(key => parseInt(key, 10))
    .sort((a, b) => a - b)

  return keys.map(key => numberedMap[key.toString()])
}

/**
 * Filter default keys (simplified - removes common defaults)
 * @param {object} message - Message object
 * @returns {object} - Filtered message
 */
function filterDefaultKeys(message) {
  // This is a placeholder - implement based on your needs
  return message
}

/**
 * Decode a value based on its type
 * @param {string} type - Type identifier
 * @param {*} value - Encoded value
 * @returns {*} - Decoded value
 */
function decodeValue(type, value) {
  switch (type.toLowerCase()) {
    case "integer":
      return parseInt(parseStructuredItem(value), 10)

    case "float":
      return parseFloat(value)

    case "atom":
      const atomItem = parseStructuredItem(value)
      return atomItem.replace(/^"|"$/g, "") // Remove quotes

    case "list":
      return parseStructuredList(value).map(item => {
        if (typeof item === "string" && item.startsWith("(ao-type-")) {
          const match = item.match(/^\(ao-type-(.+?)\) (.+)$/)
          if (match) {
            const [, itemType, itemValue] = match
            return decodeValue(itemType, itemValue)
          }
        }
        return item
      })

    case "binary":
      return value

    default:
      return value
  }
}

/**
 * Parse structured field item (simplified)
 * @param {string} value - Structured field value
 * @returns {*} - Parsed value
 */
function parseStructuredItem(value) {
  // This is a simplified parser - you'd want to use a proper structured fields parser
  if (value.startsWith('"') && value.endsWith('"')) {
    return value.slice(1, -1) // Remove quotes
  }
  return value
}

/**
 * Parse structured field list (simplified)
 * @param {string} value - Structured field list
 * @returns {Array} - Parsed list
 */
function parseStructuredList(value) {
  // This is a simplified parser - you'd want to use a proper structured fields parser
  return value.split(", ").map(item => {
    if (item.startsWith('"') && item.endsWith('"')) {
      return item.slice(1, -1) // Remove quotes
    }
    return item
  })
}

/**
 * Convert rich message to TABM (mirrors Erlang's from/1)
 * @param {object} msg - Rich message
 * @returns {object} - TABM
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
      values.push([normKey, value])
      continue
    }

    if (typeof value === "string") {
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
