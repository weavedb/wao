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

  // Add empty values based on their types (these may be overwritten if data exists)
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
      // Check if the child object itself indicates it's a list via .="list" in its ao-types
      const childAoTypes = value["ao-types"] || ""
      const childTypes = parseAoTypes(childAoTypes)
      const isChildList = childTypes["."] === "list"

      // Recursively decode child TABM
      const childDecoded = to(value)

      // Only convert numbered map to array if the child object itself
      // declares it's a list via .="list" in its ao-types.
      // This preserves original keys when the parent declares the type
      // but the child doesn't have the list marker.
      if (isChildList) {
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

    case "boolean":
      // SF boolean format: ?1 = true, ?0 = false
      // Convert to native boolean, will be encoded as "atom" type
      if (value === "?1") return true
      if (value === "?0") return false
      // Fallback for other formats
      return value === "true" || value === "1"

    case "atom":
      const atomItem = parseStructuredItem(value)
      const atomName = atomItem.replace(/^"|"$/g, "") // Remove quotes
      // Convert to Symbol to preserve atom type through round-trip
      // Special cases for common atoms that JS has native types for
      if (atomName === "true") return true
      if (atomName === "false") return false
      if (atomName === "null") return null
      return Symbol.for(atomName)

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
  // Handle non-string values (e.g., numbers from HyperBEAM responses)
  if (typeof value !== "string") return String(value)
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
  if (typeof value !== "string") return [value]
  return value.split(", ").map(item => {
    if (item.startsWith('"') && item.endsWith('"')) {
      // Remove quotes and unescape SF string escapes
      // In SF strings: \" = " and \\ = \
      return item.slice(1, -1)
        .replace(/\\"/g, '"')     // \" -> "
        .replace(/\\\\/g, '\\')   // \\ -> \
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
  // Handle binary input - passthrough
  if (msg instanceof Buffer || msg instanceof Uint8Array) {
    return msg
  }

  // Handle non-object values - passthrough
  if (typeof msg !== "object" || msg === null) {
    return msg
  }

  // Handle arrays - convert to numbered map with .="list" in ao-types
  // Mirrors Erlang: from(List, Req, Opts) when is_list(List)
  if (Array.isArray(msg)) {
    // Convert to numbered map (1-based indexing like Erlang)
    const numberedMap = {}
    msg.forEach((item, idx) => {
      numberedMap[(idx + 1).toString()] = item
    })

    // Recursively process the numbered map
    const result = from(numberedMap)

    // Add .="list" to ao-types to indicate this message is a list
    const existingAoTypes = result["ao-types"] || ""
    if (existingAoTypes) {
      result["ao-types"] = '.="list", ' + existingAoTypes
    } else {
      result["ao-types"] = '.="list"'
    }

    return result
  }

  // Process keys - preserve original case to match Erlang behavior
  // HTTP headers are case-insensitive but JSON/map keys preserve case
  const keysMap = {}
  for (const [key, value] of Object.entries(msg)) {
    keysMap[key] = value
  }

  // Get sorted keys (preserving case)
  const sortedKeys = Object.keys(keysMap).sort()

  const types = []
  const values = []

  // Process each key in sorted order
  for (const key of sortedKeys) {
    const value = keysMap[key]

    // Handle empty binaries/strings - just include as-is, no type annotation
    // (Erlang doesn't add empty-binary type, it just keeps the empty binary)
    if (value === "" || (value instanceof Buffer && value.length === 0)) {
      values.push([key, value])
      continue
    }

    // Empty arrays - convert to numbered map with .="list" in ao-types
    // (Erlang doesn't add empty-list type, just the list marker)
    if (Array.isArray(value) && value.length === 0) {
      values.push([key, from(value)])
      continue
    }

    // Empty objects - just include as-is, no type annotation
    // (Erlang doesn't add empty-message type, it just keeps the empty map)
    if (
      typeof value === "object" &&
      value !== null &&
      !Array.isArray(value) &&
      !(value instanceof Buffer) &&
      Object.keys(value).length === 0
    ) {
      values.push([key, value])
      continue
    }

    // Handle binary/string values
    if (value instanceof Buffer || value instanceof Uint8Array) {
      values.push([key, value])
      continue
    }

    if (typeof value === "string") {
      values.push([key, value])
      continue
    }

    // Handle nested maps
    if (typeof value === "object" && !Array.isArray(value) && value !== null) {
      values.push([key, from(value)])
      continue
    }

    // Handle arrays - from() converts to numbered map with .="list" in ao-types
    if (Array.isArray(value) && value.length > 0) {
      values.push([key, from(value)])
      continue
    }

    // Handle typed values (need encoding)
    if (
      typeof value === "symbol" ||
      typeof value === "number" ||
      typeof value === "boolean" ||
      value === null
    ) {
      const [type, encoded] = encodeValue(value)
      types.push([key, type])
      values.push([key, encoded])
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
  // Null (as atom) - use token format (unquoted)
  if (value === null) {
    return ["atom", "null"]
  }

  // Integer
  if (typeof value === "number" && Number.isInteger(value)) {
    return ["integer", value.toString()]
  }

  // Float
  if (typeof value === "number") {
    // Format like Erlang's float_to_binary - scientific notation with full precision
    // Erlang's float_to_binary/1 uses ~20 decimal digits and keeps trailing zeros
    let str = value.toExponential(20)
    // Ensure 2-digit exponent with sign
    str = str.replace(/e([+-])(\d)$/, "e$10$2")
    return ["float", str]
  }

  // Boolean (as atom) - use token format (unquoted)
  if (typeof value === "boolean") {
    return ["atom", value.toString()]
  }

  // Symbol (as atom) - use token format (unquoted)
  if (typeof value === "symbol") {
    const name = Symbol.keyFor(value) || value.description || ""
    return ["atom", name]
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
