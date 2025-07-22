/**
 * HyperBEAM Structured Format Codec
 * Implements encoding/decoding for TABM (Type-Annotated Binary Message) format
 */

export function structured_from(input) {
  const tabm = encodeToTabm(input)
  // Apply post-processing to convert buffers to strings where appropriate
  return postProcessTabm(tabm)
}

// Post-process TABM to convert buffers to strings for certain fields
function postProcessTabm(obj) {
  if (!obj || typeof obj !== "object" || Buffer.isBuffer(obj)) {
    return obj
  }

  const result = {}

  // Parse ao-types if present
  const types = parseTypes(obj["ao-types"] || "")

  for (const [key, value] of Object.entries(obj)) {
    const normKey = normalize(key)
    const type = types[normKey]

    if (Buffer.isBuffer(value)) {
      // Check if this buffer should be converted to a string
      // This matches the behavior of bin_to_str in erl_str.js
      if (shouldConvertToString(value)) {
        result[key] = value.toString("utf8")
      } else {
        result[key] = value
      }
    } else if (
      typeof value === "object" &&
      value !== null &&
      !Buffer.isBuffer(value)
    ) {
      // Check if this is a numbered map that should be converted to array
      if (type === "list" && isNumberedMap(value)) {
        result[key] = numberedMapToArray(postProcessTabm(value))
      } else {
        // Recursively process nested objects
        result[key] = postProcessTabm(value)
      }
    } else {
      result[key] = value
    }
  }

  return result
}

// Check if an object is a numbered map (has keys like "0", "1", "2", etc.)
function isNumberedMap(obj) {
  const keys = Object.keys(obj)
  return keys.some(k => /^\d+$/.test(k))
}

// Check if buffer should be converted to string (matching bin_to_str logic)
function shouldConvertToString(buffer) {
  if (!Buffer.isBuffer(buffer) || buffer.length === 0) {
    return false
  }

  try {
    const str = buffer.toString("utf8")
    // Check if it's valid UTF-8 by seeing if it round-trips correctly
    if (!Buffer.from(str, "utf8").equals(buffer)) {
      return false
    }

    // Check if all characters are printable or common whitespace
    for (let i = 0; i < str.length; i++) {
      const code = str.charCodeAt(i)
      // Allow printable ASCII (32-126) and common whitespace (tab, newline, carriage return)
      if (
        !(code >= 32 && code <= 126) &&
        code !== 9 &&
        code !== 10 &&
        code !== 13
      ) {
        return false
      }
    }

    return true
  } catch (e) {
    return false
  }
}

export function structured_to(input) {
  return decodeFromTabm(input)
}

// Main encoding function
function encodeToTabm(obj) {
  // Handle primitives directly
  if (obj === null || obj === undefined) {
    return obj
  }

  // Handle Buffers - they pass through unchanged
  if (Buffer.isBuffer(obj)) {
    return obj
  }

  // Handle non-objects (strings, numbers, etc)
  if (typeof obj !== "object") {
    return obj
  }

  const result = {}
  const types = []

  // Special handling for objects that already have ao-types
  // This means they're already in TABM format, so we should preserve their structure
  const hasAoTypes = obj["ao-types"] !== undefined

  // Sort keys by normalized form for consistent output with Erlang
  const keys = Object.keys(obj).sort((a, b) => {
    const normA = normalize(a)
    const normB = normalize(b)
    if (normA < normB) return -1
    if (normA > normB) return 1
    return 0
  })

  for (const key of keys) {
    const value = obj[key]
    const normKey = normalize(key)

    // If this object already has ao-types, just pass through the values
    if (hasAoTypes) {
      if (
        typeof value === "object" &&
        value !== null &&
        !Buffer.isBuffer(value)
      ) {
        result[key] = encodeToTabm(value)
      } else {
        result[key] = value
      }
      continue
    }

    // Handle empty values
    if (value === "" || (Buffer.isBuffer(value) && value.length === 0)) {
      types.push([normKey, "empty-binary"])
    } else if (Array.isArray(value) && value.length === 0) {
      types.push([normKey, "empty-list"])
    } else if (isEmptyObject(value)) {
      types.push([normKey, "empty-message"])
    }
    // Handle $empty objects
    else if (value && typeof value === "object" && value["$empty"]) {
      if (value["$empty"] === "binary") {
        types.push([normKey, "empty-binary"])
      } else if (value["$empty"] === "list") {
        types.push([normKey, "empty-list"])
      } else if (value["$empty"] === "map" || value["$empty"] === "message") {
        types.push([normKey, "empty-message"])
      }
    }
    // Handle arrays
    else if (Array.isArray(value)) {
      const arrayResult = processArray(value)
      types.push([normKey, arrayResult.type])
      result[normKey] = arrayResult.value
    }
    // Handle primitives
    else if (typeof value === "string") {
      // IMPORTANT: Strings are kept as strings at the top level in TABM format
      // They only become binaries when moved to a numbered map (inside arrays)
      result[normKey] = value
    } else if (Buffer.isBuffer(value)) {
      // Buffers are kept as-is in the TABM format
      result[normKey] = value
    } else if (typeof value === "number") {
      if (Number.isInteger(value)) {
        types.push([normKey, "integer"])
        result[normKey] = String(value)
      } else {
        types.push([normKey, "float"])
        // Floats are stored as buffers at top level
        result[normKey] = Buffer.from(formatFloat(value))
      }
    } else if (typeof value === "boolean") {
      types.push([normKey, "atom"])
      result[normKey] = `"${value}"`
    } else if (value === null) {
      types.push([normKey, "atom"])
      result[normKey] = '"null"'
    } else if (typeof value === "symbol") {
      types.push([normKey, "atom"])
      const name =
        Symbol.keyFor(value) ||
        value.description ||
        value.toString().slice(7, -1)
      result[normKey] = `"${name}"`
    }
    // Handle nested objects
    else if (typeof value === "object") {
      result[normKey] = encodeToTabm(value)
    }
  }

  // Add ao-types if we have types and don't already have ao-types
  if (types.length > 0 && !hasAoTypes) {
    result["ao-types"] = types.map(([k, v]) => `${k}="${v}"`).join(", ")
  }

  return result
}

// Process arrays - convert to numbered map if needed
function processArray(arr) {
  // Pre-process $empty objects
  const processed = arr.map(item => {
    if (item && typeof item === "object" && item["$empty"]) {
      if (item["$empty"] === "binary") return Buffer.alloc(0)
      if (item["$empty"] === "list") return []
      if (item["$empty"] === "map" || item["$empty"] === "message") return {}
    }
    return item
  })

  // Check if we need numbered map (first element is array/map OR any element is a map)
  const needsNumbered = shouldUseNumberedMap(processed)

  if (!needsNumbered) {
    // Encode as structured field list
    return { type: "list", value: encodeList(processed) }
  }

  // Convert to numbered map
  const numbered = {}
  const types = []

  processed.forEach((item, idx) => {
    const key = String(idx + 1)

    // Empty values - only add type
    if (item === "" || (Buffer.isBuffer(item) && item.length === 0)) {
      types.push([key, "empty-binary"])
    } else if (Array.isArray(item) && item.length === 0) {
      types.push([key, "empty-list"])
    } else if (isEmptyObject(item)) {
      types.push([key, "empty-message"])
    }
    // Strings become buffers in numbered maps
    else if (typeof item === "string") {
      numbered[key] = Buffer.from(item)
    }
    // Buffers stay as buffers
    else if (Buffer.isBuffer(item)) {
      numbered[key] = item
    }
    // Arrays - recursive processing
    else if (Array.isArray(item)) {
      const subResult = processArray(item)
      types.push([key, subResult.type])
      numbered[key] = subResult.value
    }
    // Numbers
    else if (typeof item === "number") {
      if (Number.isInteger(item)) {
        types.push([key, "integer"])
        numbered[key] = Buffer.from(String(item))
      } else {
        types.push([key, "float"])
        numbered[key] = Buffer.from(formatFloat(item))
      }
    }
    // Booleans
    else if (typeof item === "boolean") {
      types.push([key, "atom"])
      numbered[key] = Buffer.from(`"${item}"`)
    }
    // Null
    else if (item === null) {
      types.push([key, "atom"])
      numbered[key] = Buffer.from('"null"')
    }
    // Objects
    else if (typeof item === "object") {
      numbered[key] = encodeToTabm(item)
    }
  })

  // Add ao-types
  if (types.length > 0) {
    numbered["ao-types"] = types.map(([k, v]) => `${k}="${v}"`).join(", ")
  }

  return { type: "list", value: numbered }
}

// Check if array needs to be converted to numbered map
function shouldUseNumberedMap(arr) {
  if (arr.length === 0) return false

  // If first element is array or map -> numbered map
  const first = arr[0]
  if (Array.isArray(first)) return true
  if (first && typeof first === "object" && !Buffer.isBuffer(first)) return true

  // Check if any element is a map
  return arr.some(
    item =>
      item &&
      typeof item === "object" &&
      !Buffer.isBuffer(item) &&
      !Array.isArray(item)
  )
}

// Encode array as structured field list
function encodeList(arr) {
  const items = arr.map(item => {
    // Empty values
    if (item === "" || (Buffer.isBuffer(item) && item.length === 0)) {
      return '""'
    }
    // Strings
    if (typeof item === "string") {
      return `"${escapeString(item)}"`
    }
    // Buffers
    if (Buffer.isBuffer(item)) {
      return `"${escapeString(item.toString())}"`
    }
    // Arrays
    if (Array.isArray(item)) {
      if (item.length === 0) return "[]"
      // Check if nested array can be encoded
      if (shouldUseNumberedMap(item)) {
        throw new Error(
          "Cannot encode nested array with maps as structured field"
        )
      }
      return encodeNestedList(item, 1)
    }
    // Numbers
    if (typeof item === "number") {
      if (Number.isInteger(item)) {
        return `"(ao-type-integer) ${item}"`
      } else {
        return `"(ao-type-float) ${formatFloat(item)}"`
      }
    }
    // Booleans
    if (typeof item === "boolean") {
      return `"(ao-type-atom) \\"${item}\\""`
    }
    // Null
    if (item === null) {
      return '"(ao-type-atom) \\"null\\""'
    }
    // Symbols
    if (typeof item === "symbol") {
      const name =
        Symbol.keyFor(item) || item.description || item.toString().slice(7, -1)
      return `"(ao-type-atom) \\"${escapeString(name)}\\""`
    }

    throw new Error(`Cannot encode item of type ${typeof item} in list`)
  })

  return items.join(", ")
}

// Encode nested lists with proper escaping
function encodeNestedList(arr, level) {
  const bs = "\\".repeat(Math.pow(2, level) - 1)

  const items = arr.map(item => {
    if (typeof item === "string") {
      return `${bs}"${escapeString(item)}${bs}"`
    }
    if (typeof item === "number") {
      if (Number.isInteger(item)) {
        return `${bs}"(ao-type-integer) ${item}${bs}"`
      } else {
        return `${bs}"(ao-type-float) ${formatFloat(item)}${bs}"`
      }
    }
    if (Array.isArray(item)) {
      return encodeNestedList(item, level + 1)
    }
    if (typeof item === "boolean") {
      const innerBs = "\\".repeat(Math.pow(2, level + 1) - 1)
      return `${bs}"(ao-type-atom) ${innerBs}"${item}${innerBs}"${bs}"`
    }
    if (item === null) {
      const innerBs = "\\".repeat(Math.pow(2, level + 1) - 1)
      return `${bs}"(ao-type-atom) ${innerBs}"null${innerBs}"${bs}"`
    }
    return `${bs}"${String(item)}${bs}"`
  })

  const content = items.join(", ")

  if (level === 1) {
    return `"(ao-type-list) ${content}"`
  } else {
    const outerBs = "\\".repeat(Math.pow(2, level - 1) - 1)
    return `${outerBs}"(ao-type-list) ${content}${outerBs}"`
  }
}

// Decode from TABM format
function decodeFromTabm(obj) {
  if (!obj || typeof obj !== "object" || Buffer.isBuffer(obj)) {
    return obj
  }

  const result = {}
  const types = parseTypes(obj["ao-types"] || "")

  for (const [key, value] of Object.entries(obj)) {
    if (key === "ao-types") continue

    const normKey = normalize(key)
    const type = types[normKey]

    // Handle empty types
    if (!result[key] && type) {
      if (type === "empty-binary") {
        result[key] = ""
      } else if (type === "empty-list") {
        result[key] = []
      } else if (type === "empty-message") {
        result[key] = {}
      }
    }

    // Decode values
    if (typeof value === "string" && type) {
      result[key] = decodeValue(type, value)
    } else if (typeof value === "object" && !Buffer.isBuffer(value)) {
      const decoded = decodeFromTabm(value)
      // Convert numbered maps back to arrays
      if (type === "list") {
        result[key] = numberedMapToArray(decoded)
      } else {
        result[key] = decoded
      }
    } else {
      result[key] = value
    }
  }

  return result
}

// Helper functions
function normalize(key) {
  return String(key).toLowerCase()
}

function isEmptyObject(obj) {
  return (
    obj &&
    typeof obj === "object" &&
    !Array.isArray(obj) &&
    !Buffer.isBuffer(obj) &&
    Object.keys(obj).length === 0
  )
}

function formatFloat(num) {
  let str = num.toExponential(20)
  // Ensure 2-digit exponent
  return str.replace(/e([+-])(\d)$/, "e$10$2")
}

function escapeString(str) {
  return str.replace(/\\/g, "\\\\").replace(/"/g, '\\"')
}

function parseTypes(typeStr) {
  const types = {}
  if (!typeStr) return types

  // Convert Buffer to string if needed
  const str = Buffer.isBuffer(typeStr) ? typeStr.toString("utf8") : typeStr

  const matches = str.matchAll(/(\w+)="([^"]+)"/g)
  for (const match of matches) {
    types[match[1]] = match[2]
  }
  return types
}

function decodeValue(type, value) {
  switch (type) {
    case "integer":
      return parseInt(value, 10)
    case "float":
      return parseFloat(value)
    case "atom":
      if (value === '"true"') return true
      if (value === '"false"') return false
      if (value === '"null"') return null
      return value.slice(1, -1)
    case "list":
      return parseList(value)
    default:
      return value
  }
}

function parseList(value) {
  if (!value) return []

  const items = []
  let current = ""
  let depth = 0
  let inString = false

  for (let i = 0; i < value.length; i++) {
    const char = value[i]

    if (char === '"' && value[i - 1] !== "\\") {
      inString = !inString
    }

    if (!inString) {
      if (char === "," && depth === 0) {
        items.push(parseListItem(current.trim()))
        current = ""
        continue
      }
    }

    current += char
  }

  if (current.trim()) {
    items.push(parseListItem(current.trim()))
  }

  return items
}

function parseListItem(item) {
  if (!item.startsWith('"') || !item.endsWith('"')) {
    return item
  }

  const content = item.slice(1, -1)

  // Handle typed values
  const typeMatch = content.match(/^\(ao-type-(\w+)\) (.+)$/)
  if (typeMatch) {
    const [, type, val] = typeMatch

    if (type === "integer") return parseInt(val, 10)
    if (type === "float") return parseFloat(val)
    if (type === "atom") {
      const atomVal = val.replace(/\\"/g, '"')
      if (atomVal === '"true"') return true
      if (atomVal === '"false"') return false
      if (atomVal === '"null"') return null
      return atomVal.slice(1, -1)
    }
    if (type === "list") {
      return parseList(val)
    }
  }

  // Unescape string
  return content.replace(/\\"/g, '"').replace(/\\\\/g, "\\")
}

function numberedMapToArray(map) {
  const keys = Object.keys(map)
    .filter(k => /^\d+$/.test(k))
    .map(k => parseInt(k, 10))
    .sort((a, b) => a - b)

  if (keys.length === 0) {
    return []
  }

  const arr = []

  // Check if we have 0-based or 1-based indexing
  const isZeroBased = keys[0] === 0

  if (isZeroBased) {
    // 0-based indexing
    const maxIdx = Math.max(...keys)
    for (let i = 0; i <= maxIdx; i++) {
      if (map[String(i)] !== undefined) {
        arr[i] = map[String(i)]
      }
    }
  } else {
    // 1-based indexing (default for structured fields)
    const maxIdx = Math.max(...keys)
    for (let i = 1; i <= maxIdx; i++) {
      if (map[String(i)] !== undefined) {
        arr[i - 1] = map[String(i)]
      }
    }
  }

  return arr
}
