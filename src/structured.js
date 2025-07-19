/**
 * A codec implementing the structured format for HyperBEAM's internal,
 * richly typed message format.
 *
 * This format mirrors HTTP Structured Fields (RFC-9651).
 */

/**
 * Convert to structured format (TABM).
 * This mimics the server's structured_from endpoint behavior.
 * Despite the name, this converts TO structured format, not FROM it.
 * @param {Object} input - The native JS object
 * @returns {Object} - TABM format
 */
export function structured_from(input) {
  // The server's structured_from uses dev_codec_structured:from
  // which converts TO structured format (TABM)
  return nativeToTabm(input)
}

/**
 * Convert from structured format (TABM) to native.
 * This mimics the server's structured_to endpoint behavior.
 * Despite the name, this converts FROM structured format, not TO it.
 * @param {Object} input - The TABM format
 * @returns {Object} - Native JS object
 */
export function structured_to(input) {
  // The server's structured_to uses dev_codec_structured:to
  // which converts FROM structured format to native
  return tabmToNative(input)
}

/**
 * Internal function to convert TABM to native JS object
 */
function tabmToNative(tabm) {
  if (typeof tabm === "string" || tabm instanceof Buffer) {
    return tabm
  }

  if (typeof tabm !== "object" || tabm === null) {
    return tabm
  }

  // Parse ao-types field
  const types = parseAoTypes(tabm["ao-types"] || "")

  // First, handle empty values based on types
  const result = {}
  for (const [key, type] of Object.entries(types)) {
    if (type === "empty-binary") {
      result[key] = ""
    } else if (type === "empty-list") {
      result[key] = []
    } else if (type === "empty-message") {
      result[key] = {}
    }
  }

  // Then process all other fields
  for (const [key, value] of Object.entries(tabm)) {
    if (key === "ao-types") continue

    const normalizedKey = normalizeKey(key)
    const type = types[normalizedKey]

    if (typeof value === "string") {
      if (!type) {
        // No type annotation, it's just a string
        result[key] = value
      } else {
        // Decode according to type
        result[key] = decodeValue(type, value)
      }
    } else if (
      typeof value === "object" &&
      value !== null &&
      !Array.isArray(value)
    ) {
      // Recursively decode child TABM
      const childDecoded = tabmToNative(value)
      if (type === "list") {
        // Convert numbered map back to array
        // First get all numeric keys
        const numericKeys = Object.keys(childDecoded)
          .filter(k => /^\d+$/.test(k))
          .sort((a, b) => parseInt(a) - parseInt(b))

        // Build array with proper indices
        const maxIndex =
          numericKeys.length > 0
            ? Math.max(...numericKeys.map(k => parseInt(k)))
            : -1
        const arr = new Array(maxIndex + 1)

        // Fill in the values
        for (let i = 0; i <= maxIndex; i++) {
          if (childDecoded.hasOwnProperty(String(i))) {
            arr[i] = childDecoded[String(i)]
          } else {
            // Check if it's an empty value from ao-types
            const childTypes = parseAoTypes(value["ao-types"] || "")
            if (childTypes[String(i)] === "empty-binary") {
              arr[i] = ""
            } else {
              arr[i] = undefined
            }
          }
        }

        result[key] = arr.filter(v => v !== undefined)
      } else {
        result[key] = childDecoded
      }
    } else {
      // Already decoded value
      result[key] = value
    }
  }

  return result
}

/**
 * Internal function to convert native JS to TABM
 */
function nativeToTabm(msg) {
  if (typeof msg === "string" || msg instanceof Buffer) {
    return msg
  }

  if (typeof msg !== "object" || msg === null) {
    return msg
  }

  const types = []
  const values = {}

  // Sort keys to ensure consistent order
  const sortedKeys = Object.keys(msg).sort()

  for (const key of sortedKeys) {
    const value = msg[key]
    const binKey = normalizeKey(key)

    // Handle empty values
    if (value === "") {
      types.push([binKey, "empty-binary"])
      continue
    } else if (Array.isArray(value) && value.length === 0) {
      types.push([binKey, "empty-list"])
      continue
    } else if (isEmptyMessage(value)) {
      types.push([binKey, "empty-message"])
      continue
    }

    // Handle different value types
    if (typeof value === "string") {
      values[key] = value
    } else if (Array.isArray(value)) {
      // Check if it's a list of objects/arrays or primitives
      const hasObjects = value.some(
        item =>
          typeof item === "object" && item !== null && !Array.isArray(item)
      )

      if (hasObjects) {
        // List of maps - convert to numbered map
        types.push([binKey, "list"])
        const numberedMap = {}
        value.forEach((item, index) => {
          numberedMap[String(index)] = item
        })
        values[key] = nativeToTabm(numberedMap)
      } else {
        // List of primitives or mixed with nested arrays
        // Always encode as a list with type annotations
        const [type, encoded] = encodeValue(value)
        types.push([binKey, type])
        values[key] = encoded
      }
    } else if (typeof value === "object" && value !== null) {
      values[key] = nativeToTabm(value)
    } else if (
      typeof value === "number" ||
      typeof value === "boolean" ||
      value === null
    ) {
      // Encode primitive values
      const [type, encoded] = encodeValue(value)
      types.push([binKey, type])
      values[key] = encoded
    }
  }

  // Add ao-types field if there are any types
  if (types.length > 0) {
    values["ao-types"] = encodeDictionary(types)
  }

  return values
}

/**
 * Encode a value to its structured field representation
 * @param {*} value - The value to encode
 * @returns {[string, string]} - [type, encoded_value]
 */
function encodeValue(value) {
  if (Number.isInteger(value)) {
    return ["integer", String(value)]
  } else if (typeof value === "number") {
    // Float - simplified version, actual implementation would need proper decimal handling
    return ["float", String(value)]
  } else if (typeof value === "boolean") {
    // Booleans are encoded as atoms with the string representation
    return ["atom", `"${value}"`]
  } else if (value === null) {
    return ["atom", '"null"']
  } else if (Array.isArray(value)) {
    // Encode list of primitives
    const encoded = value
      .map(item => {
        if (typeof item === "string") {
          return `"${escapeString(item)}"`
        } else if (Array.isArray(item)) {
          // Nested list - encode the inner list and escape it
          const [_, innerEncoded] = encodeValue(item)
          // Double escape the quotes for nested lists
          const escapedInner = innerEncoded.replace(/"/g, '\\"')
          return `"(ao-type-list) ${escapedInner}"`
        } else if (typeof item === "number") {
          const [type, enc] = encodeValue(item)
          return `"(ao-type-${type}) ${enc}"`
        } else if (typeof item === "boolean") {
          const [type, enc] = encodeValue(item)
          // For booleans in lists, we need to escape the quotes in the atom value
          const escapedEnc = enc.replace(/"/g, '\\"')
          return `"(ao-type-${type}) ${escapedEnc}"`
        } else if (item === null) {
          return `"(ao-type-atom) \\"null\\""`
        } else {
          throw new Error(`Cannot encode list item of type ${typeof item}`)
        }
      })
      .join(", ")
    return ["list", encoded]
  } else {
    throw new Error(`Cannot encode value of type ${typeof value}`)
  }
}

/**
 * Decode a value from its structured field representation
 * @param {string} type - The type annotation
 * @param {string} value - The encoded value
 * @returns {*} - Decoded value
 */
function decodeValue(type, value) {
  switch (type.toLowerCase()) {
    case "integer":
      return parseInt(value, 10)
    case "float":
      return parseFloat(value)
    case "boolean":
      return value === "?1"
    case "atom":
      // Atoms are quoted strings, remove the quotes
      if (value === '"null"') return null
      if (value === '"true"') return true
      if (value === '"false"') return false
      return value.slice(1, -1) // Remove quotes
    case "list":
      return parseList(value)
    default:
      return value
  }
}

/**
 * Parse HTTP Structured Fields list format
 * @param {string} value - The list string to parse
 * @returns {Array} - Parsed list
 */
function parseList(value) {
  if (!value || value.trim() === "") return []

  const items = []
  let current = ""
  let inQuotes = false
  let escapeNext = false

  for (let i = 0; i < value.length; i++) {
    const char = value[i]
    const nextChar = i < value.length - 1 ? value[i + 1] : null

    if (escapeNext) {
      current += char
      escapeNext = false
      continue
    }

    if (char === "\\" && inQuotes) {
      // Check if this is an escape sequence
      if (nextChar === '"' || nextChar === "\\") {
        escapeNext = true
        current += char
        continue
      }
    }

    if (char === '"' && !escapeNext) {
      inQuotes = !inQuotes
    }

    if (char === "," && !inQuotes) {
      items.push(parseListItem(current.trim()))
      current = ""
    } else {
      current += char
    }
  }

  if (current.trim()) {
    items.push(parseListItem(current.trim()))
  }

  return items
}

/**
 * Parse a single list item
 * @param {string} item - The item to parse
 * @returns {*} - Parsed value
 */
function parseListItem(item) {
  if (item.startsWith('"') && item.endsWith('"')) {
    const content = item.slice(1, -1)
    if (content.startsWith("(ao-type-")) {
      const match = content.match(/^\(ao-type-([^)]+)\) (.+)$/)
      if (match) {
        const type = match[1]
        const value = match[2]

        if (type === "list") {
          // For nested lists, we need to unescape the quotes first
          const unescapedValue = value.replace(/\\"/g, '"')
          return parseList(unescapedValue)
        } else if (type === "atom") {
          // For atoms, the value is already quoted and may be escaped
          const unescapedValue = value.replace(/\\"/g, '"')
          return decodeValue(type, unescapedValue)
        } else {
          return decodeValue(type, value)
        }
      }
    }
    return unescapeString(content)
  }
  return item
}

/**
 * Parse the ao-types dictionary
 * @param {string} aoTypes - The ao-types string
 * @returns {Object} - Parsed types map
 */
function parseAoTypes(aoTypes) {
  if (!aoTypes) return {}

  const types = {}
  const entries = aoTypes
    .split(",")
    .map(e => e.trim())
    .filter(e => e)

  for (const entry of entries) {
    const match = entry.match(/^([^=]+)=(.+)$/)
    if (match) {
      const key = decodeKey(match[1].trim())
      const value = match[2].trim().replace(/^"|"$/g, "")
      types[key] = value
    }
  }

  return types
}

/**
 * Encode a dictionary to HTTP Structured Fields format
 * @param {Array} entries - Array of [key, value] pairs
 * @returns {string} - Encoded dictionary
 */
function encodeDictionary(entries) {
  return entries
    .map(([key, value]) => {
      const encodedKey = encodeKey(key)
      return `${encodedKey}="${value}"`
    })
    .join(", ")
}

/**
 * Normalize a key (similar to hb_ao:normalize_key)
 * @param {string} key - The key to normalize
 * @returns {string} - Normalized key
 */
function normalizeKey(key) {
  if (typeof key !== "string") return String(key)
  return key.toLowerCase()
}

/**
 * Encode a key for structured fields (similar to hb_escape:encode)
 * @param {string} key - The key to encode
 * @returns {string} - Encoded key
 */
function encodeKey(key) {
  // Simplified version - actual implementation would need full escaping
  return key.replace(/[^a-zA-Z0-9_-]/g, match => {
    return "%" + match.charCodeAt(0).toString(16).padStart(2, "0")
  })
}

/**
 * Decode an encoded key
 * @param {string} key - The encoded key
 * @returns {string} - Decoded key
 */
function decodeKey(key) {
  return key.replace(/%([0-9a-fA-F]{2})/g, (match, hex) => {
    return String.fromCharCode(parseInt(hex, 16))
  })
}

/**
 * Check if a value is an empty message/object
 * @param {*} value - The value to check
 * @returns {boolean} - True if empty message
 */
function isEmptyMessage(value) {
  return (
    typeof value === "object" &&
    value !== null &&
    !Array.isArray(value) &&
    Object.keys(value).length === 0
  )
}

/**
 * Convert a numbered map to an ordered list
 * @param {Object} map - The numbered map
 * @returns {Array} - Ordered list
 */
function messageToOrderedList(map) {
  const keys = Object.keys(map).filter(k => /^\d+$/.test(k))
  keys.sort((a, b) => parseInt(a) - parseInt(b))
  return keys.map(k => map[k])
}

/**
 * Escape a string for structured fields
 * @param {string} str - The string to escape
 * @returns {string} - Escaped string
 */
function escapeString(str) {
  return str.replace(/\\/g, "\\\\").replace(/"/g, '\\"')
}

/**
 * Unescape a string from structured fields
 * @param {string} str - The string to unescape
 * @returns {string} - Unescaped string
 */
function unescapeString(str) {
  // The test expects:
  // \\\" to become \"  (escaped quote becomes literal quote)
  // \\\\ to become \\  (escaped backslash becomes literal backslash)
  return str.replace(/\\\\/g, "\\").replace(/\\"/g, '"')
}
