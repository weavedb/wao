/**
 * Codec for converting between JS objects with Erlang types and annotated JSON
 * Only handles types that HyperBEAM supports
 */

/**
 * Convert annotated JSON to JS object with Erlang types
 * @param {Object} json - Annotated JSON object
 * @returns {*} - JS object with Buffer for binaries, Symbol for atoms
 */
export function erl_json_from(json) {
  return convertFromAnnotatedJSON(json)
}

/**
 * Convert JS object with Erlang types to annotated JSON
 * @param {*} jsObj - JS object with Erlang types
 * @returns {Object} - Annotated JSON object
 */
export function erl_json_to(jsObj) {
  return convertToAnnotatedJSON(jsObj)
}

/**
 * Normalize JS values to match what comes back from Erlang through erl_str_from
 * This function is deterministic and matches the behavior of the Erlang round-trip
 * @param {*} obj - JS object to normalize
 * @param {boolean} binaryMode - true for binary mode, false for string mode (default)
 * @returns {*} - Normalized JS object
 */
export function normalize(obj, binaryMode = false) {
  if (obj === null) return null
  if (obj === undefined) return undefined

  // Handle symbols - convert to their special cases or match erl_str_from behavior
  if (typeof obj === "symbol") {
    const key = Symbol.keyFor(obj)
    const name = key || obj.description || obj.toString().slice(7, -1)

    // Special symbols that become primitives
    if (name === "null") return null
    if (name === "true") return true
    if (name === "false") return false

    // Note: Symbol('undefined') stays as Symbol.for('undefined')
    // It does NOT become JavaScript undefined
    // This matches what erl_str_from actually returns

    // For all symbols (including 'undefined'), convert to global symbol
    // This is because non-global symbols can't round-trip through JSON
    // Symbol('ok') -> '%ok%' -> Symbol.for('ok')
    return Symbol.for(name)
  }

  // Strings
  if (typeof obj === "string") {
    // Special case: "::" becomes empty buffer through Erlang
    if (obj === "::") {
      return Buffer.alloc(0)
    }

    if (binaryMode) {
      // In binary mode, convert strings to buffers
      return Buffer.from(obj, "utf8")
    } else {
      // In string mode, strings stay as strings
      return obj
    }
  }

  // Other primitives pass through
  if (typeof obj === "number" || typeof obj === "boolean") {
    return obj
  }

  // Buffers pass through
  if (obj instanceof Buffer || obj instanceof Uint8Array) {
    return Buffer.from(obj)
  }

  // Arrays - normalize each element
  if (Array.isArray(obj)) {
    return obj.map(item => {
      // undefined in arrays becomes null (JSON behavior)
      if (item === undefined) {
        return null
      }
      return normalize(item, binaryMode)
    })
  }

  // Objects - normalize each value and remove undefined values
  if (typeof obj === "object" && obj !== null) {
    const result = {}
    for (const [k, v] of Object.entries(obj)) {
      const normalized = normalize(v, binaryMode)
      // Skip undefined values in objects
      if (normalized !== undefined) {
        result[k] = normalized
      }
    }
    return result
  }

  return obj
}

// Convert from annotated JSON to JS with Erlang types
function convertFromAnnotatedJSON(json) {
  // Handle null/undefined
  if (json === null) {
    return null
  }

  if (json === undefined) {
    return undefined
  }

  // Handle booleans
  if (json === true || json === false) {
    return json
  }

  // Handle numbers and strings
  if (typeof json === "number") {
    return json
  }

  if (typeof json === "string") {
    // Check for binary structured field format :base64:
    if (json.startsWith(":") && json.endsWith(":") && json.length >= 2) {
      try {
        // Handle empty binary special case
        if (json === "::") {
          return Buffer.alloc(0)
        }
        // Simple base64 decode for structured field binaries
        const base64 = json.slice(1, -1)
        return Buffer.from(base64, "base64")
      } catch (e) {
        // Not valid base64, return as-is
        return json
      }
    }
    // Check for token structured field format %token%
    if (json.startsWith("%") && json.endsWith("%") && json.length >= 2) {
      // Handle empty token special case
      if (json === "%%") {
        return Symbol.for("")
      }
      // Extract token and convert to symbol
      const token = json.slice(1, -1)
      return Symbol.for(token)
    }
    // Handle empty string from empty binary conversion
    if (json === "") {
      // In context, this might be an empty binary, but we can't be sure
      // so we return it as-is
      return json
    }
    return json
  }

  // Handle arrays
  if (Array.isArray(json)) {
    return json.map(item => convertFromAnnotatedJSON(item))
  }

  // Handle objects
  if (json && typeof json === "object") {
    const keys = Object.keys(json)

    // Check for type annotations (single key objects)
    if (keys.length === 1) {
      const [key] = keys
      const value = json[key]

      switch (key) {
        case "$empty":
          switch (value) {
            case "binary":
              return Buffer.alloc(0)
            case "list":
              return []
            case "map":
              return {}
            default:
              return json
          }
      }
    }

    // Regular object - convert recursively
    const result = {}
    for (const [k, v] of Object.entries(json)) {
      result[k] = convertFromAnnotatedJSON(v)
    }
    return result
  }

  // Fallback
  return json
}

// Convert from JS with Erlang types to annotated JSON
function convertToAnnotatedJSON(obj) {
  // Handle null - pass through as JSON null
  if (obj === null) {
    return null
  }

  // Handle undefined - convert to null (JSON doesn't support undefined)
  if (obj === undefined) {
    return null
  }

  // Handle booleans - pass through as JSON booleans
  if (typeof obj === "boolean") {
    return obj
  }

  // Handle numbers - pass through
  if (typeof obj === "number") {
    return obj
  }

  // Handle strings - pass through
  if (typeof obj === "string") {
    return obj
  }

  // Handle symbols (atoms) - use token structured field format
  if (typeof obj === "symbol") {
    // Try to get the key from the global symbol registry
    const key = Symbol.keyFor(obj)
    // If it's not a global symbol, use its description
    const name = key || obj.description || obj.toString().slice(7, -1)

    // Special atoms that become JSON values
    if (name === "null") return null
    if (name === "true") return true
    if (name === "false") return false

    // Other atoms become token structured fields
    return `%${name}%`
  }

  // Handle Buffers/Uint8Arrays (binaries)
  if (obj instanceof Buffer || obj instanceof Uint8Array) {
    if (obj.length === 0) {
      return { $empty: "binary" }
    }
    // Use structured fields format for binaries
    return `:${Buffer.from(obj).toString("base64")}:`
  }

  // Handle arrays
  if (Array.isArray(obj)) {
    if (obj.length === 0) {
      return { $empty: "list" }
    }
    return obj.map(item => convertToAnnotatedJSON(item))
  }

  // Handle objects
  if (typeof obj === "object" && obj !== null) {
    const result = {}
    let hasKeys = false

    for (const [k, v] of Object.entries(obj)) {
      // Skip undefined values completely
      if (v === undefined) {
        continue
      }
      hasKeys = true
      result[k] = convertToAnnotatedJSON(v)
    }

    // Return $empty annotation if no keys remain
    if (!hasKeys) {
      return { $empty: "map" }
    }

    return result
  }

  // Fallback
  return obj
}
