/**
 * A codec for turning nested objects into/from flat objects that have
 * (potentially multi-layer) paths as their keys, and values as their values.
 */

/**
 * Convert a flat map to a nested object.
 * @param {Object|string} input - Either a flat object or a serialized string
 * @returns {Object} - Nested object
 */
export function flat_from(input) {
  if (typeof input === "string" || input instanceof Buffer) {
    // If input is binary/string, deserialize it first
    return deserialize(input).ok
  }

  if (typeof input !== "object" || input === null) {
    throw new Error("Input must be an object or string")
  }

  const result = {}

  for (const [pathKey, value] of Object.entries(input)) {
    const pathParts = pathToParts(pathKey)
    injectAtPath(pathParts, value, result)
  }

  return result
}

/**
 * Convert a nested object to a flat map.
 * @param {Object|string} input - Either a nested object or a binary string (passthrough)
 * @returns {Object|string} - Flat object or passthrough string
 */
export function flat_to(input) {
  if (typeof input === "string" || input instanceof Buffer) {
    // Binary passthrough
    return input
  }

  if (typeof input !== "object" || input === null) {
    throw new Error("Input must be an object or string")
  }

  const result = {}
  flattenRecursive(input, [], result)
  return result
}

/**
 * Helper function to convert a path string to path parts
 * @param {string|Array} path - Path string like "a/b/c" or array of parts
 * @returns {Array} - Array of path parts
 */
function pathToParts(path) {
  if (Array.isArray(path)) {
    // Handle array paths
    if (path.length === 1 && Array.isArray(path[0])) {
      return path[0]
    }
    return path
  }

  if (typeof path === "string") {
    // Split by '/' but handle edge cases
    return path.split("/")
  }

  throw new Error("Path must be a string or array")
}

/**
 * Helper function to inject a value at a specific path in a nested object
 * @param {Array} pathParts - Array of path parts
 * @param {*} value - Value to inject
 * @param {Object} obj - Object to inject into
 */
function injectAtPath(pathParts, value, obj) {
  if (pathParts.length === 0) {
    throw new Error("Path cannot be empty")
  }

  if (pathParts.length === 1) {
    const key = pathParts[0]

    if (key in obj) {
      const existing = obj[key]

      // If both are objects, merge them
      if (
        typeof existing === "object" &&
        existing !== null &&
        typeof value === "object" &&
        value !== null &&
        !Array.isArray(existing) &&
        !Array.isArray(value)
      ) {
        obj[key] = { ...existing, ...value }
      } else {
        // Path collision
        throw new Error(
          `Path collision at key: ${key}, existing: ${JSON.stringify(existing)}, value: ${JSON.stringify(value)}`
        )
      }
    } else {
      obj[key] = value
    }
    return
  }

  const [key, ...rest] = pathParts

  if (!(key in obj)) {
    obj[key] = {}
  } else if (typeof obj[key] !== "object" || obj[key] === null) {
    throw new Error(`Cannot create nested path at non-object key: ${key}`)
  }

  injectAtPath(rest, value, obj[key])
}

/**
 * Helper function to recursively flatten an object
 * @param {*} value - Current value
 * @param {Array} currentPath - Current path parts
 * @param {Object} result - Result object to populate
 */
function flattenRecursive(value, currentPath, result) {
  if (typeof value === "object" && value !== null && !Array.isArray(value)) {
    // It's an object, recurse into it
    for (const [key, subValue] of Object.entries(value)) {
      const newPath = [...currentPath, key]
      flattenRecursive(subValue, newPath, result)
    }
  } else if (typeof value === "string") {
    // It's a string leaf value (matching Erlang binary type)
    if (currentPath.length === 0) {
      throw new Error("Cannot flatten a non-object at root level")
    }

    const pathKey = currentPath.join("/")
    result[pathKey] = value
  } else {
    // Non-string leaf values cause errors in Erlang codec
    throw new Error(
      `Value type ${typeof value} not supported. Only strings and objects are allowed.`
    )
  }
}

/**
 * Serialize a map to a string format
 * @param {Object} map - The map to serialize
 * @returns {Object} - {ok: string} or {error: string}
 */
function serialize(map) {
  try {
    const flattened = flat_to(map)
    const lines = []

    for (const [key, value] of Object.entries(flattened)) {
      lines.push(`${key}: ${value}`)
    }

    return { ok: lines.join("\n") + (lines.length > 0 ? "\n" : "") }
  } catch (error) {
    return { error: error.message }
  }
}

/**
 * Deserialize a string to a map
 * @param {string} input - The string to deserialize
 * @returns {Object} - {ok: Object} or {error: string}
 */
function deserialize(input) {
  try {
    const str = input.toString()
    const lines = str.split("\n").filter(line => line.trim())
    const flat = {}

    for (const line of lines) {
      const colonIndex = line.indexOf(": ")
      if (colonIndex !== -1) {
        const key = line.substring(0, colonIndex)
        const value = line.substring(colonIndex + 2)
        flat[key] = value
      }
    }

    return { ok: flat_from(flat) }
  } catch (error) {
    return { error: error.message }
  }
}
