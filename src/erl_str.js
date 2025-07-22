/**
 * Convert a Buffer to string if it contains valid UTF-8 text
 * @param {Buffer} buffer - Buffer to check and potentially convert
 * @returns {string|Buffer} - String if valid UTF-8, otherwise original Buffer
 */
export function bin_to_str(buffer) {
  if (!Buffer.isBuffer(buffer)) {
    return buffer
  }

  // Empty buffer stays as buffer
  if (buffer.length === 0) {
    return buffer
  }

  try {
    const str = buffer.toString("utf8")
    // Check if it's valid UTF-8 by seeing if it round-trips correctly
    if (Buffer.from(str, "utf8").equals(buffer)) {
      // Check if this looks like a multipart body
      if (str.includes("--") && str.includes("content-disposition")) {
        // For multipart bodies, always convert to string
        return str
      }

      // For other content, check printability
      let isPrintable = true
      for (let i = 0; i < str.length; i++) {
        const code = str.charCodeAt(i)
        // Allow printable ASCII (32-126) and common whitespace (tab, newline, carriage return)
        if (
          !(code >= 32 && code <= 126) &&
          code !== 9 &&
          code !== 10 &&
          code !== 13
        ) {
          isPrintable = false
          break
        }
      }

      if (isPrintable) {
        return str
      }
    }
  } catch (e) {
    // Not valid UTF-8, return original buffer
  }

  return buffer
}

/**
 * Codec for converting between Erlang string format and JS objects
 * Only handles types that HyperBEAM supports
 */

/**
 * Convert Erlang string to JS object (with Erlang types)
 * @param {string} erlStr - Erlang term string
 * @returns {*} - JS object with Buffer for binaries, Symbol for atoms
 */
export function erl_str_from(erlStr) {
  if (typeof erlStr !== "string") {
    throw new Error("Input must be an Erlang string")
  }

  // If the string is already JSON with structured fields, parse it directly
  if (erlStr.startsWith("{") || erlStr.startsWith("[")) {
    try {
      const json = JSON.parse(erlStr)
      return parseStructuredFieldJSON(json)
    } catch (e) {
      // Not JSON, continue with Erlang term parsing
    }
  }

  const result = parseErlangTerm(erlStr)

  // Post-process to handle structured field values
  return postProcessStructuredFields(result)
}

// Post-process to convert buffers to strings for structured field values
function postProcessStructuredFields(obj) {
  if (obj === null || obj === undefined || typeof obj !== "object") {
    return obj
  }

  if (Buffer.isBuffer(obj)) {
    // Always try to convert buffers to strings if they're valid UTF-8
    const converted = bin_to_str(obj)
    return converted
  }

  if (Array.isArray(obj)) {
    return obj.map(item => postProcessStructuredFields(item))
  }

  // Handle objects/maps
  const result = {}
  for (const [key, value] of Object.entries(obj)) {
    if (Buffer.isBuffer(value)) {
      // Special handling for body field - always try to convert to string
      if (key === "body") {
        try {
          const str = value.toString("utf8")
          // For multipart bodies, we accept them even with binary content
          if (str.includes("--") && str.includes("content-disposition")) {
            result[key] = str
            continue
          }
        } catch (e) {
          // Fall through to regular handling
        }
      }

      const str = value.toString()
      // Convert buffers that are structured field values or special fields
      if (
        key === "ao-types" ||
        key === "content-digest" || // Always convert content-digest to string
        str.includes('="') || // Type annotations like '2="integer"'
        str.startsWith('"') || // Quoted values
        str === "?0" ||
        str === "?1" || // Boolean structured fields
        /^-?\d+$/.test(str) || // Plain integers
        str.includes("(ao-type-") || // Structured field type markers
        str.includes("sha-256=:") // Content digest format
      ) {
        result[key] = str
      } else {
        // For other buffers, apply bin_to_str
        result[key] = bin_to_str(value)
      }
    } else if (typeof value === "object" && value !== null) {
      // Recursively process nested objects
      result[key] = postProcessStructuredFields(value)
    } else {
      result[key] = value
    }
  }

  // Special handling for $empty objects
  if (result["$empty"] && Buffer.isBuffer(result["$empty"])) {
    result["$empty"] = result["$empty"].toString()
  }

  return result
}

/**
 * Convert JS object to Erlang string format
 * @param {*} jsObj - JS object with Erlang types
 * @returns {string} - Erlang term string
 */
export function erl_str_to(jsObj) {
  return toErlangString(jsObj)
}

// Parse JSON that contains structured field binaries
function parseStructuredFieldJSON(json) {
  if (json === null || json === undefined) {
    return json
  }

  if (typeof json === "string") {
    // Check for structured field binary format :base64:
    if (json.startsWith(":") && json.endsWith(":") && json.length >= 2) {
      try {
        // Handle empty binary special case
        if (json === "::") {
          return Buffer.alloc(0)
        }
        // Simple base64 decode for structured field binaries
        const base64 = json.slice(1, -1)
        let buffer = Buffer.from(base64, "base64")

        // Always apply bin_to_str
        return bin_to_str(buffer)
      } catch (e) {
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
    return json
  }

  if (Array.isArray(json)) {
    return json.map(item => parseStructuredFieldJSON(item))
  }

  if (typeof json === "object" && json !== null) {
    // Check if this is a numbered map that should be converted to array
    if (shouldConvertToArray(json)) {
      return convertNumberedMapToArray(json)
    }

    const result = {}
    for (const [k, v] of Object.entries(json)) {
      result[k] = parseStructuredFieldJSON(v)
    }
    return result
  }

  return json
}

// Check if object should be converted to array (has ao-types with list type)
function shouldConvertToArray(obj) {
  const aoTypes = obj["ao-types"]
  if (!aoTypes || typeof aoTypes !== "string") return false

  // Check if any field in ao-types is marked as "list"
  const types = aoTypes.split(",").map(t => t.trim())
  for (const type of types) {
    if (type.includes('="list"')) {
      const [key] = type.split("=")
      if (obj[key] && typeof obj[key] === "object") {
        return true
      }
    }
  }
  return false
}

// Convert numbered map to array
function convertNumberedMapToArray(obj) {
  const result = {}

  for (const [key, value] of Object.entries(obj)) {
    if (key === "ao-types") {
      result[key] = value
      continue
    }

    // Check if this field is a list according to ao-types
    const aoTypes = obj["ao-types"]
    if (aoTypes && aoTypes.includes(`${key}="list"`)) {
      // Convert 1-based to 0-based indexing
      const arr = []
      const listObj = parseStructuredFieldJSON(value)

      // Find all numeric keys
      const numericKeys = Object.keys(listObj)
        .filter(k => /^\d+$/.test(k))
        .map(k => parseInt(k))
        .sort((a, b) => a - b)

      for (const numKey of numericKeys) {
        // Convert 1-based to 0-based index
        arr[numKey - 1] = parseStructuredFieldJSON(listObj[String(numKey)])
      }

      result[key] = arr
    } else {
      result[key] = parseStructuredFieldJSON(value)
    }
  }

  return result
}

// Parser for Erlang terms
function parseErlangTerm(str) {
  let pos = 0

  function skipWhitespace() {
    while (pos < str.length && /\s/.test(str[pos])) pos++
  }

  function parseValue() {
    skipWhitespace()

    // Map: #{...}
    if (str[pos] === "#" && str[pos + 1] === "{") {
      pos += 2
      const map = {}

      while (pos < str.length) {
        skipWhitespace()
        if (str[pos] === "}") {
          pos++
          break
        }

        // Parse key => value
        const key = parseValue()
        skipWhitespace()

        // Skip =>
        if (str[pos] === "=" && str[pos + 1] === ">") {
          pos += 2
        } else {
          throw new Error(`Expected => at position ${pos}`)
        }

        const value = parseValue()

        // Convert binary key to string for JS object
        const jsKey = key instanceof Buffer ? key.toString() : key
        map[jsKey] = value

        skipWhitespace()
        if (str[pos] === ",") pos++
      }

      // Check if this map should be converted to array
      if (shouldConvertToArray(map)) {
        return convertNumberedMapToArray(map)
      }

      return map
    }

    // Binary: <<...>> or <<"...">>
    if (str[pos] === "<" && str[pos + 1] === "<") {
      pos += 2

      // String binary: <<"...">>
      if (str[pos] === '"') {
        pos++
        let content = ""
        while (
          pos < str.length &&
          !(str[pos] === '"' && str[pos + 1] === ">" && str[pos + 2] === ">")
        ) {
          if (str[pos] === "\\" && str[pos + 1] === '"') {
            content += '"'
            pos += 2
          } else if (str[pos] === "\\" && str[pos + 1] === "\\") {
            content += "\\"
            pos += 2
          } else if (str[pos] === "\\" && str[pos + 1] === "n") {
            content += "\n"
            pos += 2
          } else if (str[pos] === "\\" && str[pos + 1] === "r") {
            content += "\r"
            pos += 2
          } else if (str[pos] === "\\" && str[pos + 1] === "t") {
            content += "\t"
            pos += 2
          } else {
            content += str[pos++]
          }
        }
        pos += 3 // Skip ">>

        // Check if the content is a structured field binary format :base64:
        if (
          content.startsWith(":") &&
          content.endsWith(":") &&
          content.length >= 2
        ) {
          try {
            // Handle empty binary special case
            if (content === "::") {
              return Buffer.alloc(0)
            }
            // Decode structured field binaries
            const base64 = content.slice(1, -1)
            return Buffer.from(base64, "base64")
          } catch (e) {
            // Not valid base64, return as string
            return content
          }
        }

        // String binaries return as buffers (original behavior)
        return Buffer.from(content)
      }

      // Empty binary: <<>>
      if (str[pos] === ">" && str[pos + 1] === ">") {
        pos += 2
        return Buffer.alloc(0)
      }

      // Byte binary: <<1,2,3>>
      const bytes = []
      while (pos < str.length && !(str[pos] === ">" && str[pos + 1] === ">")) {
        skipWhitespace()
        let num = ""
        while (pos < str.length && /\d/.test(str[pos])) {
          num += str[pos++]
        }
        if (num) bytes.push(parseInt(num))
        skipWhitespace()
        if (str[pos] === ",") pos++
      }
      pos += 2 // Skip >>

      // For byte arrays, check if it's valid UTF-8 text
      if (bytes.length > 0) {
        const buffer = Buffer.from(bytes)
        // Apply bin_to_str to convert to string if it's printable text
        return bin_to_str(buffer)
      }

      return Buffer.from(bytes)
    }

    // List: [...]
    if (str[pos] === "[") {
      pos++
      const list = []

      while (pos < str.length) {
        skipWhitespace()
        if (str[pos] === "]") {
          pos++
          break
        }

        list.push(parseValue())

        skipWhitespace()
        if (str[pos] === ",") pos++
      }

      return list
    }

    // Atom: lowercase start or 'quoted atom'
    if (str[pos] === "'") {
      pos++
      let atom = ""
      while (pos < str.length && str[pos] !== "'") {
        if (str[pos] === "\\" && str[pos + 1] === "'") {
          atom += "'"
          pos += 2
        } else {
          atom += str[pos++]
        }
      }
      pos++

      // Special handling for null/true/false
      if (atom === "null") return null
      if (atom === "true") return true
      if (atom === "false") return false

      return Symbol.for(atom)
    }

    // Unquoted atom
    if (/[a-z]/.test(str[pos])) {
      let atom = ""
      while (pos < str.length && /[a-zA-Z0-9_]/.test(str[pos])) {
        atom += str[pos++]
      }

      // Special handling for null/true/false
      if (atom === "null") return null
      if (atom === "true") return true
      if (atom === "false") return false
      if (atom === "undefined") return undefined

      return Symbol.for(atom)
    }

    // Number (including floats with scientific notation)
    if (/[\d-]/.test(str[pos])) {
      let num = ""
      // Support scientific notation (e.g., 3.14e+00)
      while (pos < str.length && /[\d.eE+-]/.test(str[pos])) {
        num += str[pos++]
      }
      return num.includes(".") || num.includes("e") || num.includes("E")
        ? parseFloat(num)
        : parseInt(num)
    }

    throw new Error(`Unexpected character at position ${pos}: ${str[pos]}`)
  }

  return parseValue()
}

// Convert JS object to Erlang string
function toErlangString(obj) {
  // Handle null (not undefined)
  if (obj === null) {
    return "null"
  }

  // Handle undefined as atom
  if (obj === undefined) {
    return "undefined"
  }

  // Handle booleans
  if (typeof obj === "boolean") {
    return obj.toString()
  }

  // Handle symbols (atoms)
  if (typeof obj === "symbol") {
    const name = obj.description || obj.toString().slice(7, -1)

    // Special handling for null/true/false symbols
    if (name === "null" || name === "true" || name === "false") {
      return name
    }

    // Quote atom if it contains special characters or starts with uppercase
    if (/^[a-z][a-zA-Z0-9_]*$/.test(name)) {
      return name
    } else {
      return `'${name.replace(/'/g, "\\'")}'`
    }
  }

  // Handle strings (as binaries)
  if (typeof obj === "string") {
    const escaped = obj.replace(/\\/g, "\\\\").replace(/"/g, '\\"')
    return `<<"${escaped}">>`
  }

  // Handle buffers (as binaries)
  if (obj instanceof Buffer || obj instanceof Uint8Array) {
    if (obj.length === 0) {
      return "<<>>"
    }
    // Check if it can be represented as a string binary
    try {
      const str = Buffer.from(obj).toString("utf8")
      // Verify it's valid UTF-8 by checking if it round-trips correctly
      if (Buffer.from(str, "utf8").equals(Buffer.from(obj))) {
        const escaped = str.replace(/\\/g, "\\\\").replace(/"/g, '\\"')
        return `<<"${escaped}">>`
      }
    } catch (e) {
      // Fall through to byte representation
    }
    // Use byte representation for non-UTF8 data
    return `<<${Array.from(obj).join(",")}>>`
  }

  // Handle numbers
  if (typeof obj === "number") {
    return obj.toString()
  }

  // Handle arrays (as lists)
  if (Array.isArray(obj)) {
    if (obj.length === 0) {
      return "[]"
    }
    return `[${obj.map(item => toErlangString(item)).join(",")}]`
  }

  // Handle objects (as maps)
  if (typeof obj === "object" && obj !== null) {
    const pairs = Object.entries(obj)
      .map(([k, v]) => `${toErlangString(k)} => ${toErlangString(v)}`)
      .join(",")

    if (pairs === "") {
      return "#{}"
    }
    return `#{${pairs}}`
  }

  // Fallback
  return "undefined"
}

// Check if a binary is a valid UTF-8 string
function isValidUtf8String(buffer) {
  try {
    const str = buffer.toString("utf8")
    // Check if it round-trips correctly
    return Buffer.from(str, "utf8").equals(buffer)
  } catch (e) {
    return false
  }
}
