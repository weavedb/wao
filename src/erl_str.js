/**
 * Clean Erlang term parser for JavaScript
 * Parses Erlang terms into JavaScript values
 */

export function parseErlangString(str) {
  let position = 0

  function peek(offset = 0) {
    return str[position + offset] || ""
  }

  function advance(count = 1) {
    position += count
  }

  function skipWhitespace() {
    while (position < str.length && /\s/.test(peek())) {
      advance()
    }
  }

  function parseString() {
    // Parse quoted string/atom
    if (peek() !== "'") return null

    advance() // skip opening quote
    let result = ""

    while (position < str.length && peek() !== "'") {
      if (peek() === "\\" && peek(1) === "'") {
        result += "'"
        advance(2)
      } else if (peek() === "\\" && peek(1) === "\\") {
        result += "\\"
        advance(2)
      } else if (peek() === "\\" && peek(1) === "n") {
        result += "\n"
        advance(2)
      } else if (peek() === "\\" && peek(1) === "r") {
        result += "\r"
        advance(2)
      } else if (peek() === "\\" && peek(1) === "t") {
        result += "\t"
        advance(2)
      } else {
        result += peek()
        advance()
      }
    }

    if (peek() === "'") advance() // skip closing quote

    // Convert special atoms
    if (result === "null") return null
    if (result === "true") return true
    if (result === "false") return false

    return Symbol.for(result)
  }

  function parseBinary() {
    // Parse <<...>>
    if (peek() !== "<" || peek(1) !== "<") return null

    advance(2) // skip <<

    // String binary <<"...">>
    if (peek() === '"') {
      advance() // skip "
      let content = ""

      while (
        position < str.length &&
        !(peek() === '"' && peek(1) === ">" && peek(2) === ">")
      ) {
        if (peek() === "\\" && peek(1) === '"') {
          content += '"'
          advance(2)
        } else if (peek() === "\\" && peek(1) === "\\") {
          content += "\\"
          advance(2)
        } else if (peek() === "\\" && peek(1) === "n") {
          content += "\n"
          advance(2)
        } else if (peek() === "\\" && peek(1) === "r") {
          content += "\r"
          advance(2)
        } else if (peek() === "\\" && peek(1) === "t") {
          content += "\t"
          advance(2)
        } else {
          content += peek()
          advance()
        }
      }

      advance(3) // skip ">>

      // Check for structured field format
      if (
        content.startsWith(":") &&
        content.endsWith(":") &&
        content.length >= 2
      ) {
        if (content === "::") {
          return Buffer.alloc(0)
        }
        try {
          const base64 = content.slice(1, -1)
          return Buffer.from(base64, "base64")
        } catch (e) {
          // Not valid base64, return as buffer
          return Buffer.from(content)
        }
      }

      return Buffer.from(content)
    }

    // Empty binary <<>>
    if (peek() === ">" && peek(1) === ">") {
      advance(2)
      return Buffer.alloc(0)
    }

    // Byte binary <<1,2,3>>
    const bytes = []
    while (position < str.length && !(peek() === ">" && peek(1) === ">")) {
      skipWhitespace()

      let num = ""
      while (position < str.length && /\d/.test(peek())) {
        num += peek()
        advance()
      }

      if (num) bytes.push(parseInt(num))

      skipWhitespace()
      if (peek() === ",") advance()
    }

    advance(2) // skip >>
    return Buffer.from(bytes)
  }

  function parseList() {
    // Parse [...]
    if (peek() !== "[") return null

    advance() // skip [
    const list = []

    while (position < str.length) {
      skipWhitespace()

      if (peek() === "]") {
        advance()
        break
      }

      const value = parseValue()
      list.push(value)

      skipWhitespace()
      if (peek() === ",") advance()
    }

    return list
  }

  function parseMap() {
    // Parse #{...}
    if (peek() !== "#" || peek(1) !== "{") return null

    advance(2) // skip #{
    const map = {}

    while (position < str.length) {
      skipWhitespace()

      if (peek() === "}") {
        advance()
        break
      }

      // Parse key
      const key = parseValue()
      skipWhitespace()

      // Skip =>
      if (peek() === "=" && peek(1) === ">") {
        advance(2)
      } else {
        throw new Error(`Expected => at position ${position}`)
      }

      skipWhitespace()

      // Parse value
      const value = parseValue()

      // Convert key to string for JavaScript object
      const jsKey = key instanceof Buffer ? key.toString() : String(key)
      map[jsKey] = value

      skipWhitespace()
      if (peek() === ",") advance()
    }

    // Don't convert maps to arrays - keep them as objects
    // JavaScript will use string keys for numeric indices anyway

    return map
  }

  function parseAtomOrNumber() {
    // Parse unquoted atoms and numbers
    let token = ""
    const startPos = position

    // Collect characters until we hit a delimiter
    while (position < str.length) {
      const ch = peek()

      // Check for delimiters
      if (
        ch === "," ||
        ch === "]" ||
        ch === "}" ||
        ch === "\t" ||
        ch === "\r" ||
        ch === ">" ||
        (ch === "=" && peek(1) === ">") ||
        (ch === "<" && peek(1) === "<") ||
        (ch === "#" && peek(1) === "{") ||
        ch === "[" ||
        ch === "{"
      ) {
        break
      }

      // Special handling for spaces
      // In some cases, Erlang sends atoms with spaces without quotes
      // We need to detect patterns like "with spaces", "also global", etc.
      if (ch === " ") {
        // If we have "with" and the next word is "spaces", combine them
        if (token === "with" || token === "also") {
          let lookahead = 1
          let nextWord = ""

          // Skip spaces
          while (
            position + lookahead < str.length &&
            str[position + lookahead] === " "
          ) {
            lookahead++
          }

          // Collect the next word
          while (
            position + lookahead < str.length &&
            /[a-zA-Z0-9_-]/.test(str[position + lookahead])
          ) {
            nextWord += str[position + lookahead]
            lookahead++
          }

          // Check if this forms a known pattern
          if (
            (token === "with" && nextWord === "spaces") ||
            (token === "also" && nextWord === "global")
          ) {
            // Include the space and continue parsing
            token += ch
            advance()
            continue
          }
        }

        // Otherwise, space is a delimiter
        break
      }

      // Special handling for newlines in atoms
      if (ch === "\n") {
        // Check if this is part of an atom like with\nnewline
        if (position + 1 < str.length && /[a-zA-Z0-9_-]/.test(peek(1))) {
          token += ch
          advance()
          continue
        }
        // Otherwise newline is a delimiter
        break
      }

      token += ch
      advance()
    }

    // If we didn't collect any characters, return undefined (not null!)
    // This indicates we didn't find an atom/number at this position
    if (token === "") return undefined

    // Check special atoms
    if (token === "null") return null
    if (token === "true") return true
    if (token === "false") return false
    // Don't convert 'undefined' to JavaScript undefined!
    // Keep it as a symbol like any other atom

    // Check if it's a number
    if (/^-?\d+(\.\d+)?([eE][+-]?\d+)?$/.test(token)) {
      return token.includes(".") || token.includes("e") || token.includes("E")
        ? parseFloat(token)
        : parseInt(token)
    }

    // Otherwise it's an atom
    return Symbol.for(token)
  }

  function parseValue() {
    skipWhitespace()

    // Try parsing different value types
    let result

    // Map
    if (peek() === "#" && peek(1) === "{") {
      return parseMap()
    }

    // Binary
    if (peek() === "<" && peek(1) === "<") {
      return parseBinary()
    }

    // List
    if (peek() === "[") {
      return parseList()
    }

    // Quoted atom
    if (peek() === "'") {
      return parseString()
    }

    // Unquoted atom or number
    result = parseAtomOrNumber()
    if (result !== undefined) {
      return result
    }

    // If nothing matched, throw error
    throw new Error(
      `Unexpected character at position ${position}: '${peek()}' (char code: ${peek().charCodeAt(0)})\n` +
        `Context: "${str.slice(Math.max(0, position - 10), position)}[${peek()}]${str.slice(position + 1, position + 10)}"`
    )
  }

  // Parse the string
  const result = parseValue()

  // Make sure we consumed everything
  skipWhitespace()
  if (position < str.length) {
    throw new Error(
      `Unexpected content at position ${position}: ${str.slice(position)}`
    )
  }

  return result
}

// Export the parser function
export function erl_str_from(str) {
  return parseErlangString(str)
}

// Formatter function - converts JavaScript values to Erlang string representation
export function erl_str_to(value) {
  function escapeString(str) {
    return str
      .replace(/\\/g, "\\\\")
      .replace(/'/g, "\\'")
      .replace(/\n/g, "\\n")
      .replace(/\r/g, "\\r")
      .replace(/\t/g, "\\t")
  }

  function formatValue(val) {
    // null
    if (val === null) return "null"

    // undefined
    if (val === undefined) return "undefined"

    // boolean
    if (typeof val === "boolean") return val.toString()

    // number
    if (typeof val === "number") {
      if (Number.isInteger(val)) return val.toString()
      // Format float with enough precision
      return val.toString()
    }

    // string
    if (typeof val === "string") {
      return `'${escapeString(val)}'`
    }

    // symbol
    if (typeof val === "symbol") {
      const key = Symbol.keyFor(val)
      const desc = val.description || key || ""

      // Check if it's a special symbol that becomes a literal
      if (desc === "null") return "null"
      if (desc === "true") return "true"
      if (desc === "false") return "false"
      if (desc === "undefined") return "undefined"

      // Regular atom - check if it needs quoting
      if (/^[a-z][a-zA-Z0-9_]*$/.test(desc)) {
        // Simple atom, no quotes needed
        return desc
      } else {
        // Complex atom, needs quotes
        return `'${escapeString(desc)}'`
      }
    }

    // Buffer
    if (val instanceof Buffer || val instanceof Uint8Array) {
      const bytes = Array.from(val)

      // Check if it's printable ASCII
      const isPrintable = bytes.every(b => b >= 32 && b <= 126)

      if (isPrintable) {
        // Use string binary format
        const str = Buffer.from(val).toString()
        return `<<"${escapeString(str)}">>`
      } else {
        // Use byte list format
        return `<<${bytes.join(",")}>>`
      }
    }

    // Array
    if (Array.isArray(val)) {
      const items = val.map(formatValue)
      return `[${items.join(",")}]`
    }

    // Object/Map
    if (typeof val === "object" && val !== null) {
      const entries = Object.entries(val).map(([k, v]) => {
        const key = /^\d+$/.test(k) ? k : `<<"${escapeString(k)}">>`
        return `${key} => ${formatValue(v)}`
      })
      return `#{${entries.join(",")}}`
    }

    // Fallback
    return String(val)
  }

  return formatValue(value)
}
