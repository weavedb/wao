/**
 * Erlang term string parser and formatter
 * Converts between Erlang term strings and JavaScript values
 */

/**
 * Parse an Erlang term string into JavaScript values
 * @param {string} str - Erlang term string
 * @param {boolean} binaryMode - If true, keep binaries as Buffers; if false, convert to strings
 * @returns {*} JavaScript value
 */
export function erl_str_from(str, binaryMode = false) {
  // Handle the new response format
  if (str.startsWith("#erl_response{")) {
    const rawMatch = str.match(/#erl_response\{raw=(.*?),formatted=(.*?)\}$/s)
    if (rawMatch && rawMatch[1] && rawMatch[2]) {
      const rawStr = rawMatch[1]
      const formattedStr = rawMatch[2]

      if (binaryMode) {
        // In binary mode, just parse formatted
        const parser = new ErlangParser(formattedStr, true)
        return parser.parse()
      } else {
        // Build a type map from raw, then parse formatted with type info
        const typeMap = buildTypeMap(rawStr)
        const parser = new TypeAwareParser(formattedStr, typeMap)
        return parser.parse()
      }
    }
  }

  // Fallback for non-response format
  const parser = new ErlangParser(str, binaryMode)
  return parser.parse()
}

/**
 * Parse body field specially - convert to string only for multipart bodies
 * @param {*} parsed - Parsed Erlang term object
 * @returns {*} Object with body field potentially converted to string
 */
export function parse_body(parsed) {
  if (!parsed || typeof parsed !== "object") {
    return parsed
  }

  // If there's a body field
  if ("body" in parsed) {
    if (Buffer.isBuffer(parsed.body)) {
      // Check if this looks like a multipart body
      // Multipart bodies start with "--" boundary
      const bodyStart = parsed.body.toString(
        "binary",
        0,
        Math.min(100, parsed.body.length)
      )
      if (
        bodyStart.startsWith("--") &&
        parsed["content-type"] &&
        parsed["content-type"].includes("multipart")
      ) {
        // It's a multipart body, convert to string
        return {
          ...parsed,
          body: parsed.body.toString("binary"),
        }
      }
      // Not multipart, keep as Buffer
      return parsed
    }
    // Already a string, return as-is
    return parsed
  }

  return parsed
}

// Build a map of paths to types by analyzing the raw string
function buildTypeMap(rawStr) {
  const typeMap = new Map()

  function detectBinaryType(str, startPos) {
    // Check what follows <<
    let pos = startPos + 2
    while (pos < str.length && /\s/.test(str[pos])) pos++

    if (pos < str.length && str[pos] === '"') {
      // It looks like a string, but check if it has escape sequences
      // If it has \NNN octal escapes, it's actually a binary that Erlang formatted as string
      let scanPos = pos + 1
      let hasOctalEscapes = false

      while (
        scanPos < str.length &&
        !(
          str[scanPos] === '"' &&
          str[scanPos + 1] === ">" &&
          str[scanPos + 2] === ">"
        )
      ) {
        if (str[scanPos] === "\\" && scanPos + 1 < str.length) {
          const nextChar = str[scanPos + 1]
          // Check for octal escape \NNN
          if (/[0-7]/.test(nextChar)) {
            hasOctalEscapes = true
            break
          }
          scanPos += 2
        } else {
          scanPos++
        }
      }

      // If it has octal escapes, treat as binary
      if (hasOctalEscapes) {
        return "binary"
      }

      return "string"
    }
    return "binary"
  }

  function scanValue(str, pos, path) {
    while (pos < str.length && /\s/.test(str[pos])) pos++

    if (pos >= str.length) return pos

    if (str[pos] === "#" && str[pos + 1] === "{") {
      // Map
      pos += 2
      let mapIndex = 0

      while (pos < str.length) {
        while (pos < str.length && /\s/.test(str[pos])) pos++
        if (str[pos] === "}") return pos + 1

        // Parse key
        const keyStart = pos
        if (str[pos] === "<" && str[pos + 1] === "<") {
          // Binary key
          const keyType = detectBinaryType(str, pos)
          let keyEnd = pos + 2

          if (keyType === "string") {
            keyEnd++ // skip "
            while (
              keyEnd < str.length &&
              !(
                str[keyEnd] === '"' &&
                str[keyEnd + 1] === ">" &&
                str[keyEnd + 2] === ">"
              )
            ) {
              if (str[keyEnd] === "\\") keyEnd++
              keyEnd++
            }
            keyEnd += 3
          } else {
            let depth = 1
            while (depth > 0 && keyEnd < str.length) {
              if (str[keyEnd] === "<" && str[keyEnd + 1] === "<") {
                depth++
                keyEnd += 2
              } else if (str[keyEnd] === ">" && str[keyEnd + 1] === ">") {
                depth--
                keyEnd += 2
              } else {
                keyEnd++
              }
            }
          }

          pos = keyEnd
        } else {
          // Regular key
          while (pos < str.length && !/[\s=,}]/.test(str[pos])) pos++
        }

        // Skip =>
        while (pos < str.length && /\s/.test(str[pos])) pos++
        if (str[pos] === "=" && str[pos + 1] === ">") pos += 2
        while (pos < str.length && /\s/.test(str[pos])) pos++

        // Parse value and record type if binary
        if (str[pos] === "<" && str[pos + 1] === "<") {
          const type = detectBinaryType(str, pos)
          const pathKey = [...path, mapIndex].join(".")
          typeMap.set(pathKey, type)
        }

        pos = scanValue(str, pos, [...path, mapIndex])
        mapIndex++

        // Skip comma
        while (pos < str.length && /\s/.test(str[pos])) pos++
        if (str[pos] === ",") pos++
      }
    } else if (str[pos] === "<" && str[pos + 1] === "<") {
      // Binary
      const type = detectBinaryType(str, pos)
      const pathKey = path.join(".")
      typeMap.set(pathKey, type)

      pos += 2
      if (type === "string" || str[pos] === '"') {
        // Skip past string literal
        if (str[pos] === '"') pos++ // skip opening quote
        while (
          pos < str.length &&
          !(str[pos] === '"' && str[pos + 1] === ">" && str[pos + 2] === ">")
        ) {
          if (str[pos] === "\\") pos++
          pos++
        }
        return pos + 3
      } else {
        let depth = 1
        while (depth > 0 && pos < str.length) {
          if (str[pos] === "<" && str[pos + 1] === "<") {
            depth++
            pos += 2
          } else if (str[pos] === ">" && str[pos + 1] === ">") {
            depth--
            pos += 2
          } else {
            pos++
          }
        }
        return pos
      }
    } else if (str[pos] === "[") {
      // List
      pos++
      let listIndex = 0

      while (pos < str.length) {
        while (pos < str.length && /\s/.test(str[pos])) pos++
        if (str[pos] === "]") return pos + 1

        pos = scanValue(str, pos, [...path, listIndex])
        listIndex++

        while (pos < str.length && /\s/.test(str[pos])) pos++
        if (str[pos] === ",") pos++
      }
    } else {
      // Skip other values
      while (pos < str.length && !/[\s,\]}=>]/.test(str[pos])) {
        pos++
      }
      return pos
    }

    return pos
  }

  scanValue(rawStr, 0, [])
  return typeMap
}

// Parser that uses type information
class TypeAwareParser {
  constructor(str, typeMap) {
    this.str = str
    this.typeMap = typeMap
    this.pos = 0
    this.path = []
  }

  parse() {
    const result = this.parseValue()
    this.skipWhitespace()
    if (this.pos < this.str.length) {
      throw new Error(`Unexpected content at position ${this.pos}`)
    }
    return result
  }

  peek(offset = 0) {
    return this.str[this.pos + offset] || ""
  }

  advance(count = 1) {
    this.pos += count
  }

  skipWhitespace() {
    while (this.pos < this.str.length && /\s/.test(this.peek())) {
      this.advance()
    }
  }

  parseValue() {
    this.skipWhitespace()

    const ch = this.peek()
    const ch2 = this.peek(1)

    if (ch === "#" && ch2 === "{") {
      return this.parseMap()
    }

    if (ch === "<" && ch2 === "<") {
      return this.parseBinary()
    }

    if (ch === "[") {
      return this.parseList()
    }

    if (ch === "'") {
      return this.parseQuotedAtom()
    }

    if (ch === '"') {
      return this.parseQuotedString()
    }

    return this.parseAtomOrNumber()
  }

  parseMap() {
    this.advance(2) // skip #{
    const map = {}
    let mapIndex = 0

    while (true) {
      this.skipWhitespace()

      if (this.peek() === "}") {
        this.advance()
        break
      }

      this.path.push(mapIndex)
      const key = this.parseValue()
      this.path.pop()

      this.skipWhitespace()

      if (this.peek() !== "=" || this.peek(1) !== ">") {
        throw new Error("Expected =>")
      }
      this.advance(2)

      this.skipWhitespace()

      this.path.push(mapIndex)
      const value = this.parseValue()
      this.path.pop()

      const jsKey =
        key instanceof Buffer
          ? key.toString("utf8")
          : typeof key === "symbol"
            ? Symbol.keyFor(key) || String(key)
            : String(key)

      map[jsKey] = value
      mapIndex++

      this.skipWhitespace()
      if (this.peek() === ",") {
        this.advance()
      }
    }

    return map
  }

  parseBinary() {
    const currentPath = this.path.join(".")
    const type = this.typeMap.get(currentPath) || "binary"

    // Parse the formatted version (always byte format)
    this.advance(2) // skip <<

    if (this.peek() === ">" && this.peek(1) === ">") {
      this.advance(2)
      return Buffer.alloc(0)
    }

    const bytes = []
    while (!(this.peek() === ">" && this.peek(1) === ">")) {
      this.skipWhitespace()

      let num = ""
      while (/\d/.test(this.peek())) {
        num += this.peek()
        this.advance()
      }

      if (num) bytes.push(parseInt(num, 10))

      this.skipWhitespace()
      if (this.peek() === ",") this.advance()
    }

    this.advance(2) // skip >>

    const buffer = Buffer.from(bytes)

    // If it was a string literal in raw, convert to string
    if (type === "string") {
      return buffer.toString("utf8")
    }

    // For binaries that are not explicitly marked as strings, check if they're valid UTF-8
    // that contains reasonable string content (no control characters except \t, \n, \r)
    try {
      const str = buffer.toString("utf8")
      // Check if the string round-trips correctly
      if (Buffer.from(str, "utf8").equals(buffer)) {
        // It's valid UTF-8, but also check if it contains reasonable characters
        let hasReasonableChars = true
        for (let i = 0; i < str.length; i++) {
          const code = str.charCodeAt(i)
          // Allow printable chars, space, tab, newline, carriage return, and Unicode
          if (code < 0x20 && code !== 0x09 && code !== 0x0a && code !== 0x0d) {
            hasReasonableChars = false
            break
          }
        }

        if (hasReasonableChars) {
          return str
        }
      }
    } catch (e) {
      // Not valid UTF-8, keep as buffer
    }

    return buffer
  }

  parseQuotedString() {
    this.advance() // skip opening "
    let str = ""

    while (this.peek() !== '"') {
      if (this.peek() === "\\") {
        this.advance()
        const escaped = this.peek()
        switch (escaped) {
          case '"':
            str += '"'
            break
          case "\\":
            str += "\\"
            break
          case "n":
            str += "\n"
            break
          case "r":
            str += "\r"
            break
          case "t":
            str += "\t"
            break
          default:
            str += escaped
        }
        this.advance()
      } else {
        str += this.peek()
        this.advance()
      }
    }

    this.advance() // skip closing "
    return str
  }

  parseList() {
    this.advance() // skip [
    const list = []
    let listIndex = 0

    while (true) {
      this.skipWhitespace()

      if (this.peek() === "]") {
        this.advance()
        break
      }

      this.path.push(listIndex)
      list.push(this.parseValue())
      this.path.pop()

      listIndex++

      this.skipWhitespace()
      if (this.peek() === ",") {
        this.advance()
      }
    }

    return list
  }

  parseQuotedAtom() {
    this.advance() // skip '

    let atom = ""
    while (this.peek() !== "'") {
      if (this.peek() === "\\") {
        this.advance()
        const escaped = this.peek()
        switch (escaped) {
          case "'":
            atom += "'"
            break
          case "\\":
            atom += "\\"
            break
          case "n":
            atom += "\n"
            break
          case "r":
            atom += "\r"
            break
          case "t":
            atom += "\t"
            break
          default:
            atom += escaped
        }
        this.advance()
      } else {
        atom += this.peek()
        this.advance()
      }
    }

    this.advance() // skip closing '

    if (atom === "null") return null
    if (atom === "true") return true
    if (atom === "false") return false

    return Symbol.for(atom)
  }

  parseAtomOrNumber() {
    let token = ""

    // Parse unquoted atom/number
    while (this.pos < this.str.length) {
      const ch = this.peek()

      if (
        ch === "," ||
        ch === "]" ||
        ch === "}" ||
        ch === ")" ||
        (ch === "=" && this.peek(1) === ">")
      ) {
        break
      }

      if (/\s/.test(ch)) {
        let i = this.pos
        while (i < this.str.length && /\s/.test(this.str[i])) i++

        if (i >= this.str.length) break

        const nextCh = this.str[i]
        if (
          nextCh === "," ||
          nextCh === "]" ||
          nextCh === "}" ||
          nextCh === ")" ||
          (nextCh === "=" && i + 1 < this.str.length && this.str[i + 1] === ">")
        ) {
          break
        }
      }

      token += ch
      this.advance()
    }

    token = token.trim()

    if (token === "null") return null
    if (token === "true") return true
    if (token === "false") return false

    if (/^-?\d+(\.\d+)?([eE][+-]?\d+)?$/.test(token)) {
      return parseFloat(token)
    }

    return Symbol.for(token)
  }
}

// Parser implementation
class ErlangParser {
  constructor(str, binaryMode = false) {
    this.str = str
    this.pos = 0
    this.binaryMode = binaryMode
  }

  parse() {
    const result = this.parseValue()
    this.skipWhitespace()
    if (this.pos < this.str.length) {
      throw new Error(
        `Unexpected content at position ${this.pos}: ${this.str.slice(this.pos, this.pos + 50)}`
      )
    }
    return result
  }

  peek(offset = 0) {
    return this.str[this.pos + offset] || ""
  }

  advance(count = 1) {
    this.pos += count
  }

  skipWhitespace() {
    while (this.pos < this.str.length && /\s/.test(this.peek())) {
      this.advance()
    }
  }

  parseValue() {
    this.skipWhitespace()

    const ch = this.peek()
    const ch2 = this.peek(1)

    // Map: #{...}
    if (ch === "#" && ch2 === "{") {
      return this.parseMap()
    }

    // Binary: <<...>>
    if (ch === "<" && ch2 === "<") {
      return this.parseBinary()
    }

    // List: [...]
    if (ch === "[") {
      return this.parseList()
    }

    // Quoted atom: 'atom'
    if (ch === "'") {
      return this.parseQuotedAtom()
    }

    // Quoted string: "..."
    if (ch === '"') {
      return this.parseQuotedString()
    }

    // Number or unquoted atom
    return this.parseAtomOrNumber()
  }

  parseMap() {
    this.advance(2) // skip #{
    const map = {}

    while (true) {
      this.skipWhitespace()

      if (this.peek() === "}") {
        this.advance()
        break
      }

      // Parse key
      const key = this.parseValue()
      this.skipWhitespace()

      // Expect =>
      if (this.peek() !== "=" || this.peek(1) !== ">") {
        throw new Error(`Expected => at position ${this.pos}`)
      }
      this.advance(2)

      this.skipWhitespace()

      // Parse value
      const value = this.parseValue()

      // Convert key to string
      let jsKey
      if (key instanceof Buffer) {
        jsKey = key.toString("utf8")
      } else if (typeof key === "symbol") {
        jsKey = Symbol.keyFor(key) || key.description || String(key)
      } else {
        jsKey = String(key)
      }

      map[jsKey] = value

      this.skipWhitespace()
      if (this.peek() === ",") {
        this.advance()
      }
    }

    return map
  }

  parseBinary() {
    this.advance(2) // skip <<

    // String binary: <<"...">>
    if (this.peek() === '"') {
      this.advance() // skip "
      let content = ""

      while (
        !(this.peek() === '"' && this.peek(1) === ">" && this.peek(2) === ">")
      ) {
        if (this.pos >= this.str.length) {
          throw new Error("Unterminated string binary")
        }

        if (this.peek() === "\\") {
          this.advance()
          const escaped = this.peek()

          // Check for octal escape sequences \NNN
          if (/[0-7]/.test(escaped)) {
            let octal = escaped
            this.advance()

            // Get up to 2 more octal digits
            if (/[0-7]/.test(this.peek())) {
              octal += this.peek()
              this.advance()

              if (/[0-7]/.test(this.peek())) {
                octal += this.peek()
                this.advance()
              }
            }

            // Convert octal to character
            const charCode = parseInt(octal, 8)
            content += String.fromCharCode(charCode)
          } else {
            // Regular escape sequences
            switch (escaped) {
              case '"':
                content += '"'
                break
              case "\\":
                content += "\\"
                break
              case "n":
                content += "\n"
                break
              case "r":
                content += "\r"
                break
              case "t":
                content += "\t"
                break
              default:
                content += escaped
            }
            this.advance()
          }
        } else {
          content += this.peek()
          this.advance()
        }
      }

      this.advance(3) // skip ">>

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
          // Not valid base64, treat as regular string
        }
      }

      // In binary mode, return as Buffer; otherwise check if it's printable
      if (this.binaryMode) {
        return Buffer.from(content, "utf8")
      } else {
        // Check if the content is printable
        const bytes = Buffer.from(content, "utf8")
        const isPrintable = Array.from(bytes).every(b => b >= 32 && b <= 126)

        // If it contains non-printable characters, return as Buffer
        if (!isPrintable) {
          return bytes
        }

        // Otherwise return as string
        return content
      }
    }

    // Empty binary: <<>>
    if (this.peek() === ">" && this.peek(1) === ">") {
      this.advance(2)
      return Buffer.alloc(0)
    }

    // Byte binary: <<1,2,3>>
    const bytes = []
    while (!(this.peek() === ">" && this.peek(1) === ">")) {
      if (this.pos >= this.str.length) {
        throw new Error("Unterminated byte binary")
      }

      this.skipWhitespace()

      let num = ""
      while (/\d/.test(this.peek())) {
        num += this.peek()
        this.advance()
      }

      if (num) {
        bytes.push(parseInt(num, 10))
      }

      this.skipWhitespace()
      if (this.peek() === ",") {
        this.advance()
      }
    }

    this.advance(2) // skip >>
    return Buffer.from(bytes)
  }

  parseQuotedString() {
    this.advance() // skip "
    let str = ""

    while (this.peek() !== '"') {
      if (this.peek() === "\\") {
        this.advance()
        const escaped = this.peek()
        switch (escaped) {
          case '"':
            str += '"'
            break
          case "\\":
            str += "\\"
            break
          case "n":
            str += "\n"
            break
          case "r":
            str += "\r"
            break
          case "t":
            str += "\t"
            break
          default:
            str += escaped
        }
        this.advance()
      } else {
        str += this.peek()
        this.advance()
      }
    }

    this.advance() // skip closing "
    return str
  }

  parseList() {
    this.advance() // skip [
    const list = []

    while (true) {
      this.skipWhitespace()

      if (this.peek() === "]") {
        this.advance()
        break
      }

      list.push(this.parseValue())

      this.skipWhitespace()
      if (this.peek() === ",") {
        this.advance()
      }
    }

    return list
  }

  parseQuotedAtom() {
    this.advance() // skip '
    let atom = ""

    while (this.peek() !== "'") {
      if (this.pos >= this.str.length) {
        throw new Error("Unterminated quoted atom")
      }

      if (this.peek() === "\\") {
        this.advance()
        const escaped = this.peek()
        switch (escaped) {
          case "'":
            atom += "'"
            break
          case "\\":
            atom += "\\"
            break
          case "n":
            atom += "\n"
            break // Literal newline
          case "r":
            atom += "\r"
            break
          case "t":
            atom += "\t"
            break
          default:
            atom += escaped
        }
        this.advance()
      } else {
        atom += this.peek()
        this.advance()
      }
    }

    this.advance() // skip closing '

    // Special atoms that become JS primitives
    if (atom === "null") return null
    if (atom === "true") return true
    if (atom === "false") return false

    return Symbol.for(atom)
  }

  parseAtomOrNumber() {
    const startPos = this.pos
    let token = ""

    // Simple approach: collect until we hit a known delimiter
    while (this.pos < this.str.length) {
      const ch = this.peek()
      const ch2 = this.peek(1)

      // Stop at these delimiters
      if (ch === ",") break
      if (ch === "]") break
      if (ch === "}") break
      if (ch === ")") break
      if (ch === "=" && ch2 === ">") break

      // Handle backslash specially
      if (ch === "\\") {
        // In unquoted atoms, backslash is just a regular character
        token += ch
        this.advance()

        // Don't try to interpret the next character as an escape
        if (this.pos < this.str.length) {
          token += this.peek()
          this.advance()
        }
        continue
      }

      // For whitespace, check if it's trailing
      if (/\s/.test(ch)) {
        // Scan ahead to find next non-whitespace
        let i = this.pos
        while (i < this.str.length && /\s/.test(this.str[i])) {
          i++
        }

        // Check what comes after whitespace
        if (i >= this.str.length) {
          // Hit end of string
          break
        }

        const nextCh = this.str[i]
        if (
          nextCh === "," ||
          nextCh === "]" ||
          nextCh === "}" ||
          nextCh === ")"
        ) {
          // This whitespace is trailing, not part of atom
          break
        }
        if (
          nextCh === "=" &&
          i + 1 < this.str.length &&
          this.str[i + 1] === ">"
        ) {
          // Whitespace before =>
          break
        }
      }

      // Include this character in the atom
      token += ch
      this.advance()

      // Safety check to prevent infinite loops
      if (this.pos - startPos > 1000) {
        throw new Error(`Atom too long at position ${startPos}`)
      }
    }

    // Trim only trailing whitespace
    token = token.trimEnd()

    if (!token) {
      throw new Error(`Empty atom at position ${this.pos}`)
    }

    // Handle special atoms
    if (token === "null") return null
    if (token === "true") return true
    if (token === "false") return false

    // Try to parse as number
    if (/^-?\d+(\.\d+)?([eE][+-]?\d+)?$/.test(token)) {
      return parseFloat(token)
    }

    // Return as atom
    return Symbol.for(token)
  }
}

/**
 * Format JavaScript values as Erlang term strings
 * @param {*} value - JavaScript value
 * @returns {string} Erlang term string
 */
export function erl_str_to(value) {
  return formatValue(value)
}

// Formatter implementation
function formatValue(value) {
  if (value === null) return "null"
  if (value === undefined) return "undefined"
  if (typeof value === "boolean") return value.toString()

  if (typeof value === "number") {
    return value.toString()
  }

  if (typeof value === "string") {
    // Format as string binary
    return `<<"${escapeString(value)}">>`
  }

  if (typeof value === "symbol") {
    const key = Symbol.keyFor(value)
    const name = key || value.description || ""

    // Special symbols
    if (name === "null") return "null"
    if (name === "true") return "true"
    if (name === "false") return "false"
    if (name === "undefined") return "undefined"

    // Check if needs quoting
    if (/^[a-z][a-zA-Z0-9_]*$/.test(name)) {
      return name
    } else {
      return `'${escapeString(name)}'`
    }
  }

  if (value instanceof Buffer || value instanceof Uint8Array) {
    const bytes = Array.from(value)

    // Check if it's printable
    const isPrintable = bytes.every(b => b >= 32 && b <= 126)

    if (isPrintable) {
      const str = Buffer.from(value).toString()
      return `<<"${escapeString(str)}">>`
    } else {
      return `<<${bytes.join(",")}>>`
    }
  }

  if (Array.isArray(value)) {
    const items = value.map(formatValue)
    return `[${items.join(",")}]`
  }

  if (typeof value === "object" && value !== null) {
    const entries = Object.entries(value).map(([k, v]) => {
      const key = `<<"${escapeString(k)}">>`
      return `${key} => ${formatValue(v)}`
    })
    return `#{${entries.join(",")}}`
  }

  return String(value)
}

function escapeString(str) {
  return str
    .replace(/\\/g, "\\\\")
    .replace(/"/g, '\\"')
    .replace(/\n/g, "\\n")
    .replace(/\r/g, "\\r")
    .replace(/\t/g, "\\t")
}
