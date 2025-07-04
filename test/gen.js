function gen() {
  // Track used field names to avoid case conflicts
  const usedFieldNames = new Set()

  // Helper to generate random string
  const randomString = (minLen = 1, maxLen = 20) => {
    const chars =
      "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_"
    const len = Math.floor(Math.random() * (maxLen - minLen + 1)) + minLen
    let str = ""
    // First char should be letter
    str += chars[Math.floor(Math.random() * 52)]
    for (let i = 1; i < len; i++) {
      str += chars[Math.floor(Math.random() * chars.length)]
    }
    return str
  }

  // Helper to generate unique field names (avoid case conflicts)
  const uniqueFieldName = () => {
    let name
    do {
      name = randomString(3, 15)
    } while (usedFieldNames.has(name.toLowerCase()))
    usedFieldNames.add(name.toLowerCase())
    return name
  }

  // Helper to generate random symbol
  const randomSymbol = () => {
    const symbolNames = [
      "ok",
      "error",
      "undefined",
      "null",
      "true",
      "false",
      "active",
      "inactive",
      "pending",
      "success",
      "failure",
      "warning",
      "info",
      "debug",
      "trace",
      "fatal",
      "start",
      "stop",
      "pause",
      "resume",
      "reset",
      "create",
      "read",
      "update",
      "delete",
      "list",
      "open",
      "close",
      "connect",
      "disconnect",
      "send",
      "receive",
      "alpha",
      "beta",
      "gamma",
      "delta",
      "epsilon",
      "ready",
      "waiting",
      "processing",
      "completed",
      "cancelled",
      "timeout",
      "retry",
      "skip",
      "continue",
      "break",
    ]
    return Symbol(symbolNames[Math.floor(Math.random() * symbolNames.length)])
  }

  // Generate safe integer (max 32-bit signed to avoid 64-bit issues)
  const safeInteger = () => {
    const ranges = [
      () => Math.floor(Math.random() * 100), // small positive
      () => -Math.floor(Math.random() * 100), // small negative
      () => Math.floor(Math.random() * 10000), // medium positive
      () => -Math.floor(Math.random() * 10000), // medium negative
      () => Math.floor(Math.random() * 1000000), // large but safe positive
      () => -Math.floor(Math.random() * 1000000), // large but safe negative
      () => 0, // zero
      () => 1, // one
      () => -1, // negative one
      () => 2147483647, // max safe 32-bit
      () => -2147483648, // min safe 32-bit
      () => Math.floor(Math.random() * 2147483647), // random within 32-bit range
      () => -Math.floor(Math.random() * 2147483648), // random negative within 32-bit range
    ]
    return ranges[Math.floor(Math.random() * ranges.length)]()
  }

  // Generate various float patterns
  const randomFloat = () => {
    const patterns = [
      () => Math.random() * 100,
      () => -Math.random() * 100,
      () => Math.random() * 0.001, // very small
      () => Math.random() * 1000000, // large
      () => parseFloat((Math.random() * 100).toFixed(2)), // 2 decimals
      () => parseFloat((Math.random() * 100).toFixed(8)), // many decimals
      () => Math.PI,
      () => Math.E,
      () => 0.1,
      () => 0.01,
      () => 0.001,
      () => 0.0001,
      () => -0.0001,
      () => 1.23456789,
      () => -987.654321,
      () => parseFloat((Math.random() * 1000 - 500).toFixed(4)),
    ]
    return patterns[Math.floor(Math.random() * patterns.length)]()
  }

  // Generate various string patterns
  const randomStringValue = () => {
    const patterns = [
      () => randomString(1, 10), // short random
      () => randomString(20, 50), // long random
      () => "", // empty string
      () => " ", // space
      () => "  ", // spaces
      () => "\t", // tab
      () => "\n", // newline - this triggers multiline handling
      () => "Hello, World!", // common phrase
      () => "test_" + Date.now(), // timestamp
      () => "user@example.com", // email-like
      () => "https://example.com", // URL-like
      () => "192.168.1.1", // IP-like
      () => JSON.stringify({ key: "value" }), // JSON string
      () => "<tag>content</tag>", // XML-like
      () => "key=value&other=data", // query string
      () => "/path/to/file.txt", // path-like
      () => "SELECT * FROM table", // SQL-like
      () => "!@#$%^&*()_+-=[]{}|;:,.<>?", // special chars
      () => "émojis 🌟 work 🎉 too 🚀", // unicode
      () => "\\n\\t\\r\\\\", // escaped chars
      () => Array(100).fill("a").join(""), // repeated char
      () => "multi\nline\nstring", // actual multiline
      () => "The quick brown fox jumps over the lazy dog", // pangram
      () => `Lorem ipsum dolor sit amet, consectetur adipiscing elit. 
Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.`, // multiline lorem
    ]
    return patterns[Math.floor(Math.random() * patterns.length)]()
  }

  // Generate simple value (no binaries, suitable for arrays)
  const randomSimpleValue = (allowEmpty = true, allowNull = true) => {
    const types = ["string", "integer", "float", "boolean", "symbol"]
    if (allowEmpty && allowNull) {
      types.push("null", "undefined")
    }

    const type = types[Math.floor(Math.random() * types.length)]

    switch (type) {
      case "string":
        return randomStringValue()
      case "integer":
        return safeInteger()
      case "float":
        return randomFloat()
      case "boolean":
        return Math.random() > 0.5
      case "symbol":
        return randomSymbol()
      case "null":
        return null
      case "undefined":
        return undefined
    }
  }

  // Generate safe array (NO binaries, NO objects in arrays, NO null/undefined)
  const safeArray = (depth = 0, maxDepth = 5) => {
    if (depth >= maxDepth) {
      const len = Math.floor(Math.random() * 10) + 1
      return Array(len)
        .fill(0)
        .map(() => randomSimpleValue(true, false))
    }

    const len = Math.floor(Math.random() * 20)
    if (len === 0) return [] // empty array

    const arr = []
    const strategy = Math.random()

    if (strategy < 0.3) {
      // Homogeneous array
      const type = ["string", "integer", "float", "boolean", "symbol"][
        Math.floor(Math.random() * 5)
      ]
      for (let i = 0; i < len; i++) {
        switch (type) {
          case "string":
            arr.push(randomStringValue())
            break
          case "integer":
            arr.push(safeInteger())
            break
          case "float":
            arr.push(randomFloat())
            break
          case "boolean":
            arr.push(Math.random() > 0.5)
            break
          case "symbol":
            arr.push(randomSymbol())
            break
        }
      }
    } else if (strategy < 0.7) {
      // Mixed simple types (NO null/undefined in arrays)
      for (let i = 0; i < len; i++) {
        arr.push(randomSimpleValue(true, false))
      }
    } else {
      // Include nested arrays (but NO objects)
      for (let i = 0; i < len; i++) {
        if (Math.random() > 0.7 && depth < maxDepth - 1) {
          arr.push(safeArray(depth + 1, maxDepth))
        } else {
          arr.push(randomSimpleValue(true, false))
        }
      }
    }

    return arr
  }

  // Generate single binary
  const randomBinary = () => {
    const patterns = [
      () => Buffer.from([]), // empty
      () => Buffer.from([0]), // single zero
      () => Buffer.from([255]), // single max
      () => Buffer.from([1, 2, 3, 4, 5]), // sequence
      () =>
        Buffer.from(
          Array(10)
            .fill(0)
            .map(() => Math.floor(Math.random() * 256))
        ), // random
      () =>
        Buffer.from(
          Array(100)
            .fill(0)
            .map(() => Math.floor(Math.random() * 256))
        ), // large random
      () => Buffer.from("Hello, World!", "utf8"), // text
      () => Buffer.from([0x89, 0x50, 0x4e, 0x47]), // PNG header
      () => Buffer.from([0xff, 0xd8, 0xff]), // JPEG header
      () =>
        Buffer.from(
          Array(256)
            .fill(0)
            .map((_, i) => i)
        ), // all bytes
      () => Buffer.from([137, 80, 78, 71, 13, 10, 26, 10]), // full PNG signature
      () =>
        Buffer.from(
          Array(32)
            .fill(0)
            .map(() => Math.floor(Math.random() * 256))
        ), // 32 random bytes
    ]
    return patterns[Math.floor(Math.random() * patterns.length)]()
  }

  // Generate safe map (NO binaries in arrays, NO objects in arrays, max ONE binary at root)
  const safeMap = (
    depth = 0,
    maxDepth = 5,
    isRoot = true,
    hasBinary = false
  ) => {
    if (depth >= maxDepth) {
      const obj = {}
      const numKeys = Math.floor(Math.random() * 5) + 1
      for (let i = 0; i < numKeys; i++) {
        obj[uniqueFieldName()] = randomSimpleValue(true, true)
      }
      return obj
    }

    const obj = {}
    const numKeys = Math.floor(Math.random() * 15) + 1

    // Decide on structure strategy
    const strategy = Math.random()

    if (strategy < 0.15 && isRoot && !hasBinary) {
      // Single binary at root (max ONE)
      const binField = uniqueFieldName()
      obj[binField] = randomBinary()
      hasBinary = true

      // Add some other non-binary fields
      for (let i = 0; i < numKeys - 1; i++) {
        const fieldType = Math.random()
        if (fieldType < 0.3) {
          obj[uniqueFieldName()] = randomSimpleValue(true, true)
        } else if (fieldType < 0.6) {
          obj[uniqueFieldName()] = safeArray(0, maxDepth - depth)
        } else {
          obj[uniqueFieldName()] = safeMap(
            depth + 1,
            maxDepth,
            false,
            hasBinary
          )
        }
      }
    } else if (strategy < 0.3) {
      // Flat structure with simple types
      for (let i = 0; i < numKeys; i++) {
        obj[uniqueFieldName()] = randomSimpleValue(true, true)
      }
    } else if (strategy < 0.5) {
      // Include safe arrays
      for (let i = 0; i < numKeys; i++) {
        if (Math.random() > 0.5) {
          obj[uniqueFieldName()] = safeArray(0, maxDepth - depth)
        } else {
          obj[uniqueFieldName()] = randomSimpleValue(true, true)
        }
      }
    } else if (strategy < 0.7) {
      // Nested maps
      for (let i = 0; i < numKeys; i++) {
        if (Math.random() > 0.6 && depth < maxDepth - 1) {
          obj[uniqueFieldName()] = safeMap(
            depth + 1,
            maxDepth,
            false,
            hasBinary
          )
        } else if (Math.random() > 0.3) {
          obj[uniqueFieldName()] = safeArray(0, maxDepth - depth)
        } else {
          obj[uniqueFieldName()] = randomSimpleValue(true, true)
        }
      }
    } else {
      // Complex mixed structure
      const includeEmpty = Math.random() > 0.5
      const includeArrays = Math.random() > 0.3
      const includeNested = Math.random() > 0.4
      const includeMultiline = Math.random() > 0.7

      if (includeEmpty) {
        obj[uniqueFieldName()] = []
        obj[uniqueFieldName()] = {}
        obj[uniqueFieldName()] = ""
      }

      if (includeArrays) {
        obj[uniqueFieldName()] = safeArray(0, 3)
        obj[uniqueFieldName()] = [[], [[]], [[], []]] // nested empty arrays
        obj[uniqueFieldName()] = Array(10)
          .fill(0)
          .map(() => safeInteger())
        obj[uniqueFieldName()] = Array(5)
          .fill(0)
          .map(() => randomFloat())
      }

      if (includeNested && depth < maxDepth - 1) {
        obj[uniqueFieldName()] = safeMap(depth + 1, maxDepth, false, hasBinary)
        obj[uniqueFieldName()] = {
          [uniqueFieldName()]: safeMap(depth + 2, maxDepth, false, hasBinary),
          [uniqueFieldName()]: safeArray(0, 2),
        }
      }

      if (includeMultiline) {
        obj[uniqueFieldName()] = `This is a
multiline string that will
require special handling in
the encoding process.`
      }

      // Special patterns that are known to work
      if (isRoot && Math.random() > 0.8) {
        // data/body pattern (from working examples)
        if (Math.random() > 0.5 && !hasBinary) {
          obj.data = { body: randomBinary() }
          obj.body = { data: randomBinary() }
          hasBinary = true
        } else {
          // Simple nested structure
          obj.data = {
            test: safeArray(0, 2),
            value: randomSimpleValue(),
            nested: { item: randomFloat() },
          }
        }
      }

      // Fill remaining with mixed values
      while (Object.keys(obj).length < numKeys) {
        const fieldType = Math.random()
        if (fieldType < 0.4) {
          obj[uniqueFieldName()] = randomSimpleValue(true, true)
        } else if (fieldType < 0.7) {
          obj[uniqueFieldName()] = safeArray(0, 2)
        } else if (depth < maxDepth - 1) {
          obj[uniqueFieldName()] = safeMap(
            depth + 1,
            maxDepth,
            false,
            hasBinary
          )
        } else {
          obj[uniqueFieldName()] = randomSimpleValue(true, true)
        }
      }
    }

    return obj
  }

  // Main structure generators
  const structures = [
    // Simple flat map
    () => safeMap(0, 1),

    // Deep nested structure
    () => safeMap(0, 5),

    // Array-heavy structure
    () => {
      const obj = {}
      const numArrays = Math.floor(Math.random() * 8) + 3
      for (let i = 0; i < numArrays; i++) {
        obj[uniqueFieldName()] = safeArray(0, 4)
      }
      // Add some scalars
      obj[uniqueFieldName()] = randomSimpleValue()
      obj[uniqueFieldName()] = safeInteger()
      obj[uniqueFieldName()] = randomFloat()
      return obj
    },

    // Mixed complex structure
    () => {
      const obj = {}
      // Add various patterns
      obj[uniqueFieldName()] = safeArray(0, 3)
      obj[uniqueFieldName()] = safeMap(1, 3, false)
      obj[uniqueFieldName()] = randomStringValue()
      obj[uniqueFieldName()] = safeInteger()
      obj[uniqueFieldName()] = randomFloat()
      obj[uniqueFieldName()] = Math.random() > 0.5
      obj[uniqueFieldName()] = randomSymbol()
      obj[uniqueFieldName()] = null
      obj[uniqueFieldName()] = undefined
      if (Math.random() > 0.5) {
        obj[uniqueFieldName()] = []
        obj[uniqueFieldName()] = {}
      }
      return obj
    },

    // Single binary with other fields (max ONE binary)
    () => {
      const obj = {}
      obj[uniqueFieldName()] = randomBinary()
      // Add other non-binary fields
      for (let i = 0; i < 5; i++) {
        if (Math.random() > 0.5) {
          obj[uniqueFieldName()] = randomSimpleValue()
        } else {
          obj[uniqueFieldName()] = safeArray(0, 2)
        }
      }
      return obj
    },

    // Empty containers focus
    () => ({
      [uniqueFieldName()]: [],
      [uniqueFieldName()]: {},
      [uniqueFieldName()]: "",
      [uniqueFieldName()]: randomSimpleValue(),
      [uniqueFieldName()]: [[], [[]], [[[]]]],
      [uniqueFieldName()]: { [uniqueFieldName()]: {}, [uniqueFieldName()]: [] },
    }),

    // Deeply nested arrays (no objects in arrays)
    () => {
      const createNestedArray = d => {
        if (d <= 0) return randomSimpleValue(true, false)
        if (Math.random() > 0.5) {
          return Array(Math.floor(Math.random() * 5) + 1)
            .fill(0)
            .map(() => createNestedArray(d - 1))
        }
        return randomSimpleValue(true, false)
      }
      return {
        [uniqueFieldName()]: createNestedArray(5),
        [uniqueFieldName()]: randomSimpleValue(),
        [uniqueFieldName()]: safeArray(0, 3),
      }
    },

    // Large structures
    () => ({
      [uniqueFieldName()]: Array(50)
        .fill(0)
        .map(() => safeInteger()), // large array
      [uniqueFieldName()]: {
        ...Array(20)
          .fill(0)
          .reduce(
            (acc, _, i) => ({
              ...acc,
              [uniqueFieldName()]: randomSimpleValue(),
            }),
            {}
          ),
      }, // many keys
      [uniqueFieldName()]: safeArray(0, 2),
      [uniqueFieldName()]: randomFloat(),
    }),

    // Working patterns from examples
    () => {
      const patterns = [
        // Pattern from working examples
        {
          str: randomStringValue(),
          bool: Math.random() > 0.5,
          num: safeInteger(),
          float: randomFloat(),
          atom: randomSymbol(),
          list: safeArray(0, 2),
          body: randomBinary(),
        },
        // Nested maps
        {
          map: {
            a: safeInteger(),
            b: randomStringValue(),
            c: { d: { e: randomFloat() } },
          },
        },
        // Complex nested with arrays
        {
          data: {
            test: [
              randomFloat(),
              randomStringValue(),
              safeInteger(),
              randomSymbol(),
              safeArray(0, 2),
            ],
          },
        },
        // Mixed types
        {
          complex: {
            nested: {
              array: [true, false, randomStringValue(), safeInteger()],
              empty_map: {},
              symbol: randomSymbol(),
            },
            simple: randomStringValue(),
          },
        },
      ]
      return patterns[Math.floor(Math.random() * patterns.length)]
    },

    // Multiline strings focus
    () => ({
      [uniqueFieldName()]: `First line
Second line
Third line`,
      [uniqueFieldName()]: "Regular string",
      [uniqueFieldName()]: `Another
multiline
string with ${Math.floor(Math.random() * 100)} lines`,
      [uniqueFieldName()]: safeArray(0, 2),
      [uniqueFieldName()]: { nested: randomSimpleValue() },
    }),
  ]

  // Clear used field names for fresh generation
  usedFieldNames.clear()

  // Select and generate structure
  return structures[Math.floor(Math.random() * structures.length)]()
}

// Generate multiple test cases
function generateTestCases(count = 10) {
  return Array(count)
    .fill(0)
    .map(() => gen())
}

// Validate that a test case follows safe patterns
function isValidTestCase(obj) {
  const checkObj = (o, path = "", isRoot = true) => {
    // Check field name conflicts
    const fieldNames = Object.keys(o)
    const lowerNames = fieldNames.map(n => n.toLowerCase())
    if (lowerNames.length !== new Set(lowerNames).size) {
      console.warn(`Field name case conflict at ${path}`)
      return false
    }

    // Count binaries at root
    let rootBinaryCount = 0

    for (const [key, value] of Object.entries(o)) {
      const currentPath = path ? `${path}.${key}` : key

      // Check for binaries
      if (Buffer.isBuffer(value)) {
        if (isRoot) {
          rootBinaryCount++
          if (rootBinaryCount > 1) {
            console.warn(`Multiple binaries at root level`)
            return false
          }
        }
      }

      // Check arrays
      if (Array.isArray(value)) {
        for (let i = 0; i < value.length; i++) {
          const item = value[i]

          // No null/undefined in arrays
          if (item === null || item === undefined) {
            console.warn(`Null/undefined in array at ${currentPath}[${i}]`)
            return false
          }

          // No binaries in arrays
          if (Buffer.isBuffer(item)) {
            console.warn(`Binary in array at ${currentPath}[${i}]`)
            return false
          }

          // No objects in arrays (except nested arrays)
          if (item && typeof item === "object" && !Array.isArray(item)) {
            console.warn(`Object in array at ${currentPath}[${i}]`)
            return false
          }

          // Recursively check nested arrays
          if (Array.isArray(item)) {
            if (!checkArray(item, `${currentPath}[${i}]`)) return false
          }
        }
      }

      // Check for large integers
      if (typeof value === "number" && Number.isInteger(value)) {
        if (Math.abs(value) > 2147483647) {
          console.warn(`Large integer at ${currentPath}: ${value}`)
          return false
        }
      }

      // Recursively check nested objects
      if (
        value &&
        typeof value === "object" &&
        !Array.isArray(value) &&
        !Buffer.isBuffer(value)
      ) {
        if (!checkObj(value, currentPath, false)) return false
      }
    }

    return true
  }

  const checkArray = (arr, path) => {
    for (let i = 0; i < arr.length; i++) {
      const item = arr[i]
      if (item === null || item === undefined) {
        console.warn(`Null/undefined in nested array at ${path}[${i}]`)
        return false
      }
      if (Buffer.isBuffer(item)) {
        console.warn(`Binary in nested array at ${path}[${i}]`)
        return false
      }
      if (item && typeof item === "object" && !Array.isArray(item)) {
        console.warn(`Object in nested array at ${path}[${i}]`)
        return false
      }
      if (Array.isArray(item)) {
        if (!checkArray(item, `${path}[${i}]`)) return false
      }
    }
    return true
  }

  return checkObj(obj)
}

// Export for use
export { gen, generateTestCases, isValidTestCase }
