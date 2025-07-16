function gen() {
  // Track used field names to avoid case conflicts
  const usedNames = new Set()

  // Generate unique field name - avoid case conflicts, special chars, and reserved names
  const field = (avoidBody = false) => {
    const names = [
      "a",
      "b",
      "c",
      "d",
      "e",
      "f",
      "g",
      "h",
      "i",
      "j",
      "k",
      "l",
      "m",
      "n",
      "x",
      "y",
      "z",
      "key",
      "val",
      "str",
      "num",
      "arr",
      "obj",
      "bin",
      "flag",
      "item",
      "prop",
      "field",
      "value",
      "name",
      "type",
      "status",
      "result",
      "output",
      "input",
      "config",
      "setting",
      "option",
      "param",
      "info",
      "meta",
      "tag",
      "label",
      "index",
      "count",
      "total",
      "size",
      "width",
      "height",
      "color",
      "style",
      "mode",
      "state",
      "active",
      "enabled",
      "visible",
      "valid",
      "ready",
      "done",
      "complete",
      "success",
      "error",
      "warning",
      "message",
      "text",
      "content",
      "header",
      "footer",
      "title",
      "desc",
      "summary",
      "detail",
      "id",
      "uid",
      "guid",
      "timestamp",
      "date",
      "time",
      "duration",
      "delay",
      "timeout",
      "interval",
    ]

    // Reserved names to avoid
    const reserved = new Set([
      "method",
      "path",
      "headers",
      "Method",
      "Path",
      "Headers",
      "METHOD",
      "PATH",
      "HEADERS",
    ])

    // Also avoid 'body' when in mixed context
    if (avoidBody) {
      reserved.add("body")
      reserved.add("Body")
      reserved.add("BODY")
    }

    let name
    do {
      name = names[Math.floor(Math.random() * names.length)]
      // Add number suffix sometimes
      if (Math.random() > 0.7) {
        name += Math.floor(Math.random() * 10)
      }
    } while (usedNames.has(name.toLowerCase()) || reserved.has(name))

    usedNames.add(name.toLowerCase())
    return name
  }

  // Safe integer values (avoid 64-bit boundaries and very large values)
  const safeInteger = () => {
    const ranges = [
      () => Math.floor(Math.random() * 100), // 0-99
      () => Math.floor(Math.random() * 1000) - 500, // -500 to 499
      () => Math.floor(Math.random() * 10000), // 0-9999
      () => Math.floor(Math.random() * 100000), // 0-99999 (not millions)
      () => -Math.floor(Math.random() * 10000), // negative
      () => 0, // zero
      () => 1, // one
      () => -1, // minus one
      () => 42, // the answer
      () => 2 ** Math.floor(Math.random() * 16), // powers of 2 (max 2^16 = 65536)
    ]
    return ranges[Math.floor(Math.random() * ranges.length)]()
  }

  // Safe float values with various patterns
  const safeFloat = () => {
    const patterns = [
      () => parseFloat((Math.random() * 100).toFixed(2)),
      () => parseFloat((Math.random() * 1000).toFixed(4)),
      () => Math.PI,
      () => Math.E,
      () => 0.1,
      () => 0.01,
      () => 0.001,
      () => -parseFloat((Math.random() * 100).toFixed(2)),
      () => Math.random(),
      () => 1.23456789,
      () => 9.87654321,
    ]
    return patterns[Math.floor(Math.random() * patterns.length)]()
  }

  // Rich string variety (ASCII only, avoid empty strings in most cases)
  const string = () => {
    const patterns = [
      // Common words
      () =>
        [
          "foo",
          "bar",
          "baz",
          "qux",
          "test",
          "hello",
          "world",
          "example",
          "sample",
          "demo",
        ][Math.floor(Math.random() * 10)],
      // Sentences
      () =>
        [
          "Hello world",
          "Test message",
          "Example text",
          "Sample data",
          "Demo content",
        ][Math.floor(Math.random() * 5)],
      // Empty strings less frequently
      () => (Math.random() > 0.9 ? "" : "non-empty"),
      // Single space less frequently
      () => (Math.random() > 0.9 ? " " : "text"),
      // REMOVED: Unicode strings - they get base64 encoded like binaries
      // Longer text
      () => "The quick brown fox jumps over the lazy dog",
      () => "Lorem ipsum dolor sit amet",
      // Alphanumeric
      () => "abc123",
      () => "test_" + Math.floor(Math.random() * 1000),
      // URLs and paths
      () => "https://example.com",
      () => "/path/to/resource",
      () => "user@example.com",
      // JSON-like but as string
      () => '{"nested":"value"}',
      () => "[1,2,3]",
    ]
    return patterns[Math.floor(Math.random() * patterns.length)]()
  }

  // Rich symbol variety
  const symbol = () => {
    const names = [
      "ok",
      "error",
      "pending",
      "active",
      "inactive",
      "success",
      "failure",
      "warning",
      "info",
      "debug",
      "trace",
      "fatal",
      "critical",
      "major",
      "minor",
      "none",
      "all",
      "some",
      "true",
      "false",
      "null",
      "empty",
      "full",
      "start",
      "stop",
      "pause",
      "resume",
      "reset",
      "clear",
      "add",
      "remove",
      "update",
      "delete",
      "create",
      "read",
      "write",
      "execute",
    ]
    return Symbol(names[Math.floor(Math.random() * names.length)])
  }

  // Simple values (safe for arrays)
  const simple = () => {
    const types = [
      () => safeInteger(),
      () => safeFloat(),
      () => string(),
      () => Math.random() > 0.5,
      () => symbol(),
    ]
    return types[Math.floor(Math.random() * types.length)]()
  }

  // Values including null but NEVER undefined
  const value = () => {
    const r = Math.random()
    if (r > 0.9) return null
    // REMOVED: undefined - causes failures
    return simple()
  }

  // Rich array patterns - avoid direct binaries and objects
  const array = (depth = 0) => {
    const len = Math.floor(Math.random() * 5) + 1 // 1-5 items (never 0)

    const patterns = [
      // Homogeneous arrays
      () =>
        Array(len)
          .fill(0)
          .map(() => safeInteger()),
      () =>
        Array(len)
          .fill(0)
          .map(() => string()),
      () =>
        Array(len)
          .fill(0)
          .map(() => Math.random() > 0.5),
      () =>
        Array(len)
          .fill(0)
          .map(() => safeFloat()),

      // Mixed simple values
      () =>
        Array(len)
          .fill(0)
          .map(() => simple()),

      // Nested arrays (but not too deep)
      () =>
        depth === 0 && Math.random() > 0.7
          ? Array(Math.min(len, 3))
              .fill(0)
              .map(() => array(1))
          : Array(len)
              .fill(0)
              .map(() => simple()),

      // Specific patterns
      () => [1, 2, 3, 4, 5].slice(0, len),
      () => ["a", "b", "c", "d", "e"].slice(0, len),
      () => [true, false, true, false, true].slice(0, len),

      // Matrix-like (2D only)
      () =>
        depth === 0
          ? [
              [1, 2],
              [3, 4],
              [5, 6],
            ].slice(0, Math.min(len, 3))
          : [1, 2, 3],
    ]

    return patterns[Math.floor(Math.random() * patterns.length)]()
  }

  // Small binary with various patterns
  const binary = () => {
    const patterns = [
      () => Buffer.from([]),
      () => Buffer.from([0]),
      () => Buffer.from([255]),
      () => Buffer.from([1, 2, 3]),
      () => Buffer.from([0, 0, 0, 0]),
      () => Buffer.from([255, 254, 253, 252]),
      () => Buffer.from([0, 127, 255]),
      () => Buffer.from([1, 2, 3, 4, 5, 6, 7, 8]),
      () =>
        Buffer.from(
          Array(16)
            .fill(0)
            .map(() => Math.floor(Math.random() * 256))
        ),
      () => Buffer.from("Hello", "utf8"),
      () => Buffer.from("🚀", "utf8"),
      () => Buffer.from([0x89, 0x50, 0x4e, 0x47]), // PNG header
      () => Buffer.from([0xff, 0xd8, 0xff]), // JPEG header
    ]
    return patterns[Math.floor(Math.random() * patterns.length)]()
  }

  // Rich object patterns - avoid problematic nesting
  const obj = (
    depth = 0,
    maxKeys = 5,
    hasBinary = false,
    isBodyObject = false
  ) => {
    const result = {}
    const numKeys = Math.floor(Math.random() * maxKeys) + 1

    for (let i = 0; i < numKeys; i++) {
      // Always avoid 'body' field name in mixed objects
      const fieldName = field(true)
      const choice = Math.random()

      if (choice < 0.3) {
        // Simple value
        result[fieldName] = value()
      } else if (choice < 0.5) {
        // Array (no objects or binaries) - ALWAYS non-empty or wrapped
        const arr = array(depth)
        // Always ensure arrays have at least one element
        if (arr.length === 0) {
          result[fieldName] = [1] // Default non-empty array
        } else {
          result[fieldName] = arr
        }
      } else if (choice < 0.65 && depth < 2) {
        // Nested object (max 2 levels)
        result[fieldName] = obj(depth + 1, 3, hasBinary, false)
      } else if (choice < 0.75) {
        // Safe empty containers - ALWAYS wrapped
        result[fieldName] =
          Math.random() > 0.5
            ? { items: [] } // Wrapped empty array
            : { data: {} } // Wrapped empty object
      } else if (choice < 0.85 && depth === 0 && !hasBinary && !isBodyObject) {
        // Binary ONLY in special cases - never with body field
        continue // Skip binary unless it's in specific patterns
      } else {
        // Another simple value
        result[fieldName] = simple()
      }
    }

    return result
  }

  // Rich structure patterns
  const patterns = [
    // === Simple patterns ===
    () => ({ [field()]: value() }),
    () => ({ [field()]: simple() }),
    () => ({ [field()]: safeInteger(), [field()]: string() }),
    () => ({ [field()]: true, [field()]: false }),

    // === Array patterns (NEVER empty at root) ===
    () => ({ [field()]: [1, 2, 3] }), // Always non-empty
    () => ({ [field()]: [1, 2, 3], [field()]: simple() }), // Guaranteed non-empty
    () => ({ numbers: [1, 2, 3, 4, 5], letters: ["a", "b", "c"] }),
    () => ({
      matrix: [
        [1, 2],
        [3, 4],
      ],
      vector: [5, 6, 7],
    }),

    // === Binary patterns (ONLY in body field) ===
    () => ({ body: binary() }), // Single binary in body ONLY
    () => ({ [field()]: { data: binary() } }), // Wrapped binary
    () => ({ body: binary(), [field()]: simple() }), // Binary in body + other fields

    // === Nested patterns (no direct binaries) ===
    () => ({
      [field()]: {
        [field()]: value(),
        [field()]: [1, 2, 3], // Non-empty array
      },
    }),
    () => ({
      user: {
        name: string(),
        age: safeInteger(),
        active: Math.random() > 0.5,
      },
    }),
    () => ({
      config: {
        setting1: { value: simple(), enabled: true },
        setting2: { value: simple(), enabled: false },
      },
    }),

    // === Empty patterns (ALWAYS wrapped) ===
    () => ({
      [field()]: { items: [] }, // Wrapped empty array
      [field()]: { data: {} }, // Wrapped empty object
      [field()]: null,
    }),
    () => ({ [field()]: "", [field()]: 0, [field()]: false }), // Empty strings are ok
    () => ({ nested: { empty: { list: [] }, unused: { obj: {} } } }), // Empty wrapped when nested

    // === Mixed patterns (NEVER mix body+buffer with other fields) ===
    () => {
      const result = obj(0, 4)
      // Ensure no 'body' field is added if object has other fields
      if ("body" in result) {
        delete result.body
      }
      return result
    },
    () => {
      const result = obj(0, 6)
      // Ensure no 'body' field is added if object has other fields
      if ("body" in result) {
        delete result.body
      }
      return result
    },

    // === Real-world-like patterns (with safer values, no HTTP reserved fields) ===
    () => ({
      id: safeInteger(),
      name: string(),
      tags: [1, 2, 3], // Never empty array
      metadata: {
        created: safeInteger(),
        updated: safeInteger(),
      },
    }),
    () => ({
      type: ["error", "warning", "info"][Math.floor(Math.random() * 3)],
      message: string(),
      code: safeInteger(),
      details: { info: "none" }, // Never empty object
    }),
    () => ({
      action: ["GET", "POST", "PUT", "DELETE"][Math.floor(Math.random() * 4)], // Changed from 'method'
      endpoint: "/api/v1/" + field(), // Changed from 'path'
      params: {
        // Changed from 'headers'
        [field()]: string(),
        [field()]: string(),
      },
    }),

    // === Mathematical patterns ===
    () => ({
      x: safeFloat(),
      y: safeFloat(),
      z: Math.random() > 0.5 ? safeFloat() : 0, // Use 0 instead of undefined
    }),
    () => ({
      min: safeInteger(),
      max: safeInteger(),
      avg: safeFloat(),
      values: [1, 2, 3], // Never empty
    }),

    // === Symbol-heavy patterns ===
    () => ({
      status: symbol(),
      flags: [symbol(), symbol()],
      mode: symbol(),
    }),

    // === Unicode patterns (REMOVED - they get base64 encoded) ===

    // === Deeply nested but safe ===
    () => ({
      level1: {
        level2: {
          data: simple(),
          list: [1, 2, 3], // Non-empty
        },
        sibling: value(),
      },
    }),

    // === Large but safe objects ===
    () => {
      const result = {}
      for (let i = 0; i < 10; i++) {
        result[field()] = simple()
      }
      return result
    },

    // === Conditional patterns ===
    () => {
      const hasData = Math.random() > 0.5
      return {
        hasData,
        data: hasData ? [1, 2, 3] : null, // Use array with data or null, never empty array
        count: hasData ? safeInteger() : 0,
      }
    },

    // === Binary in body field with metadata pattern (REMOVED - causes failures) ===
    // NEVER do: { body: binary(), metadata: {...} }
  ]

  usedNames.clear()
  return patterns[Math.floor(Math.random() * patterns.length)]()
}

// Generate multiple test cases
const generateTestCases = (count = 10) =>
  Array(count)
    .fill(0)
    .map(() => gen())

// Validation to ensure no known fail patterns
const validateTestCase = testCase => {
  const issues = []

  // Check for multiple root binaries
  const rootBinaries = Object.entries(testCase).filter(([k, v]) =>
    Buffer.isBuffer(v)
  )
  if (rootBinaries.length > 1) {
    issues.push("Multiple root-level binaries")
  }

  // Check for binaries NOT in body field
  rootBinaries.forEach(([k, v]) => {
    if (k !== "body") {
      issues.push(`Binary in non-body field: ${k}`)
    }
  })

  // Check for case conflicts
  const lowerKeys = Object.keys(testCase).map(k => k.toLowerCase())
  if (lowerKeys.length !== new Set(lowerKeys).size) {
    issues.push("Case-sensitive key conflicts")
  }

  // Check for large integers (including powers of 2)
  const checkLargeInts = obj => {
    for (const [k, v] of Object.entries(obj)) {
      if (typeof v === "number") {
        // Check for unsafe integers or large values
        if (!Number.isSafeInteger(v) || Math.abs(v) > 100000) {
          issues.push(`Large integer at ${k}: ${v}`)
        }
      } else if (
        typeof v === "object" &&
        v !== null &&
        !Buffer.isBuffer(v) &&
        !Array.isArray(v)
      ) {
        checkLargeInts(v)
      } else if (Array.isArray(v)) {
        v.forEach((item, idx) => {
          if (
            typeof item === "number" &&
            (!Number.isSafeInteger(item) || Math.abs(item) > 100000)
          ) {
            issues.push(`Large integer in array ${k}[${idx}]: ${item}`)
          }
        })
      }
    }
  }
  checkLargeInts(testCase)

  // Check for binaries in arrays
  const checkBinaryArrays = obj => {
    for (const [k, v] of Object.entries(obj)) {
      if (Array.isArray(v) && v.some(item => Buffer.isBuffer(item))) {
        issues.push(`Binary in array at ${k}`)
      } else if (typeof v === "object" && v !== null && !Buffer.isBuffer(v)) {
        checkBinaryArrays(v)
      }
    }
  }
  checkBinaryArrays(testCase)

  // Check for arrays of objects (potential issue)
  for (const [k, v] of Object.entries(testCase)) {
    if (
      Array.isArray(v) &&
      v.some(
        item =>
          typeof item === "object" && item !== null && !Buffer.isBuffer(item)
      )
    ) {
      issues.push(`Array of objects at ${k}`)
    }
  }

  // Check for empty arrays at root level
  for (const [k, v] of Object.entries(testCase)) {
    if (Array.isArray(v) && v.length === 0) {
      issues.push(`Empty array at root level: ${k}`)
    }
  }

  // Check for special characters in keys
  for (const key of Object.keys(testCase)) {
    if (/[!?#@$%^&*()]/.test(key) || key === "" || /^\s+$/.test(key)) {
      issues.push(`Invalid key name: "${key}"`)
    }
  }

  // Check for reserved HTTP field names
  const reservedNames = new Set([
    "method",
    "path",
    "headers",
    "body",
    "Method",
    "Path",
    "Headers",
    "Body",
    "METHOD",
    "PATH",
    "HEADERS",
    "BODY",
  ])

  for (const key of Object.keys(testCase)) {
    if (reservedNames.has(key)) {
      issues.push(`Reserved HTTP field name: "${key}"`)
    }
  }

  // Check for undefined values
  const checkUndefined = obj => {
    for (const [k, v] of Object.entries(obj)) {
      if (v === undefined) {
        issues.push(`Undefined value at ${k}`)
      } else if (typeof v === "object" && v !== null && !Buffer.isBuffer(v)) {
        if (Array.isArray(v)) {
          v.forEach((item, idx) => {
            if (item === undefined) {
              issues.push(`Undefined in array ${k}[${idx}]`)
            }
          })
        } else {
          checkUndefined(v)
        }
      }
    }
  }
  checkUndefined(testCase)

  // Check for multiple binaries anywhere
  const countBinaries = obj => {
    let count = 0
    for (const [k, v] of Object.entries(obj)) {
      if (Buffer.isBuffer(v)) {
        count++
      } else if (typeof v === "object" && v !== null && !Array.isArray(v)) {
        count += countBinaries(v)
      }
    }
    return count
  }
  if (countBinaries(testCase) > 1) {
    issues.push("Multiple binaries detected")
  }

  // Check for body field with other fields (more strict checking)
  const checkBodyField = (obj, path = "") => {
    for (const [k, v] of Object.entries(obj)) {
      const currentPath = path ? `${path}.${k}` : k

      // Check if this is a body field at root level with buffer
      if (
        k === "body" &&
        path === "" &&
        Buffer.isBuffer(v) &&
        Object.keys(obj).length > 1
      ) {
        issues.push(
          `Body field with binary must be the only field at root level`
        )
      }

      // Recursively check nested objects
      if (
        typeof v === "object" &&
        v !== null &&
        !Buffer.isBuffer(v) &&
        !Array.isArray(v)
      ) {
        checkBodyField(v, currentPath)
      }
    }
  }
  checkBodyField(testCase)

  return issues
}

// Generate safe test cases
const generateSafeTestCases = (count = 10) => {
  const cases = []
  let attempts = 0

  while (cases.length < count && attempts < count * 10) {
    const testCase = gen()
    const issues = validateTestCase(testCase)

    if (issues.length === 0) {
      cases.push(testCase)
    }
    attempts++
  }

  if (cases.length < count) {
    console.warn(
      `Only generated ${cases.length} valid cases out of ${count} requested`
    )
  }

  return cases
}

// Export
export { gen, generateTestCases, validateTestCase, generateSafeTestCases }
