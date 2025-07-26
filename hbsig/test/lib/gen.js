/**
 * Generate versatile JSON objects for fuzz testing the Erlang JSON codec
 * This generator creates edge cases and various combinations to stress test the system
 */

// Helper function to get random element from array
function randomChoice(arr) {
  return arr[Math.floor(Math.random() * arr.length)]
}

// Helper function to generate random integer in range
function randomInt(min, max) {
  return Math.floor(Math.random() * (max - min + 1)) + min
}

// Generate a random string with various edge cases
function generateString() {
  const choices = [
    // Empty string (edge case)
    "",
    // Single character
    "a",
    // ASCII printable
    "Hello World",
    // With special characters
    "Line1\nLine2\tTab\rReturn",
    // With quotes and backslashes
    'He said "Hello" and \\escaped\\',
    // Numbers as strings
    "123",
    "3.14",
    "-42",
    // Boolean-like strings
    "true",
    "false",
    "null",
    "undefined",
    // Long string
    "Lorem ipsum dolor sit amet, consectetur adipiscing elit. ".repeat(10),
    // Unicode (though it will be converted to bytes)
    "Hello 世界 😀",
    // Special structured field patterns
    ":base64:",
    // '%token%', // This causes issues - interpreted as atom marker
    "::",
    // '%%', // This causes issues - empty token marker
    // Path-like
    "/path/to/file.txt",
    // URL-like
    "https://example.com/path?query=value",
    // JSON-like string
    '{"key": "value"}',
    // Array-like string
    "[1, 2, 3]",
    // With null bytes (will be encoded)
    "before\x00after",
    // All whitespace
    "   \t\n\r   ",
    // Mixed case
    "CamelCase_snake_case-kebab-case",
  ]

  return randomChoice(choices)
}

// Generate a random buffer with various patterns
function generateBuffer() {
  const choices = [
    // Empty buffer (edge case)
    Buffer.alloc(0),
    // Single byte
    Buffer.from([0]),
    Buffer.from([255]),
    Buffer.from([128]),
    // ASCII text
    Buffer.from("Hello World"),
    // Binary data
    Buffer.from([0, 1, 2, 3, 255, 254, 253]),
    // All zeros
    Buffer.alloc(10),
    // All 255s
    Buffer.alloc(10, 255),
    // Pattern
    Buffer.from([0, 255, 0, 255, 0, 255]),
    // Looks like UTF-8 but isn't valid
    Buffer.from([0xff, 0xfe, 0xfd]),
    // Valid UTF-8 for non-ASCII
    Buffer.from("Hello 世界", "utf8"),
    // Float bytes
    Buffer.from(Float64Array.from([3.14159]).buffer),
    // Large buffer
    Buffer.alloc(1000, 42),
    // Printable ASCII that looks like base64
    Buffer.from("SGVsbG8gV29ybGQ="),
    // Bytes that decode to special strings
    Buffer.from("true"),
    Buffer.from("null"),
    Buffer.from("undefined"),
  ]

  return randomChoice(choices)
}

// Generate a random symbol - ONLY use global symbols or special ones
function generateSymbol() {
  const choices = [
    // Special symbols that convert to primitives
    // These are handled specially by normalize()
    Symbol("null"),
    Symbol("true"),
    Symbol("false"),
    Symbol("undefined"),
    // ALL other symbols MUST be global to round-trip properly
    Symbol.for("ok"),
    Symbol.for("error"),
    Symbol.for("atom"),
    Symbol.for("empty"),
    Symbol.for("space"),
    Symbol.for("simple"),
    Symbol.for("with spaces"),
    Symbol.for("with-dashes"),
    Symbol.for("with_underscores"),
    Symbol.for("123numeric"),
    Symbol.for("CamelCase"),
    Symbol.for("global"),
    Symbol.for("also-global"),
    Symbol.for('with"quotes'),
    Symbol.for("with\\backslash"),
    Symbol.for("with\nnewline"),
  ]

  return randomChoice(choices)
}

// Generate a random number (no Infinity, -Infinity, or NaN)
function generateNumber() {
  const choices = [
    // Integers
    0,
    1,
    -1,
    42,
    255,
    256,
    -256,
    // JavaScript number limits
    Number.MAX_SAFE_INTEGER,
    Number.MIN_SAFE_INTEGER,
    9223372036854776000, // Close to max int64
    -9223372036854776000,
    // Floats
    3.14,
    -3.14,
    0.1,
    -0.1,
    1.23e10,
    1.23e-10,
    // Edge cases
    0.0,
    -0.0,
    0.999999999999999,
    Number.EPSILON,
    // Very large/small but not Infinity
    1e308,
    1e-308,
  ]

  return randomChoice(choices)
}

// Generate a random primitive value
function generatePrimitive() {
  const types = [
    "string",
    "buffer",
    "number",
    "boolean",
    "null",
    "undefined",
    "symbol",
  ]
  const type = randomChoice(types)

  switch (type) {
    case "string":
      return generateString()
    case "buffer":
      return generateBuffer()
    case "number":
      return generateNumber()
    case "boolean":
      return randomChoice([true, false])
    case "null":
      return null
    case "undefined":
      return undefined
    case "symbol":
      return generateSymbol()
  }
}

// Generate a random array
function generateArray(depth = 0, maxDepth = 3) {
  if (depth >= maxDepth) {
    return []
  }

  const choices = [
    // Empty array
    () => [],
    // Single element
    () => [generatePrimitive()],
    // Homogeneous arrays
    () => [1, 2, 3, 4, 5],
    () => ["a", "b", "c"],
    () => [true, false, true],
    () => [Buffer.from("a"), Buffer.from("b"), Buffer.from("c")],
    // Mixed types - use generateSymbol() for symbols
    () => [1, "two", Buffer.from("three"), true, null, generateSymbol()],
    // With undefined (which should be preserved in arrays)
    // Actually, undefined in arrays causes issues with JSON serialization
    // () => [1, undefined, 3],
    // Nested arrays
    () =>
      depth < maxDepth - 1
        ? [
            [1, 2],
            [3, 4],
            [5, 6],
          ]
        : [1, 2, 3],
    // Array with objects
    () => (depth < maxDepth - 1 ? [{ a: 1 }, { b: 2 }, { c: 3 }] : [1, 2, 3]),
    // Large array
    () => Array(100).fill(42),
    // Array with all types - use generators
    () => [
      0,
      -1,
      3.14,
      "",
      "hello",
      Buffer.alloc(0),
      Buffer.from("data"),
      true,
      false,
      null,
      undefined,
      generateSymbol(),
      generateSymbol(),
      [],
      {},
    ],
  ]

  const choice = randomChoice(choices)
  const arr = choice()

  // Sometimes add nested structures
  if (depth < maxDepth - 1 && Math.random() < 0.3) {
    arr.push(generateArray(depth + 1, maxDepth))
    arr.push(generateObject(depth + 1, maxDepth))
  }

  return arr
}

// Generate a random object
function generateObject(depth = 0, maxDepth = 3) {
  if (depth >= maxDepth) {
    return {}
  }

  const choices = [
    // Empty object
    () => ({}),
    // Single key
    () => ({ key: generatePrimitive() }),
    // Multiple keys with same type
    () => ({ a: 1, b: 2, c: 3 }),
    () => ({ x: "hello", y: "world", z: "test" }),
    // Mixed types - use generators
    () => ({
      string: "hello",
      number: 42,
      float: 3.14,
      buffer: Buffer.from("data"),
      bool: true,
      nil: null,
      symbol: generateSymbol(),
    }),
    // With undefined values (should be omitted)
    () => ({ a: 1, b: undefined, c: 3 }),
    // Nested objects
    () =>
      depth < maxDepth - 1
        ? {
            user: {
              name: "Alice",
              age: 30,
              data: Buffer.from("userdata"),
            },
          }
        : { name: "Alice" },
    // Keys that test normalization
    () => ({ CamelCase: 1, lowercase: 2, UPPERCASE: 3 }),
    // Numeric string keys
    () => ({ 0: "zero", 1: "one", 2: "two" }),
    // Special key names
    () => ({
      "ao-types": "test",
      "content-type": "application/json",
      $empty: "should not be special",
    }),
    // Deep nesting
    () =>
      depth === 0
        ? {
            a: { b: { c: { d: { e: "deep" } } } },
          }
        : { shallow: true },
    // Large object
    () =>
      Object.fromEntries(
        Array(50)
          .fill(0)
          .map((_, i) => [`key${i}`, i])
      ),
  ]

  const choice = randomChoice(choices)
  const obj = choice()

  // Sometimes add nested structures
  if (depth < maxDepth - 1 && Math.random() < 0.3) {
    obj.nested_array = generateArray(depth + 1, maxDepth)
    obj.nested_object = generateObject(depth + 1, maxDepth)
  }

  return obj
}

// Generate a complex test case
function generateTestCase() {
  const choices = [
    // Primitives
    () => generatePrimitive(),
    // Arrays
    () => generateArray(),
    // Objects
    () => generateObject(),
    // Complex nested structure - use generators for symbols
    () => ({
      metadata: {
        version: "1.0",
        timestamp: Date.now(),
        flags: Buffer.from([0xff, 0x00, 0xff]),
      },
      data: {
        users: [
          { id: 1, name: "Alice", avatar: Buffer.from("avatar1") },
          { id: 2, name: "Bob", avatar: Buffer.from("avatar2") },
        ],
        settings: {
          enabled: true,
          threshold: 0.95,
          mode: generateSymbol(),
          options: [null, undefined, "", []],
        },
      },
      _internal: {
        cache: {},
        buffer: Buffer.alloc(1024),
        symbols: Array(3)
          .fill(null)
          .map(() => generateSymbol()),
      },
    }),
    // Edge case: circular reference prevention test
    () => {
      const obj = { a: 1 }
      obj.b = { c: 2, d: { e: 3 } } // Deep but not circular
      return obj
    },
    // All empty values
    () => ({
      empty_string: "",
      empty_buffer: Buffer.alloc(0),
      empty_array: [],
      empty_object: {},
      null_value: null,
      undefined_value: undefined,
    }),
    // Number edge cases (no Infinity, -Infinity, NaN)
    () => ({
      integers: [0, -0, 1, -1, 1000000, -1000000],
      floats: [0.0, -0.0, 0.1, -0.1, 3.14159, -3.14159],
      scientific: [1e10, 1e-10, 1.23e45, -1.23e-45],
    }),
    // String edge cases
    () => ({
      strings: [
        "",
        " ",
        "\n",
        "\t",
        "\r\n",
        "null",
        "true",
        "false",
        "undefined",
        "0",
        "[]",
        "{}",
        '"quoted"',
        "'quoted'",
        "\\escaped\\",
        "multi\nline\nstring",
        "../../path/traversal",
        '<script>alert("xss")</script>',
        '"; DROP TABLE users; --',
        "\x00\x01\x02\x03",
      ],
    }),
    // Buffer patterns
    () => ({
      buffers: [
        Buffer.alloc(0),
        Buffer.alloc(1, 0),
        Buffer.alloc(1, 255),
        Buffer.from([0, 127, 128, 255]),
        Buffer.from("Hello World"),
        Buffer.from("00000000", "hex"),
        Buffer.from("FFFFFFFF", "hex"),
        Buffer.from("DEADBEEF", "hex"),
        Buffer.from([0x00, 0x00, 0x00, 0x00]),
        Buffer.concat([
          Buffer.from("Hello"),
          Buffer.from(" "),
          Buffer.from("World"),
        ]),
      ],
    }),
    // Symbol variations - use generator
    () => ({
      symbols: Array(10)
        .fill(null)
        .map(() => generateSymbol()),
    }),
  ]

  return randomChoice(choices)()
}

// Main generator function
export function gen(count = 100) {
  const cases = []

  // Ensure we get a good distribution of different types
  const minPerType = Math.floor(count / 10)

  // Add some guaranteed edge cases
  const guaranteedCases = [
    // Primitives
    null,
    // undefined, // Skip undefined in guaranteed cases
    true,
    false,
    0,
    -0,
    "",
    Buffer.alloc(0),
    Symbol("undefined"),
    Symbol("null"),
    // Empty collections
    [],
    {},
    // Simple collections
    [1, 2, 3],
    { a: 1, b: 2 },
    // Mixed arrays - use global symbols
    [null, undefined, 0, "", Buffer.alloc(0), [], {}],
    // Nested empty
    { a: { b: { c: {} } } },
    [[[[[]]]]],
    // All types in one object - use global symbol
    {
      str: "string",
      num: 42,
      float: 3.14,
      bool_t: true,
      bool_f: false,
      nil: null,
      undef: undefined,
      buf: Buffer.from("buffer"),
      arr: [1, 2, 3],
      obj: { nested: true },
      sym: Symbol.for("symbol"),
    },
  ]

  // Add guaranteed cases
  guaranteedCases.forEach(c => {
    if (cases.length < count) {
      cases.push(c)
    }
  })

  // Generate random cases for the rest
  while (cases.length < count) {
    cases.push(generateTestCase())
  }

  // Shuffle the array to mix guaranteed and random cases
  for (let i = cases.length - 1; i > 0; i--) {
    const j = Math.floor(Math.random() * (i + 1))
    ;[cases[i], cases[j]] = [cases[j], cases[i]]
  }

  return cases
}

// Export individual generators for testing
export {
  generateString,
  generateBuffer,
  generateSymbol,
  generateNumber,
  generatePrimitive,
  generateArray,
  generateObject,
  generateTestCase,
}
