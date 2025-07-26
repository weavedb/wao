// Test cases for json_to_erl endpoint
// These are JS objects with Erlang types (Buffer for binaries, Symbol for atoms)
export const cases_from = [
  // 1. Simple binary
  { bin: Buffer.from([1, 2, 3]) },

  // 2. Empty binary
  { empty: Buffer.alloc(0) },

  // 3. Text as binary
  { text: Buffer.from("Hello World") },

  // 4. Multiple binaries
  {
    a: Buffer.from([255]),
    b: Buffer.from([0, 128]),
    c: Buffer.from("test"),
  },

  // 5. Mixed types
  {
    name: "test",
    count: 42,
    active: true,
    data: Buffer.from([1, 2, 3]),
  },

  // 6. Nested maps with binary
  {
    user: {
      name: "Alice",
      avatar: Buffer.from("image data"),
    },
  },

  // 7. Array with binaries
  {
    items: ["text", Buffer.from("binary"), 123, true],
  },

  // 8. Null and binary
  {
    value: null,
    data: Buffer.from("null test"),
  },

  // 9. All primitive types
  {
    str: "string",
    int: 42,
    float: 3.14,
    bool_t: true,
    bool_f: false,
    nil: null,
    bin: Buffer.from("data"),
  },

  // 10. Empty collections with binary
  {
    list: [],
    map: {},
    data: Buffer.from([255, 254, 253]),
  },

  // 11. Large binary
  {
    large: Buffer.from(
      "Lorem ipsum dolor sit amet, consectetur adipiscing elit. ".repeat(10)
    ),
  },

  // 12. Binary with all byte values
  {
    bytes: Buffer.from([0, 1, 127, 128, 255]),
  },

  // 13. Unicode text as binary
  {
    unicode: Buffer.from("Hello 世界 😀", "utf8"),
  },

  // 14. Deep nesting with binaries
  {
    a: {
      b: {
        c: {
          data: Buffer.from("deep"),
          more: {
            bin: Buffer.from([42]),
          },
        },
      },
    },
  },

  // 15. Array of maps with binaries
  {
    users: [
      { name: "Alice", key: Buffer.from("key1") },
      { name: "Bob", key: Buffer.from("key2") },
      { name: "Charlie", key: Buffer.from([1, 2, 3]) },
    ],
  },

  // 16. Binary and string with same content
  {
    as_string: "test",
    as_binary: Buffer.from("test"),
  },

  // 17. Map with numeric keys (converted to strings in JSON)
  {
    123: Buffer.from("numeric key"),
    regular: "value",
  },

  // 18. Special characters in strings
  {
    text: "Line1\nLine2\tTab",
    binary: Buffer.from("Line1\nLine2\tTab"),
  },

  // 19. Floats and binaries
  {
    pi: 3.14159265359,
    e: 2.71828182846,
    data: Buffer.from(Float64Array.from([3.14159265359]).buffer),
  },

  // 20. Everything combined
  {
    // Primitives
    string: "hello",
    integer: 42,
    float: 3.14,
    bool_true: true,
    bool_false: false,
    null_value: null,

    // Binary data
    binary: Buffer.from([0, 1, 2, 255]),
    text_binary: Buffer.from("Hello, World!"),
    empty_binary: Buffer.alloc(0),

    // Collections
    array: [1, "two", Buffer.from("three"), true, null],
    nested: {
      map: {
        with: {
          binary: Buffer.from("nested binary"),
        },
      },
    },

    // Array of objects
    list: [
      { id: 1, data: Buffer.from("first") },
      { id: 2, data: Buffer.from([255, 0, 128]) },
    ],
  },
]
