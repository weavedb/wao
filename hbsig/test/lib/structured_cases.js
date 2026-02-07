// Test cases for structured_from (TABM to native)
// NOTE: Simplified to match HB's dev_codec_structured behavior.
// The structured codec applies type conversions and removes ao-types.
export const cases_from = [
  // 1. Simple string value without type
  {
    message: "Hello World",
  },

  // 2. Simple nested object without types
  {
    user: {
      name: "John",
      city: "NYC",
    },
  },

  // 3. Unicode in values
  {
    greeting: "Hello 世界",
    emoji: "🎉",
  },

  // 4. Multiple string values
  {
    first: "one",
    second: "two",
    third: "three",
  },

  // 5. Deeply nested strings
  {
    level1: {
      level2: {
        level3: {
          value: "deep",
        },
      },
    },
  },

  // 6. Special characters in values
  {
    message: "Hello, World!",
    json: '{"key": "value"}',
  },

  // 7. Keys with special characters
  {
    "special-key": "value",
    another_key: "data",
  },

  // 8. Multiple nested levels
  {
    app: {
      name: "MyApp",
      version: "1.0.0",
      config: {
        env: "production",
        region: "us-east",
      },
    },
  },

  // 9. Path-like keys
  {
    path: "/api/users",
    method: "GET",
  },

  // 10. Long string values
  {
    description: "This is a longer text value that spans multiple words.",
  },
]

// Test cases for structured_to (native to TABM)
export const cases_to = [
  // 1. Simple string value
  {
    message: "Hello World",
  },

  // 2. Integer value
  {
    count: 42,
  },

  // 3. Boolean values
  {
    enabled: true,
    disabled: false,
  },

  // 4. Empty values
  {
    "empty-string": "",
    "empty-list": [],
    "empty-object": {},
  },

  // 5. Nested object
  {
    user: {
      name: "John",
      age: 30,
    },
  },

  // 6. List of strings
  {
    tags: ["first", "second", "third"],
  },

  // 7. Mixed list
  {
    values: [1, true, "text"],
  },

  // 8. List of objects
  {
    items: [{ name: "Item 1" }, { name: "Item 2" }],
  },

  // 9. Complex nested structure
  {
    app: {
      name: "MyApp",
      version: "1.0.0",
      config: {
        port: 3000,
        debug: true,
      },
    },
  },

  // 10. Null value
  {
    nullable: null,
  },

  // 11. Float value
  {
    price: 19.99,
  },

  // 12. Strings with special characters
  {
    messages: ['Hello "World"', "Line1\\Line2"],
  },

  // 13. Multiple empty types with regular value
  {
    a: "",
    b: [],
    c: {},
    d: "not empty",
  },

  // 14. Unicode in values
  {
    greeting: "Hello 世界",
    emoji: "🎉",
  },

  // 15. Deeply nested with mixed types
  {
    level1: {
      level2: {
        level3: {
          value: 42,
        },
      },
    },
  },

  // 16. List with various types
  {
    mixed: ["", "value", 123, true, null],
  },

  // 17. All primitive types
  {
    int: 123,
    float: 45.67,
    bool: true,
    null: null,
    string: "text",
  },

  // 18. Keys requiring encoding
  {
    "special-key": "value",
    another_key: 123,
  },

  // 19. Complex list of mixed types
  {
    data: [100, 3.14, false, "plain string"],
  },

  // 20. Large structure with various types
  {
    users: [
      {
        id: 1,
        name: "Alice",
        active: true,
        score: 95.5,
        tags: ["admin", "user"],
      },
      {
        id: 2,
        name: "Bob",
        active: false,
        score: 87.3,
        tags: ["user"],
      },
    ],
  },
]
