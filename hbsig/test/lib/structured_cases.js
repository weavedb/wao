// Test cases for structured_from (TABM to native)
export const cases_from = [
  // 1. Simple string value without type
  {
    message: "Hello World",
  },

  // 2. Integer with type annotation
  {
    count: "42",
    "ao-types": 'count="integer"',
  },

  // 3. Boolean values
  {
    enabled: "?1",
    disabled: "?0",
    "ao-types": 'enabled="boolean", disabled="boolean"',
  },

  // 4. Empty values
  {
    "ao-types":
      'empty%2dstring="empty-binary", empty%2dlist="empty-list", empty%2dobject="empty-message"',
  },

  // 5. Nested object
  {
    user: {
      name: "John",
      age: "30",
      "ao-types": 'age="integer"',
    },
  },

  // 6. List of strings
  {
    tags: '"first", "second", "third"',
    "ao-types": 'tags="list"',
  },

  // 7. Mixed list with types
  {
    values: '"(ao-type-integer) 1", "(ao-type-boolean) ?1", "text"',
    "ao-types": 'values="list"',
  },

  // 8. List of objects (numbered map)
  {
    items: {
      0: { name: "Item 1" },
      1: { name: "Item 2" },
    },
    "ao-types": 'items="list"',
  },

  // 9. Complex nested structure
  {
    app: {
      name: "MyApp",
      version: "1.0.0",
      config: {
        port: "3000",
        debug: "?1",
        "ao-types": 'port="integer", debug="boolean"',
      },
    },
  },

  // 10. Null value (atom)
  {
    nullable: '"null"',
    "ao-types": 'nullable="atom"',
  },

  // 11. Float value
  {
    price: "19.99",
    "ao-types": 'price="float"',
  },

  // 12. Escaped strings in list
  {
    messages: '"Hello \\\\\\"World\\\\\\"", "Line1\\\\\\\\Line2"',
    "ao-types": 'messages="list"',
  },

  // 13. Multiple empty types
  {
    a: "",
    b: [],
    c: {},
    d: "not empty",
    "ao-types": 'a="empty-binary", b="empty-list", c="empty-message"',
  },

  // 14. Unicode in values
  {
    greeting: "Hello 世界",
    emoji: "🎉",
  },

  // 15. Deeply nested with types
  {
    level1: {
      level2: {
        level3: {
          value: "42",
          "ao-types": 'value="integer"',
        },
      },
    },
  },

  // 16. List with empty values
  {
    mixed: {
      1: "value",
      "ao-types": '0="empty-binary", 2="empty-binary"',
    },
    "ao-types": 'mixed="list"',
  },

  // 17. All primitive types
  {
    int: "123",
    float: "45.67",
    bool: "?1",
    null: '"null"',
    string: "text",
    "ao-types": 'int="integer", float="float", bool="boolean", null="atom"',
  },

  // 18. Regular string values (not empty)
  {
    "special-key": "value",
    another_key: "123",
    "ao-types": 'another_key="integer"',
  },

  // 19. Complex list of mixed types
  {
    data: '"(ao-type-integer) 100", "(ao-type-float) 3.14", "(ao-type-boolean) ?0", "plain string"',
    "ao-types": 'data="list"',
  },

  // 20. Large structure with various types
  {
    users: {
      0: {
        id: "1",
        name: "Alice",
        active: "?1",
        score: "95.5",
        tags: '"admin", "user"',
        "ao-types":
          'id="integer", active="boolean", score="float", tags="list"',
      },
      1: {
        id: "2",
        name: "Bob",
        active: "?0",
        score: "87.3",
        tags: '"user"',
        "ao-types":
          'id="integer", active="boolean", score="float", tags="list"',
      },
    },
    "ao-types": 'users="list"',
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
