import assert from "assert"
import { after, describe, it, before, beforeEach } from "node:test"
import { acc, toAddr } from "../src/test.js"
import { getJWK } from "./lib/test-utils.js"
import HB from "../src/hb.js"
import HyperBEAM from "../src/hyperbeam.js"
import { resolve } from "path"
import { writeFileSync } from "fs"
const jwk = getJWK("../../HyperBEAM/.wallet.json")
const addr = toAddr(jwk.n)

let cases = [
  { "Tbun4iRRQW93gUiSAmTmZJ2PGI-_yYaXsX69ETgzSRE": "ABC" },
  // 1-10: Basic types
  { simple_string: "hello world" },
  { simple_integer: 42 },
  { simple_float: 3.14159 },
  { simple_boolean: true },
  { simple_null: null },
  { empty_string: "" },
  { negative_integer: -999 },
  { zero: 0 },
  //{ large_number: 9007199254740991 }, // MAX_SAFE_INTEGER
  { tiny_float: 0.0000001 },

  // 11-20: Arrays with different types
  { int_array: [1, 2, 3, 4, 5] },
  { string_array: ["apple", "banana", "cherry"] },
  { mixed_array: [1, "two", 3.0, true, null] },
  {
    nested_array: [
      [1, 2],
      [3, 4],
      [5, 6],
    ],
  },
  { deep_nested_array: [[[1]], [[2]], [[3]]] },
  { empty_array: [] },
  { single_item_array: [42] },
  { boolean_array: [true, false, true, false] },
  { array_with_objects: [{ a: 1 }, { b: 2 }, { c: 3 }] },
  { sparse_array_simulation: { 1: "first", 3: "third", 5: "fifth" } },

  // 21-30: Objects with various structures
  { flat_object: { name: "John", age: 30, city: "New York" } },
  { nested_object: { user: { profile: { name: "Jane", age: 25 } } } },
  { object_with_array: { items: ["item1", "item2", "item3"] } },
  {
    object_with_mixed: {
      str: "text",
      num: 123,
      arr: [1, 2, 3],
      obj: { nested: true },
    },
  },
  { empty_object: {} },
  { single_key_object: { only_key: "only_value" } },
  { numeric_string_keys: { 123: "numeric key", 456: "another numeric key" } },
  {
    special_char_keys: {
      "key-with-dash": "value1",
      key_with_underscore: "value2",
    },
  },
  { unicode_keys: { 键: "值", ключ: "значение" } },
  { deep_nesting: { a: { b: { c: { d: { e: "deep value" } } } } } },

  // 31-40: Complex nested structures
  {
    company: {
      name: "Tech Corp",
      employees: [
        { id: 1, name: "Alice", skills: ["JavaScript", "Python"] },
        { id: 2, name: "Bob", skills: ["Java", "C++"] },
      ],
      locations: {
        headquarters: { city: "San Francisco", country: "USA" },
        branches: [
          { city: "London", country: "UK" },
          { city: "Tokyo", country: "Japan" },
        ],
      },
    },
  },
  {
    matrix: [
      [1, 2, 3],
      [4, 5, 6],
      [7, 8, 9],
    ],
    metadata: { rows: 3, cols: 3, type: "integer" },
  },
  {
    tree: {
      value: "root",
      children: [
        { value: "child1", children: [] },
        { value: "child2", children: [{ value: "grandchild", children: [] }] },
      ],
    },
  },
  {
    graph: {
      nodes: ["A", "B", "C"],
      edges: [
        { from: "A", to: "B" },
        { from: "B", to: "C" },
      ],
    },
  },
  {
    multi_level_array: [
      [
        ["deep1", "deep2"],
        ["deep3", "deep4"],
      ],
      [
        ["deep5", "deep6"],
        ["deep7", "deep8"],
      ],
    ],
  },
  /*{
    mixed_depth: {
      level1: {
        array: [1, 2, { nested: ["a", "b"] }],
        value: "test",
      },
    },
  },*/
  {
    indexed_data: {
      1: { id: 1, value: "first" },
      2: { id: 2, value: "second" },
      3: { id: 3, value: "third" },
    },
  },
  {
    routes: {
      1: { path: "/home", method: "GET" },
      2: { path: "/api", method: "POST" },
    },
  },
  {
    config: {
      database: {
        host: "localhost",
        port: 5432,
        credentials: { user: "admin", pass: "secret" },
      },
      cache: { enabled: true, ttl: 3600 },
    },
  },
  {
    recursive_like: {
      name: "parent",
      related: {
        name: "child",
        related: {
          name: "grandchild",
          related: null,
        },
      },
    },
  },

  // 41-50: Edge cases with special values
  { large_string: "x".repeat(1000) },
  //{ unicode_string: "Hello 世界 🌍 Мир" },
  //{ escape_chars: "Line1\nLine2\tTabbed\r\nWindows" },
  { quotes_string: "He said \"Hello\" and she said 'Hi'" },
  {
    special_numbers: { infinity: 999999999, neg_infinity: -999999999, zero: 0 },
  },
  { date_string: "2025-07-10T12:00:00Z" },
  { base64_like: "SGVsbG8gV29ybGQ=" },
  { html_content: "<div class='test'>Hello <b>World</b></div>" },
  { json_string: '{"nested": "json", "as": "string"}' },
  { regex_pattern: "^[a-zA-Z0-9]+@[a-zA-Z0-9]+\\.[a-zA-Z]{2,}$" },

  // 51-60: Arrays that should be converted from objects
  { stack_print_prefixes: { 1: [104, 98] } }, // Should become [[104, 98]]
  { numbered_list: { 1: "first", 2: "second", 3: "third" } },
  { sparse_numbered: { 1: "one", 3: "three", 5: "five" } },
  { mixed_numeric_keys: { 1: "numeric", a: "alpha", 2: "numeric2" } },
  { nested_numeric: { items: { 1: { name: "item1" }, 2: { name: "item2" } } } },
  { double_nested_numeric: { 1: { 1: "one-one", 2: "one-two" } } },
  { array_of_arrays_sim: { 1: [1, 2], 2: [3, 4], 3: [5, 6] } },
  {
    complex_numeric: {
      data: { 1: { values: [1, 2, 3] }, 2: { values: [4, 5, 6] } },
    },
  },
  {
    mixed_depth_numeric: { 1: "simple", 2: { nested: "value" }, 3: [1, 2, 3] },
  },
  { interrupted_numeric: { 1: "first", 2: "second", 4: "fourth" } }, // Gap in sequence

  // 61-70: Path-like structures (testing flattening/unflattening)
  /*{ "user/profile/name": "John Doe" },
  { "api/v1/users": ["user1", "user2", "user3"] },
  { "config/database/host": "localhost", "config/database/port": 5432 },
  { "routes/1/path": "/home", "routes/1/method": "GET" },
  { "a/b/c/d/e": "deeply nested path value" },
  { "mixed/1/value": "numeric path", "mixed/a/value": "alpha path" },
  { "array/1/1": "nested numeric paths" },
  { "data/users/1/name": "Alice", "data/users/2/name": "Bob" },
  { "settings/feature/enabled": true, "settings/feature/config/timeout": 5000 },
  { "matrix/1/1": 1, "matrix/1/2": 2, "matrix/2/1": 3, "matrix/2/2": 4 },*/

  // 71-80: Empty and null cases
  { empty_values: { str: "", arr: [], obj: {} } },
  { null_values: { a: null, b: null, c: null } },
  {
    mixed_empty: { hasvalue: "yes", empty: "", nullvalue: null, emptyarr: [] },
  },
  { deeply_empty: { a: { b: { c: {} } } } },
  { array_with_empty: ["full", "", "another", "", "last"] },
  { object_with_empty_arrays: { a: [], b: [[]], c: [[], []] } },
  { sparse_with_null: { 1: "value", 2: null, 3: "another" } },
  { all_falsy: { zero: 0, empty: "", null: null, false: false } },
  { nested_empty: { outer: { middle: { inner: "" } } } },
  { mixed_empty_types: { arr: [], obj: {}, str: "", num: 0 } },

  // 81-90: Large and complex structures
  {
    large_array: Array(100)
      .fill(0)
      .map((_, i) => i),
    metadata: { count: 100, type: "sequence" },
  },
  {
    user_data: Array(10)
      .fill(0)
      .map((_, i) => ({
        id: i + 1,
        name: `User${i + 1}`,
        email: `user${i + 1}@example.com`,
        active: i % 2 === 0,
      })),
  },
  {
    nested_categories: {
      electronics: {
        computers: {
          laptops: ["Dell", "HP", "Apple"],
          desktops: ["Custom", "Prebuilt"],
        },
        phones: {
          smartphones: ["iPhone", "Android"],
          features: ["Flip", "Basic"],
        },
      },
      clothing: {
        mens: { shirts: ["T-Shirt", "Polo"], pants: ["Jeans", "Khakis"] },
        womens: { tops: ["Blouse", "T-Shirt"], bottoms: ["Skirt", "Pants"] },
      },
    },
  },
  {
    scientific_data: {
      experiment: "Test-123",
      measurements: [
        { time: 0, value: 1.23, error: 0.01 },
        { time: 1, value: 2.34, error: 0.02 },
        { time: 2, value: 3.45, error: 0.01 },
      ],
      constants: { g: 9.81, c: 299792458, h: 6.626e-34 },
    },
  },
  {
    filesystem: {
      "/": {
        home: {
          user: {
            documents: { "file1.txt": 1024, "file2.txt": 2048 },
            downloads: { "image.jpg": 4096, "video.mp4": 8192 },
          },
        },
        etc: { config: { "app.conf": 512 } },
      },
    },
  },
  {
    state_machine: {
      states: ["idle", "running", "paused", "stopped"],
      transitions: [
        { from: "idle", to: "running", event: "start" },
        { from: "running", to: "paused", event: "pause" },
        { from: "paused", to: "running", event: "resume" },
        { from: "running", to: "stopped", event: "stop" },
      ],
      current: "idle",
    },
  },
  {
    api_response: {
      status: 200,
      data: {
        users: Array(5)
          .fill(0)
          .map((_, i) => ({ id: i, name: `User ${i}` })),
        pagination: { page: 1, perpage: 5, total: 100 },
      },
      meta: { timestamp: Date.now(), version: "1.0" },
    },
  },
  {
    math_structures: {
      vector: [1, 2, 3],
      matrix: [
        [1, 0, 0],
        [0, 1, 0],
        [0, 0, 1],
      ],
      complex: { real: 3, imaginary: 4 },
      quaternion: { w: 1, x: 0, y: 0, z: 0 },
    },
  },
  {
    game_state: {
      player: {
        name: "Hero",
        level: 42,
        stats: { hp: 100, mp: 50, strength: 20, defense: 15 },
        inventory: [
          { item: "Sword", quantity: 1, equipped: true },
          { item: "Potion", quantity: 5, equipped: false },
        ],
      },
      world: {
        location: "Castle",
        npcs: ["Guard", "Merchant", "King"],
        quests: { active: ["Save Princess"], completed: ["Tutorial"] },
      },
    },
  },
  {
    binary_tree: {
      value: 10,
      left: {
        value: 5,
        left: { value: 3, left: null, right: null },
        right: { value: 7, left: null, right: null },
      },
      right: {
        value: 15,
        left: { value: 12, left: null, right: null },
        right: { value: 20, left: null, right: null },
      },
    },
  },

  // 91-100: Special format and edge cases
  {
    special_formats: {
      uuid: "550e8400-e29b-41d4-a716-446655440000",
      ipv4: "192.168.1.1",
      ipv6: "2001:0db8:85a3:0000:0000:8a2e:0370:7334",
      mac: "00:1B:44:11:3A:B7",
    },
  },
  {
    encoded_data: {
      base64: btoa("Hello World"),
      hex: "48656c6c6f20576f726c64",
      url_encoded: encodeURIComponent("Hello World!"),
    },
  },
  {
    extreme_nesting: (() => {
      let obj = { value: "deepest" }
      for (let i = 0; i < 10; i++) {
        obj = { level: i, nested: obj }
      }
      return obj
    })(),
  },
  {
    circular_reference_like: {
      node1: { id: 1, next: 2 },
      node2: { id: 2, next: 3 },
      node3: { id: 3, next: 1 },
    },
  },
  {
    mixed_languages: {
      english: "Hello",
      spanish: "Hola",
      french: "Bonjour",
      german: "Guten Tag",
      japanese: "こんにちは",
      arabic: "مرحبا",
      emoji: "👋",
    },
  },
  {
    type_coercion_test: {
      string_number: "123",
      string_boolean: "true",
      string_null: "null",
      string_array: "[1,2,3]",
      string_object: '{"key":"value"}',
    },
  },
  {
    boundary_values: {
      max_int32: 2147483647,
      min_int32: -2147483648,
      small_float: 0.000000001,
      large_float: 999999999.999999999,
    },
  },
  {
    special_keys: {
      "": "empty key",
      " ": "space key",
      123: "numeric string key",
      true: "boolean string key",
      null: "null string key",
      "[object object]": "object string key",
    },
  },
  {
    protocol_buffer_like: {
      message_type: "UserProfile",
      fields: [
        { number: 1, name: "id", type: "int64", value: 12345 },
        { number: 2, name: "username", type: "string", value: "testuser" },
        { number: 3, name: "created_at", type: "timestamp", value: Date.now() },
      ],
    },
  },
  {
    final_test: {
      description: "This is test case #100",
      summary: {
        total_cases: 100,
        categories: ["basic", "arrays", "objects", "edge_cases", "complex"],
        purpose: "comprehensive wao@1.0 codec testing",
      },
      metadata: {
        created: new Date().toISOString(),
        version: "1.0.0",
      },
    },
  },
  // 1-10: Numeric edge cases (avoiding MAX_SAFE_INTEGER)
  { medium_number: 1000000000 }, // 1 billion
  { float_precision: 3.141592653589793 }, // PI with more precision
  { scientific_notation: 1.23e-10 },
  { negative_float: -456.789 },
  { decimal_places: 0.123456789 },
  { int_as_float: 42.0 },
  { negative_zero: -0 },
  { small_negative: -0.0001 },
  { fraction: 1 / 3 }, // 0.3333...
  { computed_float: Math.sqrt(2) },

  // 11-20: String variations (avoiding unicode/emojis)
  { single_char: "a" },
  { numeric_string: "12345" },
  { boolean_string: "false" },
  { null_string: "null" },
  { mixed_case_value: "CamelCaseString" }, // value has caps, key is lowercase
  { underscore_string: "snake_case_string" },
  { dash_string: "kebab-case-string" },
  { space_string: "string with spaces" },
  { punctuation: "!@#$%^&*()_+-=[]{}|;:,.<>?" },
  { url_string: "https://example.com/path?query=value" },

  // 21-30: Array patterns
  { homogeneous_strings: ["a", "b", "c", "d", "e"] },
  { homogeneous_numbers: [10, 20, 30, 40, 50] },
  { alternating_types: [1, "a", 2, "b", 3, "c"] },
  { array_of_nulls: [null, null, null] },
  { array_of_booleans: [true, true, false, false] },
  { array_of_empty_strings: ["", "", ""] },
  { array_of_zeros: [0, 0, 0, 0] },
  { ascending_numbers: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] },
  { descending_numbers: [10, 9, 8, 7, 6, 5, 4, 3, 2, 1] },
  { array_with_gaps: [1, undefined, 3, undefined, 5] },

  // 31-40: Object patterns with consistent naming (all lowercase)
  { all_lowercase: { firstname: "john", lastname: "doe", age: 30 } },
  { all_numbers: { first: 1, second: 2, third: 3 } },
  { numeric_values_only: { a: 1, b: 2, c: 3, d: 4, e: 5 } },
  { string_values_only: { a: "one", b: "two", c: "three" } },
  { boolean_values_only: { a: true, b: false, c: true } },
  { null_values_only: { a: null, b: null, c: null, d: null } },
  { single_char_keys: { a: 1, b: 2, c: 3, d: 4 } },
  { numeric_key_strings: { 100: "hundred", 200: "two hundred" } },
  { ordered_keys: { a: 1, b: 2, c: 3, d: 4, e: 5, f: 6 } },
  { reverse_ordered_keys: { z: 26, y: 25, x: 24, w: 23 } },

  // 41-50: Nested structures with consistent patterns (all lowercase)
  { triple_nested: { a: { b: { c: "value" } } } },
  { quad_nested: { a: { b: { c: { d: "value" } } } } },
  { nested_arrays_simple: { data: [[1], [2], [3]] } },
  { nested_objects_simple: { data: [{ id: 1 }, { id: 2 }, { id: 3 }] } },
  {
    uniform_structure: {
      item1: { type: "a", value: 1 },
      item2: { type: "b", value: 2 },
      item3: { type: "c", value: 3 },
    },
  },
  {
    matrix_2x2: {
      data: [
        [1, 2],
        [3, 4],
      ],
      rows: 2,
      cols: 2,
    },
  },
  {
    matrix_3x3: {
      data: [
        [1, 2, 3],
        [4, 5, 6],
        [7, 8, 9],
      ],
      rows: 3,
      cols: 3,
    },
  },
  {
    symmetric_object: {
      left: { value: 1, child: { value: 2 } },
      right: { value: 1, child: { value: 2 } },
    },
  },
  {
    branching_structure: {
      root: {
        branch1: { leaf1: 1, leaf2: 2 },
        branch2: { leaf3: 3, leaf4: 4 },
      },
    },
  },
  {
    list_of_lists: {
      lists: [
        ["a", "b"],
        ["c", "d"],
        ["e", "f"],
      ],
    },
  },

  // 51-60: Type mixing patterns (all lowercase)
  { number_or_string: { a: 1, b: "2", c: 3, d: "4" } },
  { bool_or_null: { a: true, b: null, c: false, d: null } },
  {
    mixed_array_types: {
      numbers: [1, 2, 3],
      strings: ["a", "b", "c"],
      booleans: [true, false],
    },
  },
  { optional_fields: { required: "value", optional1: null, optional2: "" } },
  {
    type_indicators: {
      is_number: 123,
      is_string: "text",
      is_boolean: true,
      is_null: null,
      is_array: [1, 2, 3],
      is_object: { key: "value" },
    },
  },
  {
    consistent_types: {
      numbers: { a: 1, b: 2, c: 3 },
      strings: { a: "one", b: "two", c: "three" },
      booleans: { a: true, b: false, c: true },
    },
  },
  {
    array_lengths: {
      empty: [],
      single: [1],
      pair: [1, 2],
      triple: [1, 2, 3],
      many: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
    },
  },
  {
    numeric_strings_vs_numbers: {
      string_one: "1",
      number_one: 1,
      string_float: "3.14",
      number_float: 3.14,
    },
  },
  /*{
    falsy_values: {
      zero: 0,
      empty_string: "",
      false_bool: false,
      null_value: null,
      undefined_value: undefined,
    },
  },*/
  {
    truthy_values: {
      one: 1,
      string: "text",
      true_bool: true,
      empty_object: {},
      empty_array: [],
    },
  },

  // 61-70: Special object patterns (all lowercase)
  { constructor_key: { constructor: "value", prototype: "value" } },
  { reserved_words: { class: "value", function: "value", return: "value" } },
  { math_constants: { pi: Math.PI, e: Math.E, sqrt2: Math.SQRT2 } },
  {
    date_components: {
      year: 2025,
      month: 7,
      day: 10,
      hour: 12,
      minute: 30,
      second: 45,
    },
  },
  { rgb_color: { r: 255, g: 128, b: 0, a: 0.5 } },
  { coordinates: { x: 10.5, y: 20.5, z: 30.5 } },
  { dimensions: { width: 1920, height: 1080, depth: 32 } },
  { temperature: { celsius: 25, fahrenheit: 77, kelvin: 298.15 } },
  { percentage_values: { half: 0.5, quarter: 0.25, three_quarters: 0.75 } },
  { binary_flags: { flag1: true, flag2: false, flag3: true, flag4: false } },

  // 71-80: Array transformation patterns (all lowercase)
  { range_1_to_5: { values: [1, 2, 3, 4, 5] } },
  { even_numbers: { values: [2, 4, 6, 8, 10] } },
  { odd_numbers: { values: [1, 3, 5, 7, 9] } },
  { powers_of_two: { values: [1, 2, 4, 8, 16, 32] } },
  { fibonacci: { values: [1, 1, 2, 3, 5, 8, 13] } },
  { squares: { values: [1, 4, 9, 16, 25] } },
  { alphabet_subset: { values: ["a", "b", "c", "d", "e"] } },
  { repeated_pattern: { values: [1, 2, 1, 2, 1, 2] } },
  { mirror_pattern: { values: [1, 2, 3, 3, 2, 1] } },
  { alternating_signs: { values: [1, -1, 2, -2, 3, -3] } },

  // 81-90: Practical data structures (all lowercase)
  {
    user_session: {
      session_id: "abc123",
      user_id: 42,
      logged_in: true,
      last_active: "2025-07-10T12:00:00Z",
    },
  },
  {
    shopping_item: {
      id: 1001,
      name: "Widget",
      price: 19.99,
      quantity: 5,
      in_stock: true,
    },
  },
  {
    error_response: {
      success: false,
      error: {
        code: 404,
        message: "Not Found",
        details: null,
      },
    },
  },
  {
    success_response: {
      success: true,
      data: {
        id: 1,
        created: true,
        message: "Created successfully",
      },
    },
  },
  {
    configuration: {
      app_name: "TestApp",
      version: "1.0.0",
      debug: false,
      max_connections: 100,
      timeout: 30,
    },
  },
  {
    metrics: {
      count: 1000,
      min: 0.1,
      max: 99.9,
      avg: 50.5,
      median: 49.8,
    },
  },
  {
    feature_flags: {
      feature_a: true,
      feature_b: false,
      feature_c: true,
      feature_experimental: false,
    },
  },
  {
    pagination: {
      page: 1,
      per_page: 20,
      total: 100,
      total_pages: 5,
      has_next: true,
      has_prev: false,
    },
  },
  {
    validation_result: {
      valid: true,
      errors: [],
      warnings: ["Field X is deprecated"],
      info: { processed: 10, skipped: 0 },
    },
  },
  {
    cache_entry: {
      key: "user:123",
      value: { id: 123, name: "Test User" },
      ttl: 3600,
      created_at: 1752141240610,
      expires_at: 1752144840610,
    },
  },

  // 91-100: Edge cases with safe values (all lowercase)
  { single_level_numeric: { 1: "one", 2: "two", 3: "three" } },
  {
    mixed_depth_safe: {
      shallow: "value",
      deep: { nested: { value: "deep" } },
      array: [1, 2, 3],
    },
  },
  {
    safe_special_chars: {
      dash: "value-with-dash",
      underscore: "value_with_underscore",
      dot: "value.with.dot",
    },
  },
  {
    numeric_like_strings: {
      integer_string: "123",
      float_string: "123.45",
      exp_string: "1.23e5",
      hex_string: "0xFF",
    },
  },
  {
    time_values: {
      seconds: 60,
      minutes: 60,
      hours: 24,
      days: 7,
      weeks: 52,
    },
  },
  {
    status_codes: {
      ok: 200,
      created: 201,
      bad_request: 400,
      not_found: 404,
      server_error: 500,
    },
  },
  {
    file_info: {
      name: "document.pdf",
      size: 102400,
      type: "application/pdf",
      created: "2025-07-10",
      modified: "2025-07-10",
    },
  },
  {
    network_info: {
      protocol: "https",
      host: "example.com",
      port: 443,
      path: "/api/v1/resource",
      secure: true,
    },
  },
  {
    computation_result: {
      input: [1, 2, 3, 4, 5],
      operation: "sum",
      result: 15,
      duration_ms: 0.5,
      cached: false,
    },
  },
  {
    final_mixed_test: {
      id: 100,
      type: "mixed",
      data: {
        numbers: [1, 2, 3],
        strings: ["a", "b", "c"],
        nested: {
          level: 1,
          values: [true, false, null],
        },
      },
      metadata: {
        created: "2025-07-10",
        version: 2,
        tags: ["test", "final", "mixed"],
      },
      active: true,
    },
  },
  // Test mixed_depth pattern with all lowercase
  /*{
    mixed_depth: {
      level1: {
        array: [1, 2, { nested: ["a", "b"] }],
        value: "test",
      },
    },
  },*/
  // Test path-like structures without slashes
  { user_profile_name: "John Doe" },
  { api_v1_users: ["user1", "user2", "user3"] },
  { config_database_host: "localhost", config_database_port: 5432 },
  { routes_1_path: "/home", routes_1_method: "GET" },
  { settings_feature_enabled: true, settings_feature_config_timeout: 5000 },

  // Test hasvalue/emptyarr pattern with lowercase
  {
    mixed_empty: { hasvalue: "yes", empty: "", nullvalue: null, emptyarr: [] },
  },

  // More complex structures with all lowercase
  {
    deeply_nested_data: {
      level1: {
        level2: {
          level3: {
            level4: {
              level5: {
                data: [1, 2, 3],
                meta: { count: 3 },
              },
            },
          },
        },
      },
    },
  },

  // Array of arrays of arrays (all lowercase)
  {
    triple_nested_arrays: [
      [
        [1, 2],
        [3, 4],
      ],
      [
        [5, 6],
        [7, 8],
      ],
      [
        [9, 10],
        [11, 12],
      ],
    ],
  },

  // Object with many levels of nesting (all lowercase)
  {
    organization: {
      company: {
        departments: {
          engineering: {
            teams: {
              backend: { members: 10 },
              frontend: { members: 8 },
              devops: { members: 5 },
            },
          },
          sales: {
            regions: {
              north: { revenue: 1000000 },
              south: { revenue: 800000 },
            },
          },
        },
      },
    },
  },

  // Testing various empty combinations (all lowercase)
  {
    empty_variations: {
      empty_string: "",
      empty_array: [],
      empty_object: {},
      array_of_empty: [[], [], []],
      object_of_empty: { a: {}, b: {}, c: {} },
      mixed_empty: { str: "", arr: [], obj: {}, null: null },
    },
  },
]

describe("Hyperbeam Device", function () {
  let hb, hbeam
  before(async () => {
    hbeam = await new HyperBEAM({
      devices: ["meta", "httpsig", "structured", "flat", "json", "wao"],
      clearCache: true,
      c: "12",
      cmake: "3.5",
    }).ready()
  })

  beforeEach(async () => {
    hb = await new HB({}).init(jwk)
  })

  after(async () => hbeam.kill())

  it.only("should test wao@1.0", async () => {
    let i = 0
    let io = []
    for (const json of cases) {
      const res = await hb.post({
        path: "/~wao@1.0/identity",
        body: JSON.stringify(json),
      })
      console.log(`${++i}/${cases.length}`)
      console.log(json)
      console.log(res)
      console.log(JSON.stringify(res.out))
      assert.deepEqual(json, res.out)
      /*
      delete res.headers.signature
      delete res.headers["access-control-allow-methods"]
      delete res.headers["access-control-allow-origin"]
      delete res.headers["date"]
      delete res.headers["server"]
      delete res.headers["transfer-encoding"]
      res.headers["signature-input"] =
        res.headers["signature-input"].split(";")[0]
        io.push({ in: json, out: { headers: res.headers, body: res.body } })
        */
    }
    /*
    writeFileSync(
      resolve(import.meta.dirname, "output.txt"),
      JSON.stringify(io, undefined, 2)
    )*/
  })
})
