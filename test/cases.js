const bin = Buffer.from([1, 2, 3])
const empty = Buffer.from([])
const internal_fails = [
  { "data-field": 1, data_field: 2, dataField: 3 },
  {
    nested: {
      nums: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
      maps: [{ n: 1 }, { n: 2 }, { n: 3 }, { n: 4 }, { n: 5 }],
      empty: ["", [], {}, null],
    },
  },
  {
    matrix: [
      [
        { x: 1, y: 1 },
        { x: 2, y: 2 },
      ],
      [
        { x: 3, y: 3 },
        { x: 4, y: 4 },
      ],
    ],
  },

  { recursive: [1, [2, [3, [4]]]] },
  { types: ["", 0, "", false, Symbol("null"), [], {}, -1] },
  { mixed_empty: ["", 0, [], "", {}, null, false] },
  { chaos: [1, "three", true, Symbol("ok"), null, [], {}, 4.5] },
  { network: { nodes: [{ ip: "127.0.0.1", port: 8080 }] } },
  { nested_maps: { a: [{ val: 1 }], b: [{ val: 2 }] } },
  { tree: { value: 1, children: [{ value: 2 }, { value: 3 }] } },
  { complex_empty: { lists: [[]], maps: [{}] } },
  { empty_in_maps: [{ empty: "" }, { empty: [] }, { empty: {} }] },
  { empty_nested: [[], [[]], [[], []]] },
  { empty_maps_list: [{}, {}, {}] },
  { empty_map_array: [{}, { a: 1 }, {}, { b: 2 }] },
  { mixed_empty_full: ["", [1], {}, { a: 1 }] },
  { users: [{ id: 1 }, { id: 2 }] },
  {
    items: [
      { name: "a", value: 1 },
      { name: "b", value: 2 },
    ],
  },
  {
    mixed_maps: [
      { type: "binary", data: [1, 2, 3] },
      { type: "text", data: "hello" },
    ],
  },
  { nested_array_maps: [{ items: [1, 2, 3] }, { items: [4, 5, 6] }] },

  { maps_different_keys: [{ x: 1 }, { y: 2 }, { z: 3 }] },
  { deep_map_array: [{ level: { deep: 1 } }, { level: { deep: 2 } }] },
  { maps_with_arrays: [{ arr: [1, 2] }, { arr: [3, 4] }] },
  { max_int32: 2147483647 },
  { min_int32: -2147483648 },
  { _field: 1, field: 2, field_: 3 },
  {
    matrix: [
      [1, 2],
      [3, 4],
      [5, 6],
    ],
  },
  { layers: [{ data: [1, 2] }, { data: [3, 4] }] },
  { mixed_types: [{ int: 2, str: "three" }] },
  {
    config: [
      { setting: "on", value: 1 },
      { setting: "off", value: 0 },
    ],
  },
  { alternating: [1, "one", 2, "two", 3, "three"] },
  { wrapped: [[1], [2], ["three"], [true], [Symbol("ok")]] },
  {
    indexed: [
      { idx: 1, data: 1 },
      { idx: 2, data: "two" },
    ],
  },
  {
    flagged: [
      { flag: true, value: 1 },
      { flag: false, value: 0 },
    ],
  },
  {
    tagged: [
      { tag: "num", val: 123 },
      { tag: "str", val: "hello" },
    ],
  },
  { protocol: { header: [1, 2, 3, 4], body: { data: [9, 10, 11] } } },
  { messages: [{ from: 1, to: 2, data: "hello" }] },

  {
    files: [
      { name: "file1", size: 123 },
      { name: "file2", size: 456 },
    ],
  },
  { stream: { chunks: ["part1", "part2", "part3"], eof: true } },
  {
    levels: {
      1: [1],
      2: [1, 2],
      3: [1, 2, 3],
      empty: [],
    },
  },
]
const ok = [
  ...internal_fails,
  { data: { test: [1.23, "str", 1, Symbol("ok"), [1, 2, 3]] } },
  { key: [{ str: "abc" }] },
  {
    complex: {
      nested: {
        array: [true, false, "text", 42],
        empty_map: {},
        symbol: Symbol("atom_value"),
      },
      simple: "value",
    },
  },
  {
    mixed_array: [{ name: "John", age: 25 }, "standalone_string", 3.14159],
    boolean_flag: true,
    number: 999,
  },
  {
    list_of_maps: [
      { id: 1, status: Symbol("active") },
      {
        id: 2,
        status: Symbol("inactive"),
        meta: { created: "2024-01-01" },
      },
    ],
    metadata: { version: "1.0", timestamp: 1640995200 },
  },
  {
    deep_nesting: {
      level1: {
        level2: {
          level3: [Symbol("error"), Symbol("warning"), Symbol("info")],
          config: { debug: true },
        },
      },
    },
    simple_list: [1, 2, 3, 4, 5],
  },
  {
    binary_data: "some binary content",
    symbols: [Symbol("success"), Symbol("failure"), Symbol("pending")],
    numbers: [0, -1, 42, 3.14, -99.99],
    strings: ["hello", "world", "test"],
  },
  {
    body: Buffer.from([1, 2, 3, 255, 0]),
    empty_list: [],
    empty_binary: Buffer.from([]),
  },
  {
    data: {
      bytes: Buffer.from([0, 127, 255]),
      empty: { list: [], map: {} },
    },
  },
  {
    count: 10,
    active: true,
    tags: ["alpha", "beta", "gamma"],
  },
  {
    data: "plain text",
    body: { content: Buffer.from([192, 168, 1, 1]) },
  },
  {
    nested_empty: {
      a: {},
      b: { c: [] },
    },
    flag: false,
  },
  {
    data: [[], [1], [1, 2], []],
    meta: null,
  },
  {
    body: {
      raw: Buffer.from([0, 0, 0, 0]),
      encoded: "base64_content",
    },
    empty_string: "",
  },
  {
    items: {
      first: Buffer.from([255]),
      second: Buffer.from([0]),
      third: Buffer.from([128]),
    },
  },
  {
    data: { body: Buffer.from([255]) },
    body: { data: Buffer.from([0]) },
  },
  {
    values: [1, 2, 3, 4, 5],
    states: [Symbol("active"), Symbol("inactive"), Symbol("pending")],
  },
  {
    body: { a: { b: 5, c: 3 } },
    data: { d: { e: 5, f: 3 } },
    key: { g: { h: 5, i: 3 } },
    num: 3,
    list: [1, 2, 3],
    str: "Hello",
  },
  { data: { a: 3, b: 4 } },
  { body: { a: { c: 3 }, b: 4 } },
  { data: { a: 3, b: { c: { d: 4 } } }, body: { a: 3, b: { c: { d: 4 } } } },
  {
    float: 1.23,
    int: 1,
    bool: true,
    atom: Symbol("ok"),
    nest: { bool: true, atom: Symbol("ok") },
    body: { bool: true, nest: { atom: Symbol("ok") } },
    data: { bool: true, nest: { atom: Symbol("ok") } },
  },
  { key: [{ str: "abc" }] },
  {
    data: [
      1,
      "abc",
      true,
      "ok",
      1.23,
      { bool: false, float: 3.14, int: 1, nested: "ok", str: "abc" },
    ],
  },
  { key: [{ int: 1 }] },
  { null: null, undefined: undefined },
  { body: { list: [], map: {} }, nested: { list: [], map: {} } },
  { empty: Buffer.from([]), nested: [Buffer.from([]), 3] },
  { binary: Buffer.from([1, 2, 3]) },
  {
    str: "abc",
    bool: true,
    num: 123,
    float: 3.14,
    atom: Symbol("ok"),
    list: [1, 2, 3],
    body: bin,
  },
  { map: { a: 1, b: 2, c: { d: 3 } }, body: bin },
  { body: bin, data: bin },
  { map: { a: 3, b: "abc", c: { d: { e: 3 } } } },
  { list: [1, [2, 3]] },
  { map: { jntzf: 8.02 } },
  { list: [53.05] },
  { map: { float: 86.01, bool: true } },
  { key: [[8.02]] },
  { body: bin, data: bin },
  { list: [Symbol("ok")] },
  { base64: Buffer.from([1, 2, 3]).toString("base64") },
  { bin: empty },
  { body: empty },
  { bin: empty, body: empty },
  { bin: {} },
  { data: {} },
  { body: {} },
  { data: {}, body: {} },
  { bin: {}, data: {} },
  { bin: {}, body: {} },
  { bin: {}, data: {}, body: {} },
  { bin: [] },
  { data: [] },
  { body: [] },
  { data: [], body: [] },
  { bin: [], data: [] },
  { bin: [], body: [] },
  { bin: [], data: [], body: [] },
  { bin },
  { data: bin },
  { body: bin },
  { data: bin, body: bin },
  { bin, data: bin },
  { bin, body: bin },
  { bin, data: bin, body: bin },
  { num: Symbol("atom") },
  { data: Symbol("atom") },
  { body: Symbol("atom") },
  { data: Symbol("atom"), body: Symbol("atom") },
  { num: Symbol("atom"), data: Symbol("atom") },
  { num: Symbol("atom"), body: Symbol("atom") },
  { num: Symbol("atom"), data: Symbol("atom"), body: Symbol("atom") },
  { num: 3.14 },
  { data: 3.14 },
  { body: 3.14 },
  { data: 3.14, body: 3.14 },
  { num: 3.14, data: 3.14 },
  { num: 3.14, body: 3.14 },
  { num: 3.14, data: 3.14, body: 3.14 },
  { data: "abc" },
  { body: "abc" },
  { data: "abc", body: "abc" },
  { str: "abc" },
  { data: "abc", str: "abc" },
  { body: "abc", str: "abc" },
  { data: "abc", body: "abc", str: "abc" },
  { ok: true },
  { data: true },
  { body: true },
  { data: true, body: true },
  { ok: true, data: true },
  { ok: true, body: true },
  { ok: true, data: true, body: true },
  { num: 1 },
  { data: 1 },
  { body: 1 },
  { data: 1, body: 1 },
  { num: 1, data: 1 },
  { num: 1, body: 1 },
  { num: 1, data: 1, body: 1 },
]

const err = [
  { binary: Buffer.from([1, 2, 3]), binary2: Buffer.from([1, 2, 3]) },
  { bin: [bin, bin] },
  { list: [Symbol("ok"), [bin]] },
  { data: empty },
  { data: empty, body: empty },
  { bin: empty, data: empty },
  { bin: empty, data: empty, body: empty },
  { max_int64: 9223372036854775807 },
  { min_int64: -9223372036854775808 },
  { large_positive: 1000000000000000 },
  { large_negative: -1000000000000000 },
  { int_array: [9223372036854775807, -9223372036854775808, 0] },
  { boundary_ints: { max: 9223372036854775807, min: -9223372036854775808 } },
  { data: 1, Data: 2, DATA: 3 },
  { body: "a", Body: "b", BODY: "c" },
  { test: 1, Test: 2, TEST: 3, TeSt: 4 },
  { a1: 1, A1: 2, "1a": 3, "1A": 4 },
  { 123: "numeric", abc: "alpha", ABC: "ALPHA" },
  { "field!": 1, "field?": 2, "field#": 3 },
  { "": "empty_key", " ": "space_key", "  ": "two_spaces" },
  {
    data: [1, 9223372036854775807, "text", { val: 2 }],
    Data: [3, -9223372036854775808, [], {}],
    DATA: ["", null, Symbol("undefined"), Symbol("ok")],
  },
  {
    users: [
      { id: 9223372036854775807, name: "user1", tags: ["a", "b"] },
      { id: -9223372036854775808, name: "user2", tags: [] },
    ],
  },
  {
    types: [
      { type: "integer", value: 9223372036854775807 },
      { type: "empty", value: "" },
      { type: "list", value: [1, 2] },
    ],
  },
  {
    field: 1,
    Field: [2],
    FIELD: { data: 3 },
    FiElD: [{ val: 4 }],
  },
  {
    all: [
      0,
      255,
      9223372036854775807,
      -9223372036854775808,
      "",
      [],
      {},
      { a: 1 },
      [2, 3],
      null,
      Symbol("undefined"),
      Symbol("ok"),
    ],
  },
  {
    ultimate: [
      { id: 1, data: "small", meta: { empty: "" } },
      { id: 9223372036854775807, data: [4, 5, 6], meta: {} },
      { id: -9223372036854775808, data: "", meta: { list: [7] } },
    ],
    Data: 255,
    data: 0,
    DATA: [128],
  },
]

function genSimple() {
  const values = {
    emptyBuf: Buffer.from([]),
    buf: Buffer.from([1, 2, 3]),
    emptyObj: {},
    emptyArr: [],
    str: "abc",
    int: 1,
    float: 3.14,
    bool: true,
    sym: Symbol("atom"),
    null: null,
    undef: undefined,
  }

  const testCases = []

  // Helper to check if we should skip this combination
  function shouldSkip(obj) {
    // Skip if data has empty buffer (known server issue)
    if (obj.data && Buffer.isBuffer(obj.data) && obj.data.length === 0) {
      return true
    }
    return false
  }

  // Just data/body
  for (const [, dataVal] of Object.entries(values)) {
    for (const [, bodyVal] of Object.entries(values)) {
      const testCase = { data: dataVal, body: bodyVal }
      if (!shouldSkip(testCase)) {
        testCases.push(testCase)
      }
    }
  }

  // data/body + num (only number values)
  const numValues = [values.int, values.float, 0]
  for (const [, dataVal] of Object.entries(values)) {
    for (const [, bodyVal] of Object.entries(values)) {
      for (const numVal of numValues) {
        const testCase = { data: dataVal, body: bodyVal, num: numVal }
        if (!shouldSkip(testCase)) {
          testCases.push(testCase)
        }
      }
    }
  }

  // data/body + str (only string values)
  for (const [, dataVal] of Object.entries(values)) {
    for (const [, bodyVal] of Object.entries(values)) {
      const testCase = { data: dataVal, body: bodyVal, str: "abc" }
      if (!shouldSkip(testCase)) {
        testCases.push(testCase)
      }
    }
  }

  // data/body + ok (only boolean values)
  for (const [, dataVal] of Object.entries(values)) {
    for (const [, bodyVal] of Object.entries(values)) {
      const testCase = { data: dataVal, body: bodyVal, ok: true }
      if (!shouldSkip(testCase)) {
        testCases.push(testCase)
      }
    }
  }

  // data/body + bin (only buffer values)
  const binValues = [values.emptyBuf, values.buf]
  for (const [, dataVal] of Object.entries(values)) {
    for (const [, bodyVal] of Object.entries(values)) {
      for (const binVal of binValues) {
        const testCase = { data: dataVal, body: bodyVal, bin: binVal }
        if (!shouldSkip(testCase)) {
          testCases.push(testCase)
        }
      }
    }
  }

  return testCases
}
//export default genSimple()
//export default err
export default ok
