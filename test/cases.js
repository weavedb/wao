const bin = Buffer.from([1, 2, 3])

const undone = []

const ok = [
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
]
const ok2 = [
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
]
const err = [
  //  { binary: Buffer.from([1, 2, 3]), binary2: Buffer.from([1, 2, 3]) },
  //{ bin: [bin, bin] },
  //{ body: bin, data: bin },
  //{ list: [Symbol("ok")] },
  //{ list: [Symbol("ok"), [bin]] },
]

let keys = [
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
]

export default [...ok, ...ok2, ...keys]
