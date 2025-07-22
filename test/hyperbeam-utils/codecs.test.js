import { describe, it } from "node:test"
import assert from "assert"
import { erl_json_from, erl_json_to, normalize } from "../../src/erl_json.js"
import { erl_str_from, erl_str_to } from "../../src/erl_str.js"

describe("Erlang Codec Tests", () => {
  // Test data covering all types
  const testCases = [
    // Basic types
    { name: "null", value: null },
    {
      name: "object with undefined",
      value: { a: 1, b: undefined, c: 3 },
      jsonExpected: { a: 1, c: 3 }, // undefined removed
      erlExpected: { a: 1, b: undefined, c: 3 },
    }, // undefined preserved in Erlang (strings→buffers)
    { name: "true", value: true },
    { name: "false", value: false },
    { name: "integer", value: 42 },
    { name: "negative integer", value: -123 },
    { name: "float", value: 3.14159 },
    { name: "string", value: "hello world" },
    { name: "empty string", value: "" },

    // Symbols/Atoms
    { name: "symbol", value: Symbol.for("atom") },
    { name: "unique symbol", value: Symbol("unique") }, // normalize converts to global
    { name: "symbol null", value: Symbol("null") }, // normalize converts to null
    { name: "symbol true", value: Symbol("true") }, // normalize converts to true
    { name: "symbol false", value: Symbol("false") }, // normalize converts to false

    // Buffers/Binaries
    { name: "buffer", value: Buffer.from([1, 2, 3]) },
    { name: "empty buffer", value: Buffer.alloc(0) },
    { name: "buffer with text", value: Buffer.from("test") },
    { name: "buffer with non-UTF8", value: Buffer.from([255, 254, 253]) },
    {
      name: "buffer with unicode",
      value: Buffer.from("Hello 世界 😀", "utf8"),
    },

    // Collections
    { name: "empty array", value: [] },
    { name: "empty object", value: {} },
    { name: "array", value: [1, "two", true, null] },
    {
      name: "array with undefined",
      value: [1, undefined, 3],
      jsonExpected: [1, null, 3], // undefined → null in JSON
      erlExpected: [1, undefined, 3],
    }, // undefined preserved in Erlang
    { name: "object", value: { a: 1, b: "two", c: true } },

    // Nested structures
    {
      name: "nested",
      value: {
        num: 42,
        str: "test",
        sym: Symbol.for("atom"),
        buf: Buffer.from([1, 2, 3]),
        arr: [1, 2, 3],
        obj: { nested: true },
      },
    },

    // Complex test case
    {
      name: "complex",
      value: {
        types: ["", 0, false, Symbol("null"), [], {}, Buffer.alloc(0)],
        nested: {
          data: Buffer.from([255, 0, 128]),
          list: [Symbol.for("test"), Buffer.from("hello")],
        },
      },
    },
  ]

  describe("JSON Codec (erl_json_to/from)", () => {
    testCases.forEach(({ name, value, jsonExpected }) => {
      it(`should round-trip ${name}`, () => {
        const json = erl_json_to(value)
        console.log(
          `${name}: ${JSON.stringify(value)} → ${JSON.stringify(json)}`
        )

        const result = erl_json_from(json)
        const expected =
          jsonExpected !== undefined ? jsonExpected : normalize(value, false)

        assert.deepStrictEqual(result, expected)
      })
    })

    it("should handle special JSON annotations", () => {
      // Test direct JSON parsing
      assert.deepStrictEqual(
        erl_json_from({ $empty: "binary" }),
        Buffer.alloc(0)
      )
      assert.deepStrictEqual(erl_json_from({ $empty: "list" }), [])
      assert.deepStrictEqual(erl_json_from({ $empty: "map" }), {})
      assert.deepStrictEqual(erl_json_from(":AQID:"), Buffer.from([1, 2, 3]))
      assert.deepStrictEqual(erl_json_from("%atom%"), Symbol.for("atom"))
    })
  })

  describe("Erlang String Codec (erl_str_to/from)", () => {
    testCases.forEach(({ name, value, erlExpected }) => {
      it(`should round-trip ${name}`, () => {
        const erlStr = erl_str_to(value)
        console.log(`${name}: ${JSON.stringify(value)} → ${erlStr}`)

        const result = erl_str_from(erlStr)
        const expected =
          erlExpected !== undefined ? erlExpected : normalize(value, true)

        assert.deepStrictEqual(result, expected)
      })
    })

    it("should parse Erlang syntax correctly", () => {
      // Test direct Erlang string parsing
      assert.deepStrictEqual(erl_str_from("<<1,2,3>>"), Buffer.from([1, 2, 3]))
      assert.deepStrictEqual(erl_str_from('<<"hello">>'), Buffer.from("hello"))
      assert.deepStrictEqual(erl_str_from("atom"), Symbol.for("atom"))
      assert.deepStrictEqual(erl_str_from("[]"), [])
      assert.deepStrictEqual(erl_str_from("#{}"), {})
      assert.deepStrictEqual(erl_str_from('#{<<"key">> => <<"value">>}'), {
        key: Buffer.from("value"),
      })
    })
  })

  describe("normalize function", () => {
    it("should normalize undefined in objects and arrays", () => {
      // undefined is removed from objects
      assert.deepStrictEqual(normalize({ a: 1, b: undefined, c: 3 }, false), {
        a: 1,
        c: 3,
      })

      // undefined stays in arrays
      assert.deepStrictEqual(normalize([1, undefined, 3], false), [
        1,
        undefined,
        3,
      ])
    })

    it("should convert unique symbols to global", () => {
      const unique = Symbol("test")
      const normalized = normalize(unique, false)
      assert.strictEqual(normalized, Symbol.for("test"))
      assert.strictEqual(Symbol.keyFor(normalized), "test")
    })

    it("should convert special symbols to primitives", () => {
      assert.strictEqual(normalize(Symbol("null"), false), null)
      assert.strictEqual(normalize(Symbol("true"), false), true)
      assert.strictEqual(normalize(Symbol("false"), false), false)
    })

    it("should normalize nested structures", () => {
      const input = {
        a: undefined,
        b: Symbol("test"),
        c: [Symbol("null"), Symbol("unique"), undefined],
        d: { e: Symbol("true"), f: undefined },
      }

      const expected = {
        // a is removed (undefined)
        b: Symbol.for("test"),
        c: [null, Symbol.for("unique"), undefined], // undefined stays in arrays
        d: { e: true }, // f is removed (undefined)
      }

      assert.deepStrictEqual(normalize(input, false), expected)
    })

    it("should convert strings to buffers when requested", () => {
      assert.deepStrictEqual(normalize("hello", true), Buffer.from("hello"))
      assert.deepStrictEqual(normalize("hello", false), "hello")

      const obj = { a: "test", b: 123 }
      assert.deepStrictEqual(normalize(obj, true), {
        a: Buffer.from("test"),
        b: 123,
      })
      assert.deepStrictEqual(normalize(obj, false), { a: "test", b: 123 })
    })
  })

  describe("Cross-codec compatibility", () => {
    it("should handle the different string/buffer behavior", () => {
      const original = {
        str: "hello",
        num: 42,
        sym: Symbol.for("test"),
        buf: Buffer.from([1, 2, 3]),
        arr: [1, 2, 3],
        obj: { nested: true },
      }

      // JS → JSON → JS (strings stay strings)
      const json = erl_json_to(original)
      const fromJson = erl_json_from(json)
      assert.deepStrictEqual(fromJson, normalize(original, false))

      // JS → Erlang String → JS (strings become buffers)
      const erlStr = erl_str_to(original)
      const fromErlStr = erl_str_from(erlStr)
      assert.deepStrictEqual(fromErlStr, normalize(original, true))
    })
  })

  describe("Edge cases", () => {
    it("should handle buffer edge cases", () => {
      // All possible byte values
      const allBytes = Buffer.from(Array.from({ length: 256 }, (_, i) => i))
      const json = erl_json_to(allBytes)
      assert.deepStrictEqual(erl_json_from(json), allBytes)

      const erlStr = erl_str_to(allBytes)
      assert.deepStrictEqual(erl_str_from(erlStr), allBytes)
    })

    it("should handle deeply nested structures", () => {
      const deep = { a: { b: { c: { d: { e: Buffer.from("deep") } } } } }

      const json = erl_json_to(deep)
      assert.deepStrictEqual(erl_json_from(json), deep)

      const erlStr = erl_str_to(deep)
      // In Erlang codec, strings become buffers (but this is already a buffer)
      assert.deepStrictEqual(erl_str_from(erlStr), deep)
    })

    it("should handle special characters in strings", () => {
      const special = 'Line1\nLine2\tTab"Quote\\Backslash'

      const json = erl_json_to(special)
      assert.deepStrictEqual(erl_json_from(json), special)

      const erlStr = erl_str_to(special)
      // In Erlang codec, strings become buffers
      assert.deepStrictEqual(erl_str_from(erlStr), Buffer.from(special))
    })
  })
})

// Run the tests
console.log("Running codec tests...")
