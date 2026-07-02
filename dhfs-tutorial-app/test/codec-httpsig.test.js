import assert from "assert"
import { describe, it, before, after } from "node:test"
import { HyperBEAM } from "wao/test"

describe("Httpsig Codec", function () {
  let hbeam, hb
  before(async () => {
    // v0.9-FINAL linkifies nested objects in responses; the round-trip
    // pipeline test needs inline values to compare against the input.
    hbeam = await new HyperBEAM({ reset: true, linkify_mode: false }).ready()
    hb = hbeam.hb
  })
  after(async () => hbeam.kill())

  it("should encode with httpsig device", async () => {
    const cases = [
      {
        a: {
          "ao-types": 'b="list"',
          b: '"(ao-type-integer) 1", "(ao-type-integer) 2", "(ao-type-integer) 3"',
        },
        c: {
          "ao-types": 'd="list"',
          d: '"(ao-type-float) 3.14", "(ao-type-atom) \\"true\\"", "str"',
        },
      },
    ]
    for (const v of cases) {
      const { body } = await hb.post({
        path: "/~mydev@1.0/httpsig_to",
        body: JSON.stringify(v),
      })
      console.log(JSON.parse(body))
    }
  })

  it("should decode with httpsig device", async () => {
    const cases = [
      {
        body:
          "--rqDK_isKBhMozuATy4K6NFgdADGNHedXoUEDN10AANo\r\n" +
          'ao-types: b="list"\r\n' +
          'b: "(ao-type-integer) 1", "(ao-type-integer) 2", "(ao-type-integer) 3"\r\n' +
          'content-disposition: form-data;name="a"\r\n' +
          "--rqDK_isKBhMozuATy4K6NFgdADGNHedXoUEDN10AANo\r\n" +
          'ao-types: d="list"\r\n' +
          'content-disposition: form-data;name="c"\r\n' +
          'd: "(ao-type-float) 3.14", "(ao-type-atom) \\"true\\"", "str"\r\n' +
          "--rqDK_isKBhMozuATy4K6NFgdADGNHedXoUEDN10AANo--",
        "body-keys": '"a", "c"',
        "content-digest":
          "sha-256=:mv08FUN7TpjmiHhagrxwqgjS7kQ/HY2+If2hIUq/y54=:",
        "content-type":
          'multipart/form-data; boundary="rqDK_isKBhMozuATy4K6NFgdADGNHedXoUEDN10AANo"',
      },
    ]
    for (const v of cases) {
      const { body } = await hb.post({
        path: "/~mydev@1.0/httpsig_from",
        body: JSON.stringify(v),
      })
      console.log(JSON.parse(body))
    }
  })

  it("should encode / decode in the pipeline", async () => {
    // normalize codec results: strip ao-types, convert 1-based indexed
    // objects back to 0-based arrays, convert atom strings to primitives
    const normalize = (obj) => {
      if (obj === null || obj === undefined) return obj
      if (typeof obj !== "object") return obj
      if (Array.isArray(obj)) return obj.map(normalize)
      const { "ao-types": _, ...rest } = obj
      // Check if this is a 1-based indexed object (Erlang list)
      const keys = Object.keys(rest)
      const isIndexed =
        keys.length > 0 &&
        keys.every((k) => /^\d+$/.test(k)) &&
        keys.map(Number).sort((a, b) => a - b)[0] <= 1
      if (isIndexed) {
        const sorted = keys.map(Number).sort((a, b) => a - b)
        const base = sorted[0]
        return sorted.map((k) => {
          const v = rest[String(k)]
          if (v === "true") return true
          if (v === "false") return false
          return normalize(v)
        })
      }
      const result = {}
      for (const [k, v] of Object.entries(rest)) {
        result[k] = normalize(v)
      }
      return result
    }

    const cases = [
      { list: [1, true, "abc"] },
      { nested_list: [1, [2, 3]] },
      { a: { b: [1, 2, 3] } },
      { a: [1, 2], b: [3, 4] },
      { empty_list: [], empty_binary: "", empty_message: {} },
      { data: "abc", [hb.addr]: 123 },
      { list: [1, 2, 3], map: { a: { b: { c: 4 } } } },
    ]
    for (const json of cases) {
      const res = await hb.post({
        path: "/~mydev@1.0/structured_from",
        body: JSON.stringify(json),
      })
      const structured = JSON.parse(res.body)
      console.log(structured)
      const res2 = await hb.post({
        path: "/~mydev@1.0/httpsig_to",
        body: JSON.stringify(structured),
      })
      const encoded = JSON.parse(res2.body)
      console.log(encoded)
      const res3 = await hb.post({
        path: "/~mydev@1.0/httpsig_from",
        body: JSON.stringify(encoded),
      })

      // omit: body-keys, content-type, inline-body-key
      const {
        "body-keys": _b,
        "content-type": _c,
        "inline-body-key": _i,
        ...decoded
      } = JSON.parse(res3.body)
      console.log(decoded)
      const res4 = await hb.post({
        path: "/~mydev@1.0/structured_to",
        body: JSON.stringify(decoded),
      })
      const json2 = normalize(JSON.parse(res4.body))
      assert.deepEqual(json, json2)
    }
  })
})
