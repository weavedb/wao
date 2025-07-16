import assert from "assert"
import { describe, it, before, after, beforeEach } from "node:test"
import { HyperBEAM } from "wao/test"

const cwd = "../HyperBEAM"
const mydev = { name: "mydev@1.0", module: "dev_mydev" }
const devices = ["json", "structured", "httpsig", "flat", "meta", mydev]

describe("Httpsig Codec", function () {
  let hbeam, hb
  before(async () => {
    hbeam = await new HyperBEAM({ cwd, devices, reset: true }).ready()
  })
  beforeEach(async () => (hb = hbeam.hb))
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
        "body-keys": _,
        "content-type": __,
        "inline-body-key": ___,
        ...decoded
      } = JSON.parse(res3.body)
      console.log(decoded)
      const res4 = await hb.post({
        path: "/~mydev@1.0/structured_to",
        body: JSON.stringify(decoded),
      })
      const json2 = JSON.parse(res4.body)
      assert.deepEqual(json, json2)
    }
  })
})
