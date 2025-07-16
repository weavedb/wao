import assert from "assert"
import { describe, it, before, after, beforeEach } from "node:test"
import { HyperBEAM } from "wao/test"

const cwd = "../HyperBEAM"
const mydev = { name: "mydev@1.0", module: "dev_mydev" }
const devices = ["json", "structured", "httpsig", "flat", "meta", mydev]

describe("Structured Codec", function () {
  let hbeam, hb
  before(async () => {
    hbeam = await new HyperBEAM({ cwd, devices, reset: true }).ready()
  })
  beforeEach(async () => (hb = hbeam.hb))
  after(async () => hbeam.kill())

  it("should encode with structured device", async () => {
    const cases = [
      { list: [1, true, "abc"] },
      { nested_list: [1, [2, 3]] },
      { a: { b: [1, 2, 3] } },
      { a: [1, 2], b: [3, 4] },
      { empty_list: [], empty_binary: "", empty_message: {} },
    ]
    for (const v of cases) {
      const { body } = await hb.post({
        path: "/~mydev@1.0/structured_from",
        body: JSON.stringify(v),
      })
      console.log(JSON.parse(body))
    }
  })

  it("should decode with structured device", async () => {
    const cases = [
      {
        "ao-types": 'list="list"',
        list: '"(ao-type-integer) 1", "(ao-type-atom) \\"true\\"", "abc"',
      },
      {
        "ao-types": 'nested_list="list"',
        nested_list:
          '"(ao-type-integer) 1", "(ao-type-list) \\"(ao-type-integer) 2\\", \\"(ao-type-integer) 3\\""',
      },
      {
        a: {
          "ao-types": 'b="list"',
          b: '"(ao-type-integer) 1", "(ao-type-integer) 2", "(ao-type-integer) 3"',
        },
      },
      {
        a: '"(ao-type-integer) 1", "(ao-type-integer) 2"',
        "ao-types": 'a="list", b="list"',
        b: '"(ao-type-integer) 3", "(ao-type-integer) 4"',
      },
      {
        "ao-types":
          'empty_binary="empty-binary", empty_list="empty-list", empty_message="empty-message"',
      },
    ]
    for (const v of cases) {
      const { body } = await hb.post({
        path: "/~mydev@1.0/structured_to",
        body: JSON.stringify(v),
      })
      console.log(JSON.parse(body))
    }
  })
})
