import assert from "assert"
import { describe, it, before, after, beforeEach } from "node:test"
import { HyperBEAM } from "wao/test"

const cwd = "../HyperBEAM"
const mydev = { name: "mydev@1.0", module: "dev_mydev" }
const devices = ["json", "structured", "httpsig", "flat", "meta", mydev]

describe("Flat Codec", function () {
  let hbeam, hb
  before(async () => {
    hbeam = await new HyperBEAM({ cwd, devices, reset: true }).ready()
  })
  beforeEach(async () => (hb = hbeam.hb))
  after(async () => hbeam.kill())

  it("should encode with flat device", async () => {
    const cases = [
      { a: { b: "v" } },
      { a: "v", b: { c: "v2", d: "v3" } },
      { a: { b: { c: { d: "v" } } } },
    ]
    for (const v of cases) {
      const { body } = await hb.post({
        path: "/~mydev@1.0/flat_to",
        body: JSON.stringify(v),
      })
      console.log(JSON.parse(body))
    }
  })

  it("should decode with flat device", async () => {
    const cases = [
      { "a/b": "v" },
      { a: "v", "b/c": "v2", "b/d": "v3" },
      { "a/b/c/d": "v" },
    ]
    for (const v of cases) {
      const { body } = await hb.post({
        path: "/~mydev@1.0/flat_from",
        body: JSON.stringify(v),
      })
      console.log(JSON.parse(body))
    }
  })
})
