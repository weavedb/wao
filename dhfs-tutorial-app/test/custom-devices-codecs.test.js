import assert from "assert"
import { describe, it, before, after, beforeEach } from "node:test"
import { HyperBEAM } from "wao/test"

const cwd = "../HyperBEAM"
const mydev = { name: "mydev@1.0", module: "dev_mydev" }
const devices = ["json", "structured", "httpsig", "flat", "meta", mydev]

describe("Custom Devices and Codecs", function () {
  let hbeam, hb
  before(async () => {
    hbeam = await new HyperBEAM({ cwd, devices, reset: true }).ready()
  })
  beforeEach(async () => (hb = hbeam.hb))
  after(async () => hbeam.kill())

  it("should run a test case", async () => {
    const { headers, body } = await hb.get({ path: "/~mydev@1.0/info" })
    console.log(headers)
    console.log(body)
    assert.equal(headers.version, "1.0")
  })

  it("should run a test case", async () => {
    const { body } = await hb.get({ path: "/~mydev@1.0/info_json" })
    const json = JSON.parse(body)
    assert.deepEqual(json, { version: "1.0" })
  })

  it("should greet", async () => {
    const { body } = await hb.post({
      path: "/~mydev@1.0/hello",
      body: JSON.stringify({ name: "Wao" }),
    })
    const { hello } = JSON.parse(body)
    assert.equal(hello, "Hello, Wao!")
  })

  it("should forward with GET", async () => {
    const { body } = await hb.get({ path: "/~mydev@1.0/forward", key: "abc" })
    console.log(JSON.parse(body))
  })

  it("should forward with POST", async () => {
    const { body } = await hb.post({
      path: "/~mydev@1.0/forward",
      key: "abc",
      list: [1, 2, 3],
      map: { abc: "123" },
      bool: true,
      body: "test_body",
    })
    console.log(JSON.parse(body))
  })
})
