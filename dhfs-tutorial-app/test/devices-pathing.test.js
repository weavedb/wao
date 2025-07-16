import assert from "assert"
import { describe, it, before, after, beforeEach } from "node:test"
import { HyperBEAM } from "wao/test"

const cwd = "../HyperBEAM"
const devices = ["json", "structured", "httpsig", "flat", "meta"]

describe("Devices and Pathing", function () {
  let hbeam, hb
  before(async () => {
    hbeam = await new HyperBEAM({ cwd, devices, reset: true }).ready()
  })
  beforeEach(async () => (hb = hbeam.hb))
  after(async () => hbeam.kill())

  it("should run a test case", async () => {
    /* write your test here */
  })

  it("should get node info", async () => {
    const res = await fetch("http://localhost:10001/~meta@1.0/info")
    const headers = res.headers
    const body = await res.text()
    console.log(headers)
    console.log(body)
  })

  it("should decode a response", async () => {
    const { out } = await hb.get({ path: "/~meta@1.0/info" })
    console.log(out)
  })

  it("should set new configs", async () => {
    // set a new config
    await hb.post({
      path: "/~meta@1.0/info",
      test_config: "abc",
      test_config2: 123,
      test_config3: { abc: 123 },
    })

    // get info
    const { out } = await hb.get({ path: "/~meta@1.0/info" })
    assert.equal(out.test_config, "abc")
    assert.equal(out.test_config2, 123)
    assert.deepEqual(out.test_config3, { abc: 123 })
  })

  it("should get a specific key", async () => {
    // getting the node operator wallet address
    const { out: address } = await hb.get({ path: "/~meta@1.0/info/address" })
    assert.equal(address, hb.addr)
  })

  it("should permanently freeze the configs", async () => {
    await hb.post({ path: "/~meta@1.0/info", initialized: "permanent" })

    // this should fail
    await assert.rejects(
      hb.post({ path: "/~meta@1.0/info", test_config: "def" })
    )
    const { out: test_config } = await hb.get({
      path: "/~meta@1.0/info/test_config",
    })
    assert.equal(test_config, "abc")
  })
})

describe("Devices and Pathing #2", function () {
  let hbeam, hb
  before(async () => {
    hbeam = await new HyperBEAM({ cwd, devices, reset: true }).ready()
  })
  beforeEach(async () => (hb = hbeam.hb))
  after(async () => hbeam.kill())

  it("should use shortcut methods", async () => {
    // set a new config
    await hb.p("/~meta@1.0/info", { test_config4: "def" })

    // get info
    const { test_config4 } = await hb.g("/~meta@1.0/info")
    assert.equal(test_config4, "def")
  })

  it("should serialize with json@1.0", async () => {
    const { body: json } = await hb.g("/~meta@1.0/info/~json@1.0/serialize")
    console.log(JSON.parse(json))
  })
})
