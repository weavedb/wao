import assert from "assert"
import { after, describe, beforeEach, it, before } from "node:test"
import HyperBEAM from "../../src/hyperbeam.js"
import HB from "../../src/hb.js"
import { getJWK } from "../lib/test-utils.js"
import { wait, toAddr } from "../../src/utils.js"

describe("HyperBEAM flat@1.0 codec", function () {
  let hb, hbeam, hb2, addr, jwk, jwk2
  before(async () => {
    hbeam = new HyperBEAM({ c: "12", cmake: "3.5", gateway: 4000 })
    await wait(5000)
    jwk = getJWK("../../HyperBEAM/.wallet.json")
    addr = toAddr(jwk.n)
    jwk2 = getJWK("../../HyperBEAM/hyperbeam-key.json")
  })
  beforeEach(async () => {
    hb = await new HB({}).init(jwk)
    hb2 = await new HB({}).init(jwk2)
  })

  after(async () => {
    hbeam.kill("SIGKILL")
  })

  // Test if flat device is accessible
  it("should access flat device", async () => {
    const res = await hb.send({
      path: "/~flat@1.0",
      method: "GET",
    })
    assert(res.status === 200, "Should return 200 status")
  })

  // Test serialize function - this one works!
  it("should serialize data", async () => {
    const res = await hb.send({
      path: "/~flat@1.0/serialize",
      method: "GET",
      key1: "value1",
      key2: "value2",
    })
    assert(res.status === 200, "Should return 200 status")
    // Check if response contains serialized data
    assert(res.body, "Should have a body")
  })

  // Test commit function - this works too
  it("should handle commit function", async () => {
    const res = await hb.send({
      path: "/~flat@1.0/commit",
      method: "GET",
    })
    assert(res.status === 200, "Should return 200 status")
  })

  // Test committed function - also works
  it("should handle committed function", async () => {
    const res = await hb.send({
      path: "/~flat@1.0/committed",
      method: "GET",
    })
    assert(res.status === 200, "Should return 200 status")
  })
})
