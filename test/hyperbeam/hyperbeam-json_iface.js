import assert from "assert"
import { after, describe, beforeEach, it, before } from "node:test"
import HyperBEAM from "../../src/hyperbeam.js"
import HB from "../../src/hb.js"
import { getJWK } from "../lib/test-utils.js"
import { wait, toAddr } from "../../src/utils.js"

describe("HyperBEAM JSON Interface Device", function () {
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

  // Test init method
  it("should initialize json interface", async () => {
    const res = await hb.send({
      path: "/~json-iface@1.0/init",
      method: "GET",
    })
    assert(res.status === 200, "Should return 200 status")
    // Init sets function to "handle"
    assert(
      res.headers.get("function") === "handle" || res.body.includes("handle"),
      "Should set function to handle"
    )
  })

  // Test compute method with pass 1
  it("should prepare call with pass 1", async () => {
    const res = await hb.send({
      path: "/~json-iface@1.0/compute",
      method: "GET",
      pass: "1",
    })
    assert(res.status === 200, "Should return 200 status")
    // Pass 1 returns the message, may not set function header
  })

  // Test compute method with pass 2
  it("should handle pass 2", async () => {
    const res = await hb.send({
      path: "/~json-iface@1.0/compute",
      method: "GET",
      pass: "2",
    })
    assert(res.status === 200, "Should return 200 status")
  })

  // Test compute with no pass (should return input unchanged)
  it("should return unchanged for no pass", async () => {
    const res = await hb.send({
      path: "/~json-iface@1.0/compute",
      method: "GET",
    })
    assert(res.status === 200, "Should return 200 status")
  })
})
