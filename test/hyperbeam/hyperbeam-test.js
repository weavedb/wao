import assert from "assert"
import { after, describe, beforeEach, it, before } from "node:test"
import HyperBEAM from "../../src/hyperbeam.js"
import HB from "../../src/hb.js"
import { getJWK } from "../lib/test-utils.js"
import { wait, toAddr } from "../../src/utils.js"

describe("HyperBEAM Test Device", function () {
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

  // Test info method
  it("should get device info", async () => {
    const res = await hb.send({ path: "/~test-device@1.0/info", method: "GET" })
    assert(res.status === 200, "Should return 200 status")
    assert(
      res.body.includes("Test device for testing the AO-Core framework"),
      "Should have description"
    )
  })

  // Test test_func method
  it("should execute test_func", async () => {
    const res = await hb.send({
      path: "/~test-device@1.0/test_func",
      method: "GET",
    })
    assert(res.status === 200, "Should return 200 status")
    assert(res.body === "GOOD_FUNCTION", "Should return GOOD_FUNCTION")
  })

  // Test init method
  it("should initialize device state", async () => {
    const res = await hb.send({ path: "/~test-device@1.0/init", method: "GET" })
    assert(res.status === 200, "Should return 200 status")
  })

  // Test restore method with valid data
  it("should restore state", async () => {
    const res = await hb.send({
      path: "/~test-device@1.0/restore",
      method: "GET",
      "already-seen": "[1,2,3]",
    })
    assert(res.status === 200, "Should return 200 status")
  })

  // Test snapshot method
  it("should create snapshot", async () => {
    const res = await hb.send({
      path: "/~test-device@1.0/snapshot",
      method: "GET",
    })
    assert(res.status === 200, "Should return 200 status")
  })

  // Skip compute, mul, update_state, increment_counter, and delay
  // as they require complex setup or specific runtime conditions
})
