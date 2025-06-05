import assert from "assert"
import { after, describe, beforeEach, it, before } from "node:test"
import HyperBEAM from "../../src/hyperbeam.js"
import HB from "../../src/hb.js"
import { getJWK } from "../lib/test-utils.js"
import { wait, toAddr } from "../../src/utils.js"

describe("HyperBEAM stack@1.0 device", function () {
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

  // Test if stack device is accessible
  it("should access stack device", async () => {
    const res = await hb.send({
      path: "/~stack@1.0",
      method: "GET",
    })
    assert(res.status === 200, "Should return 200 status")
  })

  // Test creating a stack with simple devices
  it("should create and execute a simple stack", async () => {
    const res = await hb.send({
      path: "/~wao_test@1.0/test_stack_add",
      method: "GET",
      "initial-value": "0",
    })
    assert(res.status === 200, "Should return 200 status")
    // The stack should add 1 then 2, resulting in 3
    assert(res.body, "Should have a body")
  })

  // Test stack in fold mode (default)
  it("should execute stack in fold mode", async () => {
    const res = await hb.send({
      path: "/~wao_test@1.0/test_stack_fold",
      method: "GET",
      value: "start",
    })
    assert(res.status === 200, "Should return 200 status")
  })

  // Test stack in map mode
  it("should execute stack in map mode", async () => {
    const res = await hb.send({
      path: "/~wao_test@1.0/test_stack_map",
      method: "GET",
      value: "test",
    })
    assert(res.status === 200, "Should return 200 status")
  })

  // Test stack with skip
  it("should handle skip in stack", async () => {
    const res = await hb.send({
      path: "/~wao_test@1.0/test_stack_skip",
      method: "GET",
      value: "test",
    })
    assert(res.status === 200, "Should return 200 status")
  })

  // Test stack with pass (re-execution)
  it("should handle pass in stack", async () => {
    const res = await hb.send({
      path: "/~wao_test@1.0/test_stack_pass",
      method: "GET",
      counter: "0",
    })
    assert(res.status === 200, "Should return 200 status")
  })

  // Test accessing individual device through transform
  it("should access individual device via transform", async () => {
    const res = await hb.send({
      path: "/~wao_test@1.0/test_stack_transform",
      method: "GET",
      input: "hello",
    })
    assert(res.status === 200, "Should return 200 status")
  })

  // Test stack with prefixes
  it("should handle input/output prefixes", async () => {
    const res = await hb.send({
      path: "/~wao_test@1.0/test_stack_prefixes",
      method: "GET",
    })
    assert(res.status === 200, "Should return 200 status")
  })
})
