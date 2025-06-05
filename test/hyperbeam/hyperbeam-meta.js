import assert from "assert"
import { after, describe, it, before } from "node:test"
import { prepare } from "./test-utils.js"

describe("HyperBEAM Meta Device", function () {
  let hbeam, server, send

  before(async () => {
    ;({ hbeam, server, send } = await prepare())
  })

  after(async () => {
    hbeam.kill("SIGKILL")
    server.close()
  })

  // Test info method - GET
  it("should get node info via GET", async () => {
    const res = await send({ path: "/~meta@1.0/info", method: "GET" })
    assert(res.status === 200, "Should return 200 status")
    assert(
      res.headers.get("content-type").includes("multipart/form-data"),
      "Should return multipart data"
    )
    assert(
      res.body.includes("preloaded_devices"),
      "Should contain device information"
    )
  })

  // Test info method - POST (unauthorized)
  it("should reject unauthorized POST to info", async () => {
    try {
      await send({
        path: "/~meta@1.0/info",
        method: "POST",
        "new-config": "value",
      })
      assert.fail("Should have rejected unauthorized POST")
    } catch (e) {
      assert(e.message.includes("400"), "Should return 400 Unauthorized")
    }
  })

  // Test info method - GET specific field
  it("should get specific config field", async () => {
    const res = await send({ path: "/~meta@1.0/info/port", method: "GET" })
    assert(res.status === 200, "Should return 200")
    assert(res.body === "10000", "Should return port value")
  })

  // Test build method - GET all build info
  it("should get build info", async () => {
    const res = await send({ path: "/~meta@1.0/build", method: "GET" })
    assert(res.status === 200, "Should return 200")
    assert(
      res.headers.get("node") === "HyperBEAM",
      "Should have node name in headers"
    )
    assert(res.headers.get("version"), "Should have version in headers")
    assert(res.headers.get("source"), "Should have source hash in headers")
    assert(res.headers.get("build-time"), "Should have build time in headers")
  })

  // Test build method - GET specific build field
  it("should get specific build field", async () => {
    const res = await send({ path: "/~meta@1.0/build/node", method: "GET" })
    assert(res.status === 200, "Should return 200")
    assert(res.body === "HyperBEAM", "Should return node name")
  })

  // Test error handling for non-existent paths
  it("should return 404 for non-existent info path", async () => {
    try {
      await send({ path: "/~meta@1.0/info/nonexistent", method: "GET" })
      assert.fail("Should have returned 404")
    } catch (e) {
      assert(e.message.includes("404"), "Should return 404")
    }
  })
})
