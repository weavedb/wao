import assert from "assert"
import { after, describe, it, before } from "node:test"
import { prepare } from "./test-utils.js"

describe("HyperBEAM flat@1.0 codec", function () {
  let hbeam, server, send

  before(async () => {
    ;({ hbeam, server, send } = await prepare())
  })

  after(async () => {
    hbeam.kill("SIGKILL")
    server.close()
  })

  // Test if flat device is accessible
  it("should access flat device", async () => {
    const res = await send({
      path: "/~flat@1.0",
      method: "GET",
    })
    assert(res.status === 200, "Should return 200 status")
  })

  // Test serialize function - this one works!
  it("should serialize data", async () => {
    const res = await send({
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
    const res = await send({
      path: "/~flat@1.0/commit",
      method: "GET",
    })
    assert(res.status === 200, "Should return 200 status")
  })

  // Test committed function - also works
  it("should handle committed function", async () => {
    const res = await send({
      path: "/~flat@1.0/committed",
      method: "GET",
    })
    assert(res.status === 200, "Should return 200 status")
  })
})
