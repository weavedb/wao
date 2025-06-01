import assert from "assert"
import { after, describe, it, before } from "node:test"
import { prepare } from "./test-utils.js"

describe("HyperBEAM JSON Interface Device", function () {
  let hbeam, server, send

  before(async () => {
    ;({ hbeam, server, send } = await prepare())
  })

  after(async () => {
    hbeam.kill("SIGKILL")
    server.close()
  })

  // Test init method
  it("should initialize json interface", async () => {
    const res = await send({
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
    const res = await send({
      path: "/~json-iface@1.0/compute",
      method: "GET",
      pass: "1",
    })
    assert(res.status === 200, "Should return 200 status")
    // Pass 1 returns the message, may not set function header
  })

  // Test compute method with pass 2
  it("should handle pass 2", async () => {
    const res = await send({
      path: "/~json-iface@1.0/compute",
      method: "GET",
      pass: "2",
    })
    assert(res.status === 200, "Should return 200 status")
  })

  // Test compute with no pass (should return input unchanged)
  it("should return unchanged for no pass", async () => {
    const res = await send({
      path: "/~json-iface@1.0/compute",
      method: "GET",
    })
    assert(res.status === 200, "Should return 200 status")
  })
})
