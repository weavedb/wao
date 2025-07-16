import assert from "assert"
import { after, describe, it, before, beforeEach } from "node:test"
import Server from "../../src/server.js"
import HyperBEAM from "../../src/hyperbeam.js"
const URL = "http://localhost:10001"

describe("Hyperbeam Device", function () {
  let hb, hbeam, server
  before(async () => {
    server = new Server({ port: 6359, log: true, hb_url: URL })
    hbeam = await new HyperBEAM({ reset: true, gateway: 6359 }).ready()
  })
  beforeEach(async () => (hb = hbeam.hb))

  after(async () => {
    hbeam.kill()
    server.end()
  })

  it("should test relay@1.0", async () => {
    assert.equal(
      1,
      JSON.parse(
        (
          await hb.getJSON({
            path: "/~relay@1.0/call",
            "relay-path": "http://localhost:6359",
          })
        ).body
      ).version
    )
    const cast = await hb.getJSON({
      path: "/~relay@1.0/cast",
      "relay-path": "http://localhost:6359",
    })
    assert.equal(cast.body, "OK")
  })
})
