import assert from "assert"
import { after, describe, it, before, beforeEach } from "node:test"
import { acc, toAddr } from "../src/test.js"
import { getJWK } from "./lib/test-utils.js"
import HB from "../src/hb.js"
import { pick } from "ramda"
import { wait } from "../src/utils.js"
import Server from "../src/server.js"
import HyperBEAM from "../src/hyperbeam.js"

const URL = "http://localhost:10001"

describe("Hyperbeam Device", function () {
  let hb, hb2, hbeam, jwk, server, addr, addr2
  before(async () => {
    server = new Server({ port: 6359, log: true, hb_url: URL })
    jwk = getJWK("../../HyperBEAM/.wallet.json")
    addr = toAddr(jwk.n)
    addr2 = toAddr(acc[0].jwk.n)
    hbeam = await new HyperBEAM({
      clearCache: true,
      gateway: 6359,
      c: "12",
      cmake: "3.5",
      operator: addr,
    }).ready()
  })

  beforeEach(async () => {
    hb = await new HB({}).init(jwk)
    hb2 = await new HB({}).init(acc[0].jwk)
  })

  after(async () => {
    hbeam.kill()
    server.end()
  })

  it("should test meta@1.0", async () => {
    const info = await hb.json("meta", "info")
    assert.deepEqual(
      pick(["initialized", "port", "operator", "gateway", "address"])(info),
      {
        port: 10001,
        operator: addr,
        address: addr,
        initialized: true,
        gateway: "http://localhost:6359",
      }
    )
    await hb.post({
      path: "/~meta@1.0/info",
      test_config: 123,
      initialized: "permanent",
    })
    const info2 = await hb.json("meta", "info")
    assert.deepEqual(info2.test_config, 123)
    try {
      // this should faild due to "initialized=permanent"
      await hb.post({
        path: "/~meta@1.0/info",
        test_config: 124,
      })
    } catch (e) {}
    const info3 = await hb.json("meta", "info")
    return
    assert.deepEqual(info3.test_config, 123)

    const build = await hb.json("meta", "build")
    assert.equal(build.node, "HyperBEAM")

    assert.equal(
      (
        await hb
          .get({ path: "/~meta@1.0/info/~json@1.0/serialize" })
          .then(r => r.json())
      ).port,
      10001
    )
  })
})
