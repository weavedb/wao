import assert from "assert"
import { after, describe, it, before, beforeEach } from "node:test"
import { acc, toAddr } from "../src/test.js"
import { getJWK } from "./lib/test-utils.js"
import HB from "../src/hb.js"
import { pick } from "ramda"
import { wait } from "../src/utils.js"
import Server from "../src/server.js"
import HyperBEAM from "../src/hyperbeam.js"
const seed = num => {
  const array = new Uint8Array(num)
  return crypto.getRandomValues(array).toString()
}

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
  it.only("should test stack@1.0", async () => {
    const { pid } = await hb.spawn({
      "execution-device": "stack@1.0",
      "device-stack": ["wao@1.0", "double@1.0"],
    })
    // [0] 1 * 2 = 2, [1] (2 + 1) * 2 = 6, 2 (3 + 1) * 2 = 14
    await hb.schedule({ pid })
    await hb.schedule({ pid })
    assert.equal((await hb.compute({ pid, slot: 0 })).count, 2)
    assert.equal((await hb.compute({ pid, slot: 1 })).count, 6)
    assert.equal((await hb.compute({ pid, slot: 2 })).count, 14)
  })
  it("should test stack@1.0", async () => {
    const { pid } = await hb.spawn({
      "execution-device": "stack@1.0",
      "device-stack": ["wao@1.0", "double@1.0", "double@1.0"],
    })
    // [0] 1 * 2 * 2 = 4, [1] (4 + 1) * 2 * 2 = 20, 2 (20 + 1) * 2 * 2= 84
    await hb.schedule({ pid })
    await hb.schedule({ pid })
    assert.equal((await hb.compute({ pid, slot: 0 })).count, 4)
    assert.equal((await hb.compute({ pid, slot: 1 })).count, 20)
    assert.equal((await hb.compute({ pid, slot: 2 })).count, 84)
  })
})
