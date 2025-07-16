import assert from "assert"
import { after, describe, it, before, beforeEach } from "node:test"
import { acc, toAddr } from "../src/test.js"
import { getJWK } from "./lib/test-utils.js"
import HB from "../src/hb.js"
import { pick } from "ramda"
import { wait } from "../src/utils.js"
import HyperBEAM from "../src/hyperbeam.js"
const seed = num => {
  const array = new Uint8Array(num)
  return crypto.getRandomValues(array).toString()
}

const URL = "http://localhost:10001"

describe("Hyperbeam Device", function () {
  let hb, hb2, hbeam, jwk, addr, addr2
  before(async () => {
    jwk = getJWK("../../HyperBEAM/.wallet.json")
    addr = toAddr(jwk.n)
    addr2 = toAddr(acc[0].jwk.n)
    hbeam = await new HyperBEAM({ clearCache: true }).ready()
  })

  beforeEach(async () => {
    hb = await new HB({}).init(jwk)
    hb2 = await new HB({}).init(acc[0].jwk)
  })

  after(async () => {
    hbeam.kill()
  })

  it("should test cron@1.0", async () => {
    const { pid } = await hb.spawn({ "execution-device": "wao@1.0" })
    const { body: task } = await hb.post({
      path: "/~cron@1.0/every",
      "cron-path": `/~wao@1.0/cron`,
      interval: "1000-milliseconds",
      target: pid,
    })
    await wait(3000)
    await hb.post({ path: "/~cron@1.0/stop", task: task })
    const { count } = await hb.now({ pid })
    await wait(3000)
    const { count: count2 } = await hb.now({ pid })
    assert.equal(count, 4)
    assert.equal(count, count2)
  })
})
