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

const data = `
local count = 0
Handlers.add("Inc", "Inc", function (msg)
  count = count + 1
  msg.reply({ Data = "Count: "..tostring(count) })
end)

Handlers.add("Get", "Get", function (msg)
  msg.reply({ Data = "Count: "..tostring(count) })
end)`

const src_data = `
local count = 0
Handlers.add("Add", "Add", function (msg)
  count = count + tonumber(msg.Plus)
end)

Handlers.add("Get", "Get", function (msg)
  msg.reply({ Data = tostring(count) })
end)
`

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

  it("should upload module", async () => {
    const {
      headers: { process: pid },
    } = await hb.post({
      device: "process@1.0",
      path: "/schedule",
      scheduler: addr,
      "execution-device": "wao@1.0",
    })
    await hb.postJSON({ path: `/${pid}/schedule` })
    await hb.postJSON({ path: `/${pid}/schedule` })
    await hb.postJSON({ path: `/${pid}/schedule` })
    const { count } = await hb.getJSON({ path: `/${pid}/compute`, slot: 3 })
    assert.equal(count, 4)
    await hb.postJSON({ path: `/${pid}/schedule` })
    const now = await hb.getJSON({ path: `/${pid}/now` })
    assert.equal(now.count, 5)
  })

  it.only("should upload module", async () => {
    const { pid } = await hb.spawn({ "execution-device": "wao@1.0" })
    await hb.schedule({ pid })
    await hb.schedule({ pid })
    await hb.schedule({ pid })
    const { count } = await hb.compute({ pid, slot: 2 })
    assert.equal(count, 3)
    await hb.schedule({ pid })
    assert.equal((await hb.now({ pid })).count, 5)
  })
})
