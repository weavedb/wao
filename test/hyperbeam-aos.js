import assert from "assert"
import { after, describe, it, before, beforeEach } from "node:test"
import { wait, toAddr } from "../src/utils.js"
import { getJWK } from "./lib/test-utils.js"
import HB from "../src/hb.js"
import HyperBEAM from "../src/hyperbeam.js"
import { resolve } from "path"
import { readFileSync } from "fs"
import gateway from "./lib/gateway.js"
import Server from "../src/server.js"

const src_data = `
local count = 0
Handlers.add("Add", "Add", function (msg)
  count = count + tonumber(msg.Plus)
end)

Handlers.add("Get", "Get", function (msg)
  msg.reply({ Data = tostring(count) })
end)
`

const URL = "http://localhost:10001"

describe("HyperBEAM", function () {
  let hb, hbeam, hb2, addr, jwk, jwk2, server

  before(async () => {
    server = new Server({ port: 6359, log: true, hb_url: URL })
    hbeam = await new HyperBEAM({
      c: "12",
      cmake: "3.5",
      gateway: 6359,
    }).ready()
    jwk = getJWK("../../HyperBEAM/.wallet.json")
    addr = toAddr(jwk.n)
    jwk2 = getJWK("../../HyperBEAM/hyperbeam-key.json")
  })

  beforeEach(async () => {
    hb = await new HB({}).init(jwk)
    hb2 = await new HB({}).init(jwk2)
  })
  after(async () => {
    hbeam.kill()
    server.end()
  })

  it("should query wao device", async () => {
    assert.equal(await hb.text("wao", "info/version"), "1.0")
    const { pid } = await hb.spawn({})
    const { slot, res } = await hb.message({ pid })
    assert.equal((await hb.messages({ pid })).edges.length, 2)
  })

  it("should handle counter with Add and Get handlers", async () => {
    const { pid } = await hb.spawnAOS()
    await hb.messageAOS({ pid, action: "Eval", tags: {}, data: src_data })
    await hb.messageAOS({ pid, action: "Add", tags: { Plus: "3" } })
    assert.equal(
      (await hb.messageAOS({ pid, action: "Get" })).outbox["1"].data,
      "3"
    )
  })

  it("should execute AOS with WAMR", async () => {
    const pid = await hb.spawnAOS()
    await hb.messageAOS({ action: "Eval", tags: {}, data: src_data })
    await hb.messageAOS({ action: "Add", tags: { Plus: "3" } })
    assert.equal((await hb.messageAOS({ action: "Get" })).outbox["1"].data, "3")
    await hb.messageAOS({ action: "Add", tags: { Plus: "3" } })
    assert.equal((await hb.messageAOS({ action: "Get" })).outbox["1"].data, "6")
  })

  it("should query meta device", async () => {
    await hb.dev.meta.info({ method: "post", abc: "def" })
    const info = await hb.dev.meta.info()
    assert.equal(info.abc, "def")
    const build = await hb.dev.meta.build()
    assert.equal(build.node, "HyperBEAM")
    const metrics = await hb.dev.hyperbuddy.metrics({})
  })
})
