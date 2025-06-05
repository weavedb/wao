import assert from "assert"
import { after, describe, it, before, beforeEach } from "node:test"
import { wait, toAddr } from "../src/utils.js"
import { getJWK } from "./lib/test-utils.js"
import HB from "../src/hb.js"
import HyperBEAM from "../src/hyperbeam.js"
import { resolve } from "path"
import { readFileSync } from "fs"
import gateway from "./lib/gateway.js"

const src_data = `
local count = 0
Handlers.add("Add", "Add", function (msg)
  count = count + tonumber(msg.Plus)
end)

Handlers.add("Get", "Get", function (msg)
  msg.reply({ Data = tostring(count) })
end)
`

describe("HyperBEAM", function () {
  let hb, hbeam, hb2, addr, jwk, jwk2
  before(async () => {
    hbeam = new HyperBEAM({ c: "12", cmake: "3.5", gateway: 4000 })
    await wait(5000)
    jwk = getJWK("../../HyperBEAM/.wallet.json")
    addr = toAddr(jwk.n)
    jwk2 = getJWK("../../HyperBEAM/hyperbeam-key.json")
  })
  beforeEach(() => {
    hb = new HB({ jwk })
    hb2 = new HB({ jwk: jwk2 })
  })
  after(async () => hbeam.kill())

  it("should query wao device", async () => {
    assert.equal(await hb.text("wao", "info/version"), "1.0")
    const { pid } = await hb.spawn()
    const { slot, res } = await hb.message({ pid })
    assert.equal((await hb.messages({ target: pid })).edges.length, 1)
  })

  it("should handle counter with Add and Get handlers", async () => {
    const pid = await hb2.spawnAOS()
    console.log(await hb2.messageAOS("Eval", {}, src_data))
    await hb.messageAOS("Add", { Plus: "3" })
    assert.equal((await hb.messageAOS("Get")).outbox["1"].data, "3")
  })

  it("should execute AOS with WAMR", async () => {
    const result = await hb.send({
      path: "/~wao@1.0/cache_wasm_image",
      method: "POST",
      filename: "test/aos-2-pure-xs.wasm",
    })
    const id = result.headers.get("image")
    const pid = await hb.spawnAOS(id)
    await hb.messageAOS("Eval", {}, src_data)
    await hb.messageAOS("Add", { Plus: "3" })
    assert.equal((await hb.messageAOS("Get")).outbox["1"].data, "3")
  })

  it("should query meta device", async () => {
    await hb.meta.info({ method: "post", abc: "def" })
    const info = await hb.meta.info()
    assert.equal(info.abc, "def")
    const build = await hb.meta.build()
    assert.equal(build.node, "HyperBEAM")
    const metrics = await hb.hyperbuddy.metrics({})
    console.log(metrics)
  })

  it("should deserialize json", async () => {
    assert.equal(
      (await hb.json.deserialize({ body: JSON.stringify({ a: 3 }) })).status,
      200
    )
  })

  it("should spawn module outside", async () => {
    const binary = readFileSync(
      resolve(import.meta.dirname, "../HyperBEAM/test/aos-2-pure-xs.wasm")
    )
    const { pid } = await hb.spawn({})
    const { slot, res } = await hb.message({
      data: Buffer.from(binary).toString("base64"),
    })
    const msgs = await hb.messages({ target: pid, from: 1, limit: 10 })
    const image = msgs.edges[0].node.message.Id
    const pid2 = await hb.spawnAOS(image)
    hb.pid = pid2
    await hb.messageAOS("Eval", {}, src_data)
  })

  it.skip("should spawn module outside", async () => {
    await hb.meta.info({ cache_writers: [addr], method: "POST" })
    const binary = readFileSync(
      resolve(import.meta.dirname, "../HyperBEAM/test/aos-2-pure-xs.wasm")
    )
    const res = await hb.send({
      path: "/~cache@1.0/write",
      body: binary,
      "content-type": "application/wasm",
    })
    console.log(res)
    const image = res.headers.get("path").split("/").pop()
    console.log("image................", image)
    const pid = await hb.spawnAOS(image)
    await hb.messageAOS("Eval", {}, src_data)
    await hb.messageAOS("Add", { Plus: "3" })
    assert.equal((await hb.messageAOS("Get")).outbox["1"].data, "3")
  })
})
