import assert from "assert"
import { after, describe, it, before, beforeEach } from "node:test"
import { wait } from "../src/utils.js"
import { getJWK } from "./test-utils.js"
import HB from "../src/hb.js"
import HyperBEAM from "../src/hyperbeam.js"
import { resolve } from "path"
import { readFileSync } from "fs"
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
  let hb, hbeam, hb2
  before(async () => {
    hbeam = new HyperBEAM({ c: "12", cmake: "3.5", gateway: 4000 })
    await wait(5000)
    const jwk = getJWK("../HyperBEAM/.wallet.json")
    const jwk2 = getJWK("../HyperBEAM/hyperbeam-key.json")
    hb = new HB({ jwk })
    hb2 = new HB({ jwk: jwk2 })
  })
  after(async () => {
    hbeam.kill()
  })
  it("should handle counter with Add and Get handlers", async () => {
    const pid = await hb.spawnAOS()
    await hb.messageAOS("Eval", {}, src_data)
    await hb.messageAOS("Add", { Plus: "3" })
    assert.equal((await hb.messageAOS("Get")).outbox["1"].data, "3")
  })

  it("should cache", async () => {
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
  it.only("should query meta device", async () => {
    await hb.meta.info({ method: "post", abc: "def" })
    const info = await hb2.meta.info()
    assert.equal(info.abc, "def")
    const build = await hb2.meta.build()
    assert.equal(build.node, "HyperBEAM")
  })
})
