import { describe, it, before, after } from "node:test"
import assert from "node:assert"
import { readFileSync } from "node:fs"
import { resolve } from "node:path"
import { createData, ArweaveSigner } from "arbundles"
import { HyperBEAM } from "../../src/test.js"
import HB from "../../src/hb.js"
import { loadWallet, REMOTE_NODES } from "./shared.js"

const luaPath = resolve(import.meta.dirname, "../../custom-lua/counter.lua")

// Upload Lua source to Arweave via Turbo (free for < 100KB)
async function uploadToArweave(data, jwk) {
  const signer = new ArweaveSigner(jwk)
  const dataItem = createData(Buffer.from(data), signer, {
    tags: [{ name: "Content-Type", value: "application/lua" }],
  })
  await dataItem.sign(signer)
  const res = await fetch("https://upload.ardrive.io/v1/tx", {
    method: "POST",
    headers: { "content-type": "application/octet-stream" },
    body: dataItem.getRaw(),
  })
  if (res.status !== 200) throw new Error(`Turbo upload failed: ${res.status}`)
  const json = await res.json()
  return json.id
}

// Spawn a process with custom Lua module (bypassing hyper-aos boot)
async function spawnCustomLua(hb, moduleId) {
  return hb.spawn({
    "data-protocol": "ao",
    variant: "ao.TN.1",
    module: moduleId,
    "execution-device": "lua@5.3a",
    "push-device": "push@1.0",
    "patch-from": "/results/outbox",
  })
}

describe("Mode 11: Custom Lua on Local HB", function () {
  let hbeam, hb, moduleId

  before(async () => {
    hbeam = await new HyperBEAM({ reset: true }).ready()
    hb = new HB({ url: hbeam.url })
    await hb.init(hbeam.jwk)

    // Cache custom Lua module via cacheScript (scheduler-based, avoids wao@1.0)
    const luaSrc = readFileSync(luaPath, "utf-8")
    moduleId = await hb.cacheScript(luaSrc, "application/lua")
    console.log(`  Cached Lua module: ${moduleId}`)
  })

  after(async () => {
    if (hbeam) hbeam.kill()
  })

  it("cache + spawn custom Lua", async () => {
    assert.ok(moduleId, "should get module ID from cache")
    const { pid, slot } = await spawnCustomLua(hb, moduleId)
    console.log(`  pid: ${pid}, spawn slot: ${slot}`)
    assert.ok(pid, "should get process ID")
  })

  it("eval: send Eval action", async () => {
    const { pid } = await spawnCustomLua(hb, moduleId)
    console.log(`  pid: ${pid}`)

    const { slot } = await hb.scheduleLua({
      pid,
      action: "Eval",
      data: "hello from custom lua",
    })
    console.log(`  Eval scheduled at slot ${slot}`)

    const result = await hb.computeLua({ pid, slot })
    console.log(`  Result:`, JSON.stringify(result).slice(0, 200))
    assert.ok(result, "should get compute result")
  })

  it("counter: Inc + Get", async () => {
    const { pid } = await spawnCustomLua(hb, moduleId)
    console.log(`  pid: ${pid}`)

    const inc1 = await hb.scheduleLua({ pid, action: "Inc" })
    console.log(`  Inc scheduled at slot ${inc1.slot}`)
    const incResult = await hb.computeLua({ pid, slot: inc1.slot })
    console.log(`  Inc result:`, JSON.stringify(incResult).slice(0, 200))

    const get1 = await hb.scheduleLua({ pid, action: "Get" })
    console.log(`  Get scheduled at slot ${get1.slot}`)
    const getResult = await hb.computeLua({ pid, slot: get1.slot })
    console.log(`  Get result:`, JSON.stringify(getResult).slice(0, 200))

    const resultStr = JSON.stringify(getResult)
    assert.ok(
      resultStr.includes('"1"') || resultStr.includes('"data":"1"'),
      `counter should be 1, got: ${resultStr.slice(0, 300)}`,
    )
  })

  it("counter: multiple Inc → correct count", async () => {
    const { pid } = await spawnCustomLua(hb, moduleId)
    console.log(`  pid: ${pid}`)

    for (let i = 1; i <= 3; i++) {
      const { slot } = await hb.scheduleLua({ pid, action: "Inc" })
      await hb.computeLua({ pid, slot })
      console.log(`  Inc #${i} at slot ${slot}`)
    }

    const { slot } = await hb.scheduleLua({ pid, action: "Get" })
    const result = await hb.computeLua({ pid, slot })
    console.log(`  Get result:`, JSON.stringify(result).slice(0, 200))

    const resultStr = JSON.stringify(result)
    assert.ok(
      resultStr.includes('"3"') || resultStr.includes('"data":"3"'),
      `counter should be 3, got: ${resultStr.slice(0, 300)}`,
    )
  })
})

describe("Mode 11R: Custom Lua on Remote HB", function () {
  const REMOTE = REMOTE_NODES[1]
  let hb, jwk, arweaveModuleId

  before(async () => {
    jwk = loadWallet()
    if (!jwk) {
      console.log("  [SKIP] No wallet found for Mode 11R")
      return
    }
    hb = new HB({ url: REMOTE, jwk })
    await hb.init(jwk)

    // Upload Lua to Arweave via Turbo
    const luaSrc = readFileSync(luaPath, "utf-8")
    console.log(`  Uploading ${luaSrc.length} bytes to Arweave via Turbo...`)
    arweaveModuleId = await uploadToArweave(luaSrc, jwk)
    console.log(`  Arweave TX: ${arweaveModuleId}`)
    console.log(`  Remote node: ${REMOTE}`)
  })

  it("spawn custom Lua from Arweave TX", async () => {
    if (!jwk) return
    const { pid, slot } = await spawnCustomLua(hb, arweaveModuleId)
    console.log(`  pid: ${pid}, spawn slot: ${slot}`)
    assert.ok(pid, "should get process ID")
  })

  it("schedule Inc + Get messages", async () => {
    if (!jwk) return
    const { pid } = await spawnCustomLua(hb, arweaveModuleId)
    console.log(`  pid: ${pid}`)

    const inc1 = await hb.scheduleLua({ pid, action: "Inc" })
    console.log(`  Inc scheduled at slot ${inc1.slot}`)
    assert.ok(inc1.slot != null, "Inc slot assigned")

    const get1 = await hb.scheduleLua({ pid, action: "Get" })
    console.log(`  Get scheduled at slot ${get1.slot}`)
    assert.ok(get1.slot != null, "Get slot assigned")
  })

  it("multiple messages scheduled", async () => {
    if (!jwk) return
    const { pid } = await spawnCustomLua(hb, arweaveModuleId)
    console.log(`  pid: ${pid}`)

    const slots = []
    for (let i = 1; i <= 3; i++) {
      const { slot } = await hb.scheduleLua({ pid, action: "Inc" })
      slots.push(slot)
      console.log(`  Inc #${i} at slot ${slot}`)
    }
    assert.strictEqual(slots.length, 3, "3 slots assigned")

    const { slot } = await hb.scheduleLua({ pid, action: "Get" })
    console.log(`  Get at slot ${slot}`)
    assert.ok(slot != null, "Get slot assigned")
  })
})
