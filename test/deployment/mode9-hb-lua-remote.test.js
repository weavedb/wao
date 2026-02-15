import assert from "assert"
import { describe, it, before } from "node:test"
import AO from "../../src/ao.js"
import {
  loadWallet,
  counterSrc,
  tokenSrc,
  receiverSrc,
  REMOTE_NODES,
} from "./shared.js"

describe("Mode 9: Remote HB — Lua", function () {
  let ao, jwk, canSpawn = false

  before(async () => {
    jwk = loadWallet()
    if (!jwk) {
      console.log("  [SKIP] No wallet found for Mode 9")
      return
    }
    ao = await new AO({ hb: REMOTE_NODES[0], mode: "lua" }).init(jwk)
    // Test if remote node supports Lua spawning (requires wao@1.0)
    try {
      const { pid, err } = await ao.spwn({})
      if (!err && pid) {
        canSpawn = true
        console.log(`  Remote Lua spawn supported: pid=${pid.slice(0, 15)}...`)
      } else {
        console.log(`  [SKIP] Remote node does not support Lua: ${err}`)
      }
    } catch (e) {
      console.log(`  [SKIP] Remote Lua spawn failed: ${e.message?.slice(0, 80)}`)
    }
  })

  it("spawn + eval", async () => {
    if (!canSpawn) return
    const { pid, err } = await ao.spwn({})
    assert.ok(!err, `spawn failed: ${err}`)
    const { mid } = await ao.msg({ pid, data: counterSrc, act: "Eval" })
    assert.ok(mid != null, "Eval slot assigned")
    console.log(`  pid: ${pid}, Eval slot=${mid}`)
  })

  it("counter: schedule Inc + Get", async () => {
    if (!canSpawn) return
    const { pid, err } = await ao.deploy({ src_data: counterSrc })
    assert.ok(!err, `deploy failed: ${err}`)
    const { mid: m1 } = await ao.msg({ pid, act: "Inc", data: "1984" })
    const { mid: m2 } = await ao.msg({ pid, act: "Get", data: "1984" })
    assert.ok(m1 != null && m2 != null, "slots assigned")
    console.log(`  pid: ${pid}, Inc=${m1}, Get=${m2}`)
  })

  it("token: schedule Mint + Balance", async () => {
    if (!canSpawn) return
    const { pid, err } = await ao.deploy({ src_data: tokenSrc })
    assert.ok(!err, `deploy failed: ${err}`)
    const { mid: m1 } = await ao.msg({ pid, act: "Mint", data: "1984", tags: { Quantity: "100" } })
    const { mid: m2 } = await ao.msg({ pid, act: "Balance", data: "1984" })
    assert.ok(m1 != null && m2 != null, "slots assigned")
    console.log(`  pid: ${pid}, Mint=${m1}, Balance=${m2}`)
  })

  it("token transfer: schedule Mint + Transfer + Balance", async () => {
    if (!canSpawn) return
    const { pid, err } = await ao.deploy({ src_data: tokenSrc })
    assert.ok(!err, `deploy failed: ${err}`)
    await ao.msg({ pid, act: "Mint", data: "1984", tags: { Quantity: "100" } })
    const { mid } = await ao.msg({
      pid, act: "Transfer", data: "1984",
      tags: { Recipient: "some-address", Quantity: "30" },
    })
    assert.ok(mid != null, "Transfer slot assigned")
    console.log(`  pid: ${pid}, Transfer slot=${mid}`)
  })

  it("cross-process: schedule Transfer to receiver", async () => {
    if (!canSpawn) return
    const { pid: tokenPid, err: e1 } = await ao.deploy({ src_data: tokenSrc })
    assert.ok(!e1, `token deploy failed: ${e1}`)
    const { pid: receiverPid, err: e2 } = await ao.deploy({ src_data: receiverSrc })
    assert.ok(!e2, `receiver deploy failed: ${e2}`)
    await ao.msg({ pid: tokenPid, act: "Mint", data: "1984", tags: { Quantity: "100" } })
    const { mid } = await ao.msg({
      pid: tokenPid, act: "Transfer", data: "1984",
      tags: { Recipient: receiverPid, Quantity: "30" },
    })
    assert.ok(mid != null, "Transfer slot assigned")
    console.log(`  token: ${tokenPid}, receiver: ${receiverPid}`)
  })

  // Receive() not implemented in HB Lua — self-receive and cross-process
  // receive tests are N/A for both local and remote Lua modes.
  it("note: Receive() not implemented in HB Lua", () => {
    console.log("  Receive() returns 'not implemented' in HB Lua runtime")
  })
})
