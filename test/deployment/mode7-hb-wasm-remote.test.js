import assert from "assert"
import { describe, it, before } from "node:test"
import AO from "../../src/ao.js"
import {
  loadWallet,
  counterSrc,
  selfReceiveSrc,
  tokenSrc,
  receiverSrc,
  chainSenderSrc,
  chainResponderSrc,
  REMOTE_NODES,
} from "./shared.js"

describe("Mode 7: Remote HB — wasm-64 (AOS)", function () {
  let ao, jwk, canSpawn = false

  before(async () => {
    jwk = loadWallet()
    if (!jwk) {
      console.log("  [SKIP] No wallet found for Mode 7")
      return
    }
    ao = await new AO({ hb: REMOTE_NODES[0], mode: "aos" }).init(jwk)
    // Test if remote node supports wasm-64 spawning (requires wao@1.0)
    try {
      const { pid, err } = await ao.spwn({})
      if (!err && pid) {
        canSpawn = true
        console.log(`  Remote wasm-64 spawn supported: pid=${pid.slice(0, 15)}...`)
      } else {
        console.log(`  [SKIP] Remote node does not support wasm-64: ${err}`)
      }
    } catch (e) {
      console.log(`  [SKIP] Remote wasm-64 spawn failed: ${e.message?.slice(0, 80)}`)
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

  it("self-receive: schedule SelfReceive", async () => {
    if (!canSpawn) return
    const { pid, err } = await ao.deploy({ boot: true, src_data: selfReceiveSrc })
    assert.ok(!err, `deploy failed: ${err}`)
    const { mid } = await ao.msg({ pid, act: "SelfReceive", data: "1984" })
    assert.ok(mid != null, "SelfReceive slot assigned")
    console.log(`  pid: ${pid}, SelfReceive slot=${mid}`)
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

  it("cross-process receive: schedule PingChain", async () => {
    if (!canSpawn) return
    const { pid: senderPid, err: e1 } = await ao.deploy({
      boot: true, src_data: chainSenderSrc,
    })
    assert.ok(!e1, `sender deploy failed: ${e1}`)
    const { pid: responderPid, err: e2 } = await ao.deploy({
      boot: true, src_data: chainResponderSrc,
    })
    assert.ok(!e2, `responder deploy failed: ${e2}`)
    const { mid } = await ao.msg({
      pid: senderPid, act: "PingChain", data: "1984",
      tags: { Recipient: responderPid },
    })
    assert.ok(mid != null, "PingChain slot assigned")
    console.log(`  sender: ${senderPid}, responder: ${responderPid}`)
  })
})
