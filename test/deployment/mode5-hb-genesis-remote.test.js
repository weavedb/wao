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

describe("Mode 5: Remote HB — genesis-wasm", function () {
  let ao, jwk

  before(async () => {
    jwk = loadWallet()
    if (!jwk) {
      console.log("  [SKIP] No wallet found for Mode 5")
      return
    }
    ao = await new AO({ hb: REMOTE_NODES[0] }).init(jwk)
  })

  it("spawn + eval", async () => {
    if (!ao) return
    const { pid, err } = await ao.spwn({})
    assert.ok(!err, `spawn failed: ${err}`)
    assert.ok(pid, "no pid")
    console.log(`  pid: ${pid}`)

    // Schedule eval — push-only node accepts messages, compute is async
    const { mid, err: msgErr } = await ao.msg({ pid, data: counterSrc, act: "Eval" })
    assert.ok(!msgErr, `eval schedule failed: ${msgErr}`)
    assert.ok(mid != null, "slot assigned")
    console.log(`  Eval scheduled at slot ${mid}`)
  })

  it("counter: schedule Inc + Get", async () => {
    if (!ao) return
    const { p, pid, err } = await ao.deploy({ src_data: counterSrc })
    assert.ok(!err, `deploy failed: ${err}`)
    console.log(`  pid: ${pid}`)

    // Schedule Inc and Get — verify slots assigned
    const { mid: m1 } = await ao.msg({ pid, act: "Inc", data: "1984" })
    assert.ok(m1 != null, "Inc slot assigned")
    const { mid: m2 } = await ao.msg({ pid, act: "Get", data: "1984" })
    assert.ok(m2 != null, "Get slot assigned")
    console.log(`  Inc slot=${m1}, Get slot=${m2} (compute async on network)`)
  })

  it("self-receive: schedule SelfReceive", async () => {
    if (!ao) return
    const { p, pid, err } = await ao.deploy({ boot: true, src_data: selfReceiveSrc })
    assert.ok(!err, `deploy failed: ${err}`)

    const { mid } = await ao.msg({ pid, act: "SelfReceive", data: "1984" })
    assert.ok(mid != null, "SelfReceive slot assigned")
    console.log(`  pid: ${pid}, SelfReceive slot=${mid} (coroutine resolves async)`)
  })

  it("token: schedule Mint + Balance", async () => {
    if (!ao) return
    const { p, pid, err } = await ao.deploy({ src_data: tokenSrc })
    assert.ok(!err, `deploy failed: ${err}`)

    const { mid: m1 } = await ao.msg({ pid, act: "Mint", data: "1984", tags: { Quantity: "100" } })
    assert.ok(m1 != null, "Mint slot assigned")
    const { mid: m2 } = await ao.msg({ pid, act: "Balance", data: "1984" })
    assert.ok(m2 != null, "Balance slot assigned")
    console.log(`  pid: ${pid}, Mint slot=${m1}, Balance slot=${m2}`)
  })

  it("token transfer: schedule Mint + Transfer + Balance", async () => {
    if (!ao) return
    const { p, pid, err } = await ao.deploy({ src_data: tokenSrc })
    assert.ok(!err, `deploy failed: ${err}`)

    await ao.msg({ pid, act: "Mint", data: "1984", tags: { Quantity: "100" } })
    const { mid } = await ao.msg({
      pid, act: "Transfer", data: "1984",
      tags: { Recipient: "some-address", Quantity: "30" },
    })
    assert.ok(mid != null, "Transfer slot assigned")
    const { mid: m2 } = await ao.msg({ pid, act: "Balance", data: "1984" })
    assert.ok(m2 != null, "Balance slot assigned")
    console.log(`  pid: ${pid}, Transfer slot=${mid}, Balance slot=${m2}`)
  })

  it("cross-process: schedule Transfer to receiver", async () => {
    if (!ao) return
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
    console.log(`  token: ${tokenPid}, receiver: ${receiverPid}, Transfer slot=${mid}`)
  })

  it("cross-process receive: schedule PingChain", async () => {
    if (!ao) return
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
    console.log(`  sender: ${senderPid}, responder: ${responderPid}, PingChain slot=${mid}`)
  })

  it("spawn on multiple remote nodes", async () => {
    if (!ao) return
    const results = []
    for (const url of REMOTE_NODES.slice(0, 3)) {
      try {
        const remoteAO = await new AO({ hb: url }).init(jwk)
        const { pid, err } = await remoteAO.spwn({})
        if (err) {
          results.push({ url, err })
        } else {
          results.push({ url, pid })
          console.log(`  ${url}: pid=${pid?.slice(0, 15)}...`)
        }
      } catch (e) {
        results.push({ url, err: e.message?.slice(0, 60) })
        console.log(`  ${url}: ${e.message?.slice(0, 60)}`)
      }
    }
    const succeeded = results.filter(r => r.pid)
    console.log(`  ${succeeded.length}/${results.length} nodes responded`)
    assert.ok(succeeded.length >= 1, "At least 1 remote node should accept spawn")
  })
})
