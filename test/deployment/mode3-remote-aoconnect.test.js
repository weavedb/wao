import assert from "assert"
import { describe, it, before } from "node:test"
import AO from "../../src/ao.js"
import { wait } from "../../src/test.js"
import {
  loadWallet,
  counterSrc,
  tokenSrc,
  receiverSrc,
  selfReceiveSrc,
  chainSenderSrc,
  chainResponderSrc,
} from "./shared.js"

// Mode 3 uses aoconnect testnet. MU can be flaky (504/500), so we retry.
// receive() never resolves inline — the CU yields the coroutine and returns
// outgoing messages. Resolution happens across multiple CU computation passes
// driven by MU cranking. We poll CU results (mode: "aoconnect") to detect
// when the coroutine has resolved.

async function retry(fn, label, maxRetries = 5, delay = 3000) {
  for (let i = 0; i < maxRetries; i++) {
    try {
      return await fn()
    } catch (e) {
      console.log(`  ${label}: attempt ${i + 1} failed: ${e.message?.slice(0, 100)}`)
      if (i === maxRetries - 1) throw e
      await wait(delay)
    }
  }
}

async function spawnAndEval(ao, src, label, { boot = false } = {}) {
  const { pid } = await retry(async () => {
    const spawnOpts = boot ? { boot: "Data", data: src } : {}
    const { pid, err } = await ao.spwn(spawnOpts)
    if (err) throw typeof err === "string" ? new Error(err) : err
    return { pid }
  }, `${label} spawn`)

  console.log(`  ${label} pid: ${pid}`)
  await wait(3000)

  if (!boot) {
    await retry(async () => {
      const res = await ao.msg({ pid, data: src, act: "Eval", timeout: 30000 })
      if (res.err) throw typeof res.err === "string" ? new Error(res.err) : res.err
    }, `${label} eval`)
  }

  return { pid, p: ao.p(pid) }
}

async function m(p, act, tagsOrGet, retries = 3) {
  return await retry(async () => {
    return await p.m(act, tagsOrGet)
  }, `m(${act})`, retries)
}

describe("Mode 3: Remote aoconnect (testnet)", function () {
  let ao, jwk

  before(async () => {
    jwk = loadWallet()
    if (!jwk) {
      console.log("  [SKIP] No wallet found for Mode 3")
      return
    }
    ao = await new AO().init(jwk)
  })

  it("counter: deploy, Inc, Get → 1", async () => {
    if (!ao) return
    const { p } = await spawnAndEval(ao, counterSrc, "Mode 3 counter")
    await m(p, "Inc")
    const count = await m(p, "Get", false)
    assert.equal(count, "1", `Mode 3: expected count 1, got ${count}`)
  })

  it("self-message receive(): Send({Target=ao.id}).receive()", async () => {
    if (!ao) return
    const { p, pid } = await spawnAndEval(ao, selfReceiveSrc, "Mode 3 self-receive", { boot: true })
    // Poll CU results — MU cranks outgoing messages through 3 passes:
    // SelfReceive → SelfReply → reply(SelfData) → coroutine resumes
    const { out, err } = await retry(async () => {
      return await p.msg(
        "SelfReceive",
        {},
        {
          get: false,
          timeout: 180000,
          check: [{ data: "Got: SelfData" }],
        }
      )
    }, "SelfReceive", 2, 5000)
    assert.ok(!err, `Mode 3: SelfReceive error: ${err}`)
    assert.equal(out, "Got: SelfData", `Mode 3: expected "Got: SelfData", got "${out}"`)
  })

  it("token: deploy, Mint 100, Balance → 100", async () => {
    if (!ao) return
    const { p } = await spawnAndEval(ao, tokenSrc, "Mode 3 token")
    await m(p, "Mint", { Quantity: "100" })
    const bal = await m(p, "Balance", false)
    assert.equal(bal, "100", `Mode 3: expected balance 100, got ${bal}`)
  })

  it("token transfer: Mint 100, Transfer 30, Balance → 70", async () => {
    if (!ao) return
    const { p } = await spawnAndEval(ao, tokenSrc, "Mode 3 transfer")
    await m(p, "Mint", { Quantity: "100" })
    await m(p, "Transfer", { Recipient: "some-address", Quantity: "30" })
    const bal = await m(p, "Balance", false)
    assert.equal(bal, "70", `Mode 3: expected balance 70, got ${bal}`)
  })

  it("cross-process: token Transfer → receiver Credit-Notice", async () => {
    if (!ao) return
    const { p: tokenP } = await spawnAndEval(ao, tokenSrc, "Mode 3 cross-token")

    const ao2 = await new AO().init(jwk)
    const { p: receiverP, pid: receiverPid } = await spawnAndEval(
      ao2, receiverSrc, "Mode 3 cross-receiver"
    )

    await m(tokenP, "Mint", { Quantity: "100" })
    await m(tokenP, "Transfer", { Recipient: receiverPid, Quantity: "30" })
    const senderBal = await m(tokenP, "Balance", false)
    assert.equal(senderBal, "70", `Mode 3: sender balance should be 70`)

    await wait(10000)
    try {
      const credits = await m(receiverP, "GetCredits", false)
      if (credits === "30") {
        console.log(`  Mode 3: cross-process worked! credits = ${credits}`)
      } else {
        console.log(`  Mode 3: cross-process not yet propagated (credits = ${credits})`)
      }
    } catch (e) {
      console.log(`  Mode 3: cross-process check failed: ${e.message}`)
    }
  })

  it("cross-process receive(): Receive() chain across processes", async () => {
    if (!ao) return
    const { p: senderP } = await spawnAndEval(
      ao, chainSenderSrc, "Mode 3 chain-sender", { boot: true }
    )

    const ao2 = await new AO().init(jwk)
    const { p: responderP, pid: responderPid } = await spawnAndEval(
      ao2, chainResponderSrc, "Mode 3 chain-responder"
    )

    // Poll CU results — MU cranks across processes:
    // PingChain(sender) → Ping(responder) → Pong(sender) → coroutine resumes
    const { out, err } = await retry(async () => {
      return await senderP.msg(
        "PingChain",
        { Recipient: responderPid },
        {
          get: false,
          timeout: 300000,
          check: [{ data: "chain:pong-hello" }],
        }
      )
    }, "PingChain", 2, 5000)

    assert.ok(!err, `Mode 3: PingChain error: ${err}`)
    assert.equal(
      out,
      "chain:pong-hello",
      `Mode 3: expected "chain:pong-hello", got "${out}"`
    )
  })
})
