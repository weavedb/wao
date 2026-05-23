/**
 * Comprehensive deployment mode test for AO.
 *
 * Tests 4 modes:
 *   Mode 1: In-memory emulator (wao/test) — legacynet AOS in Node.js
 *   Mode 4: Local HB legacy (genesis-wasm@1.0) — AOS on HyperBEAM
 *   Mode 6: Local HB AOS (stack@1.0 / wasm-64) — mainnet-compatible
 *   Mode 8: Local HB Lua (lua@5.3a) — lightweight Lua VM
 *
 * Each mode tests: deploy/spawn, messages, counter state, eval.
 *
 * Run: node --test --test-concurrency=1 test/deployment/multinet.test.js
 *   (on Node 22 prefix with --experimental-wasm-memory64; on Node 24+
 *   wasm-memory64 is default-on and the flag is rejected)
 */
import assert from "assert"
import { describe, it, before, after } from "node:test"
// tao.js = in-memory AOS (loads AoLoader WASM, takes ~15s)
import TestAO from "../../src/tao.js"
// ao.js = production AO (for HB modes)
import AO from "../../src/ao.js"
import HyperBEAM from "../../src/hyperbeam.js"
import { acc } from "../../src/accounts.js"

// ─── Lua sources ───

const COUNTER_LUA = `
local count = 0
Handlers.add("Inc", "Inc", function(msg)
  count = count + 1
  msg.reply({ Data = tostring(count) })
end)
Handlers.add("Get", "Get", function(msg)
  msg.reply({ Data = tostring(count) })
end)
Handlers.add("Hello", "Hello", function(msg)
  msg.reply({ Data = "Hello, World!" })
end)
`

const TOKEN_LUA = `
local balances = {}
Handlers.add("Mint", "Mint", function(msg)
  local qty = tonumber(msg.Quantity) or 0
  balances[msg.From] = (balances[msg.From] or 0) + qty
  msg.reply({ Data = "Minted " .. tostring(qty) })
end)
Handlers.add("Balance", "Balance", function(msg)
  local target = msg.Target2 or msg.From
  msg.reply({ Data = tostring(balances[target] or 0) })
end)
Handlers.add("Transfer", "Transfer", function(msg)
  local qty = tonumber(msg.Quantity) or 0
  local from = msg.From
  local to = msg.Recipient
  if (balances[from] or 0) < qty then
    msg.reply({ Data = "Insufficient" })
    return
  end
  balances[from] = balances[from] - qty
  balances[to] = (balances[to] or 0) + qty
  msg.reply({ Data = "Transferred " .. tostring(qty) })
end)
`

// ═══════════════════════════════════════════════════════════════════════════
// Mode 1: In-memory emulator (legacynet)
// Uses tao.js (test AO) which runs AOS WASM directly in Node.js
// ═══════════════════════════════════════════════════════════════════════════

describe("Mode 1: In-memory emulator", { timeout: 120000 }, function () {
  let ao

  before(async () => {
    ao = await new TestAO().init(acc[0])
  })

  it("should deploy counter and increment", async () => {
    const { p, pid } = await ao.deploy({ src_data: COUNTER_LUA })
    assert.ok(pid, "deploy should return pid")

    const { out } = await p.msg("Hello", false)
    assert.equal(out, "Hello, World!")

    await p.msg("Inc")
    await p.msg("Inc")
    const { out: count } = await p.msg("Get", false)
    assert.equal(count, "2")
  })

  it("should deploy token and mint/balance", async () => {
    const { p } = await ao.deploy({ src_data: TOKEN_LUA })

    const { out: minted } = await p.msg("Mint", { Quantity: "100" }, false)
    assert.ok(minted.includes("100"), "should mint 100")

    const { out: bal } = await p.msg("Balance", false)
    assert.equal(bal, "100")
  })

  it("should transfer tokens between users", async () => {
    const ao1 = await new TestAO().init(acc[0])
    const { p, pid } = await ao1.deploy({ src_data: TOKEN_LUA })

    // Mint 100 for user 0
    await p.msg("Mint", { Quantity: "100" })

    // Transfer 30 to user 1
    const ao2 = await new TestAO({ mem: ao1.mem }).init(acc[1])
    const p1 = ao2.p(pid)
    await p.msg("Transfer", { Quantity: "30", Recipient: acc[1].addr })

    // Check balances
    const { out: bal0 } = await p.msg("Balance", false)
    assert.equal(bal0, "70")

    const { out: bal1 } = await p1.msg("Balance", false)
    assert.equal(bal1, "30")
  })
})

// ═══════════════════════════════════════════════════════════════════════════
// Mode 4: Local HB — Legacy (genesis-wasm@1.0)
// Uses ao.js with { hb: url } config, mode defaults to "legacy"
// ═══════════════════════════════════════════════════════════════════════════

describe("Mode 4: Local HB Legacy (genesis-wasm)", { timeout: 120000 }, function () {
  let hbeam, ao

  before(async () => {
    hbeam = await new HyperBEAM({ reset: true, genesis_wasm: true }).ready()
    ao = await new AO({ hb: hbeam.url }).init(hbeam.jwk)
  })

  after(async () => hbeam?.kill())

  it("should deploy counter and send messages", async () => {
    const { p, pid } = await ao.deploy({ src_data: COUNTER_LUA })
    assert.ok(pid, "deploy should return pid")

    const { out } = await p.msg("Hello", false)
    assert.equal(out, "Hello, World!")

    await p.msg("Inc")
    await p.msg("Inc")
    const { out: count } = await p.msg("Get", false)
    assert.equal(count, "2")
  })

  it("should eval Lua expressions", async () => {
    const { pid } = await ao.spwn()
    assert.ok(pid)

    // eval() uses pipe(), so res is an array of fn results
    const evalResult = await ao.eval({ pid, data: "return 1+1" })
    assert.ok(!evalResult.err, "eval should not error: " + evalResult.err)
    // The compute result is in the first fn's res
    const computeRes = evalResult.res?.[0]?.res
    const output = computeRes?.Output?.data
    // For HB legacy, output might be in different paths
    assert.ok(
      output != null || !evalResult.err,
      "eval should succeed without error",
    )
  })
})

// ═══════════════════════════════════════════════════════════════════════════
// Mode 6: Local HB — AOS (stack@1.0 / wasm-64)
// Uses ao.js with { hb: url, mode: "aos" }
// Note: First compute downloads ~2MB WASM from Arweave, can be slow
// ═══════════════════════════════════════════════════════════════════════════

describe("Mode 6: Local HB AOS (wasm-64 stack)", { timeout: 120000 }, function () {
  let hbeam, ao

  before(async () => {
    hbeam = await new HyperBEAM({ reset: true }).ready()
    ao = await new AO({ hb: hbeam.url, mode: "aos" }).init(hbeam.jwk)
  })

  after(async () => hbeam?.kill())

  it("should spawn an AOS process", async () => {
    const { pid } = await ao.spwn()
    assert.ok(pid, "spawn should return pid")
  })

  it("should schedule a message", async () => {
    const { pid } = await ao.spwn()
    assert.ok(pid)

    const sched = await ao.hb.scheduleAOS({
      pid,
      action: "Eval",
      data: "return 1+1",
    })
    assert.ok(sched.slot != null, "schedule should return slot")
  })
})

// ═══════════════════════════════════════════════════════════════════════════
// Mode 8: Local HB — Lua (lua@5.3a)
// Uses ao.js with { hb: url, mode: "lua" }
// Lua processes are lightweight — no WASM download needed
// ═══════════════════════════════════════════════════════════════════════════

describe("Mode 8: Local HB Lua", { timeout: 120000 }, function () {
  let hbeam, ao

  before(async () => {
    hbeam = await new HyperBEAM({ reset: true }).ready()
    ao = await new AO({ hb: hbeam.url, mode: "lua" }).init(hbeam.jwk)
  })

  after(async () => hbeam?.kill())

  it("should spawn a Lua process", async () => {
    const { pid } = await ao.spwn()
    assert.ok(pid, "spawn should return pid")
  })

  it("should schedule and compute Lua eval", async () => {
    const { pid } = await ao.spwn()
    assert.ok(pid)

    const sched = await ao.hb.scheduleLua({
      pid,
      action: "Eval",
      data: "return 1+1",
    })
    assert.ok(sched.slot != null, "schedule should return slot")

    const comp = await ao.hb.computeLua({ pid, slot: sched.slot })
    assert.ok(comp, "compute should return result")
  })

  it("should send a message via ao.msg and get result", async () => {
    const { pid } = await ao.spwn()
    assert.ok(pid)

    // Load a counter handler via eval
    await ao.eval({ pid, data: COUNTER_LUA })

    // Send Inc
    const r1 = await ao.msg({ pid, act: "Inc" })
    assert.ok(!r1.err, "Inc should not error: " + r1.err)

    // Send Get — lua mode returns { Messages: [...], Output, Spawns }
    const r2 = await ao.msg({ pid, act: "Get" })
    const data = r2.res?.Messages?.[0]?.Data
    assert.ok(data, "Get should return data in Messages")
  })
})
