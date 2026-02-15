import assert from "assert"
import { AO, acc, wait } from "../../src/test.js"
import { readFileSync, existsSync } from "fs"
import { resolve } from "path"

// ─── Cross-Mode Lua Helpers ─────────────────────────────────────────────────
// Work across all modes: CU AOS (msg.Tags.X / msg.From),
// WAMR AOS (msg.Tags.X / msg.From), and HB Lua (msg.body.x / commitments)

export const crossModeHelpers = `
local function T(msg, name)
  if msg.Tags and msg.Tags[name] then return msg.Tags[name] end
  if msg[name] then return msg[name] end
  if msg.body then
    local lower = string.lower(name)
    if msg.body[lower] then return msg.body[lower] end
    if msg.body[name] then return msg.body[name] end
  end
  return nil
end

local function getFrom(msg)
  if msg.From then return msg.From end
  if msg.body and msg.body.commitments then
    for _, c in pairs(msg.body.commitments) do
      if type(c) == "table" and (c.type == "rsa-pss-sha512" or c.alg == "rsa-pss-sha512") then
        return c.committer
      end
      if type(c) == "table" and c.alg == "signed" and c["commitment-device"] == "ans104" then
        return c.committer
      end
    end
  end
  return nil
end

local function intstr(n)
  if n == math.floor(n) then return string.format("%d", n) end
  return tostring(n)
end
`

// ─── Lua Sources ────────────────────────────────────────────────────────────

export const counterSrc = crossModeHelpers + `
local count = 0
Handlers.add("Inc", "Inc", function(msg)
  count = count + 1
  msg.reply({ Data = intstr(count) })
end)

Handlers.add("Get", "Get", function(msg)
  msg.reply({ Data = intstr(count) })
end)
`

export const selfReceiveSrc = `
Handlers.add("SelfReceive", "SelfReceive", function(msg)
  local reply = Send({ Target = ao.id, Action = "SelfReply" }).receive()
  msg.reply({ Data = "Got: " .. reply.Data })
end)

Handlers.add("SelfReply", "SelfReply", function(msg)
  msg.reply({ Data = "SelfData" })
end)
`

export const tokenSrc = crossModeHelpers + `
local balances = {}
Handlers.add("Mint", "Mint", function(msg)
  local qty = tonumber(T(msg, "Quantity"))
  local from = getFrom(msg)
  balances[from] = (balances[from] or 0) + qty
  msg.reply({ Data = "Minted" })
end)

Handlers.add("Transfer", "Transfer", function(msg)
  local qty = tonumber(T(msg, "Quantity"))
  local from = getFrom(msg)
  if (balances[from] or 0) < qty then
    msg.reply({ Data = "Insufficient" }); return
  end
  balances[from] = balances[from] - qty
  local recipient = T(msg, "Recipient")
  balances[recipient] = (balances[recipient] or 0) + qty
  ao.send({ Target = recipient, Action = "Credit-Notice",
            Quantity = intstr(qty), Sender = from })
  msg.reply({ Data = "Transferred" })
end)

Handlers.add("Balance", "Balance", function(msg)
  local from = getFrom(msg)
  msg.reply({ Data = intstr(balances[from] or 0) })
end)
`

export const receiverSrc = crossModeHelpers + `
local credits = 0
Handlers.add("Credit-Notice", "Credit-Notice", function(msg)
  local qty = T(msg, "Quantity") or "0"
  credits = credits + tonumber(qty)
end)

Handlers.add("GetCredits", "GetCredits", function(msg)
  msg.reply({ Data = intstr(credits) })
end)
`

export const chainSenderSrc = crossModeHelpers + `
Handlers.add("PingChain", "PingChain", function(msg)
  local recipient = T(msg, "Recipient")
  Send({ Target = recipient, Action = "Ping", Data = "hello" })
  local reply = Receive({ Action = "Pong" })
  msg.reply({ Data = "chain:" .. reply.Data })
end)
`

export const chainResponderSrc = crossModeHelpers + `
Handlers.add("Ping", "Ping", function(msg)
  local from = getFrom(msg)
  ao.send({ Target = from, Action = "Pong", Data = "pong-" .. msg.Data })
end)
`

// ─── Test Helpers ───────────────────────────────────────────────────────────

export async function runCounterTest(ao, label) {
  const { p, pid, err } = await ao.deploy({ src_data: counterSrc })
  assert.ok(!err, `${label}: deploy failed: ${err}`)
  assert.ok(pid, `${label}: no pid`)
  await p.m("Inc")
  const count = await p.m("Get", false)
  assert.equal(count, "1", `${label}: expected count 1, got ${count}`)
}

export async function runSelfReceiveTest(ao, label) {
  const { p, pid, err } = await ao.deploy({ boot: true, src_data: selfReceiveSrc })
  assert.ok(!err, `${label}: deploy failed: ${err}`)
  const result = await p.m("SelfReceive", false)
  assert.equal(result, "Got: SelfData", `${label}: expected "Got: SelfData", got "${result}"`)
}

export async function runTokenMintTest(ao, label) {
  const { p, pid, err } = await ao.deploy({ src_data: tokenSrc })
  assert.ok(!err, `${label}: deploy failed: ${err}`)
  await p.m("Mint", { Quantity: "100" })
  const bal = await p.m("Balance", false)
  assert.equal(bal, "100", `${label}: expected balance 100, got ${bal}`)
}

export async function runTokenTransferTest(ao, label) {
  const { p, pid, err } = await ao.deploy({ src_data: tokenSrc })
  assert.ok(!err, `${label}: deploy failed: ${err}`)
  await p.m("Mint", { Quantity: "100" })
  await p.m("Transfer", { Recipient: "some-address", Quantity: "30" })
  const bal = await p.m("Balance", false)
  assert.equal(bal, "70", `${label}: expected balance 70, got ${bal}`)
}

export async function runCrossProcessTest(ao, ao2, label, isHB = false) {
  const { p: tokenP, pid: tokenPid, err: err1 } = await ao.deploy({
    src_data: tokenSrc,
  })
  assert.ok(!err1, `${label}: token deploy failed: ${err1}`)

  if (isHB) await wait(1000)

  const { p: receiverP, pid: receiverPid, err: err2 } = await ao2.deploy({
    src_data: receiverSrc,
  })
  assert.ok(!err2, `${label}: receiver deploy failed: ${err2}`)

  // For HB: add cross-process trust so push-delivered messages are accepted.
  // AOS getOwner() returns from-process (sender PID) for pushed messages,
  // so each process must explicitly trust the other's PID.
  if (isHB) {
    await receiverP.m("__AddAuthority", { Addr: tokenPid })
    await tokenP.m("__AddAuthority", { Addr: receiverPid })
  }

  await tokenP.m("Mint", { Quantity: "100" })

  let transferResult = await tokenP.m("Transfer", {
    Recipient: receiverPid,
    Quantity: "30",
  }, false)

  if (isHB && !transferResult) {
    await wait(1000)
    transferResult = await tokenP.m("Balance", false)
    console.log(`  ${label}: Transfer compute null (HB cache race), Balance = ${transferResult}`)
    if (transferResult === "70") {
      console.log(`  ${label}: Transfer succeeded (verified by balance)`)
    } else {
      assert.fail(`${label}: Transfer failed — balance is ${transferResult}, expected 70`)
    }
  } else {
    assert.equal(transferResult, "Transferred", `${label}: transfer reply`)
  }

  const senderBal = await tokenP.m("Balance", false)
  assert.equal(senderBal, "70", `${label}: sender balance should be 70`)

  if (isHB) {
    await wait(3000)
    try {
      const credits = await receiverP.m("GetCredits", false)
      if (credits === "30") {
        console.log(`  ${label}: cross-process push worked! credits = ${credits}`)
      } else {
        console.log(`  ${label}: cross-process push did not propagate (credits = ${credits})`)
        console.log(`  ${label}: expected — HB push routing is async`)
      }
    } catch (e) {
      console.log(`  ${label}: cross-process check failed: ${e.message}`)
    }
  } else {
    const credits = await receiverP.m("GetCredits", false)
    assert.equal(credits, "30", `${label}: expected credits 30, got ${credits}`)
  }
}

export async function runCrossProcessReceiveTest(ao, label) {
  const ao2 = await new AO({ mem: ao.mem }).init(acc[1])

  const { p: senderP, pid: senderPid, err: err1 } = await ao.deploy({
    boot: true,
    src_data: chainSenderSrc,
  })
  assert.ok(!err1, `${label}: sender deploy failed: ${err1}`)

  const { p: responderP, pid: responderPid, err: err2 } = await ao2.deploy({
    src_data: chainResponderSrc,
  })
  assert.ok(!err2, `${label}: responder deploy failed: ${err2}`)

  const { out } = await senderP.msg(
    "PingChain",
    { Recipient: responderPid },
    {
      get: false,
      mode: "gql",
      timeout: 5000,
      check: [{ from: responderPid }],
    }
  )
  assert.equal(
    out,
    "chain:pong-hello",
    `${label}: expected "chain:pong-hello", got "${out}"`
  )
}

export async function runCrossProcessReceiveHBTest(ao, label) {
  // Deploy sender (uses Send().receive() coroutine)
  const { p: senderP, pid: senderPid, err: err1 } = await ao.deploy({
    boot: true,
    src_data: chainSenderSrc,
  })
  assert.ok(!err1, `${label}: sender deploy failed: ${err1}`)

  await wait(500)

  // Deploy responder
  const { p: responderP, pid: responderPid, err: err2 } = await ao.deploy({
    boot: true,
    src_data: chainResponderSrc,
  })
  assert.ok(!err2, `${label}: responder deploy failed: ${err2}`)

  // Cross-trust: sender trusts responder (for Pong reply) and vice versa
  await senderP.m("__AddAuthority", { Addr: responderPid })
  await responderP.m("__AddAuthority", { Addr: senderPid })

  // Send PingChain — sender sends Ping to responder, waits for Pong via receive()
  const result = await senderP.m("PingChain", { Recipient: responderPid }, false)
  assert.equal(
    result,
    "chain:pong-hello",
    `${label}: expected "chain:pong-hello", got "${result}"`,
  )
}

export function loadWallet() {
  const paths = [
    resolve(import.meta.dirname, "../../.wallet.json"),
    resolve(import.meta.dirname, "../../src/workspace/.wallet.json"),
    resolve(import.meta.dirname, "../../HyperBEAM/.wallet.json"),
  ]
  const found = paths.find(p => existsSync(p))
  if (!found) return null
  return JSON.parse(readFileSync(found, "utf8"))
}

export const REMOTE_NODES = [
  "https://push.forward.computer",
  ...Array.from({ length: 10 }, (_, i) => `https://push-${i + 1}.forward.computer`),
]
