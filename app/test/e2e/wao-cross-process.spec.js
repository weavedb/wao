// @ts-check
import { test, expect } from "@playwright/test"

test.describe("WAO Studio — cross-process messaging", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("/")
    await page.waitForLoadState("networkidle", { timeout: 60000 })
    await page.waitForFunction(
      () => Boolean(globalThis.g?.ao?.mem),
      null,
      { timeout: 60000 },
    )
  })

  test("token Transfer fires Credit-Notice on recipient", async ({ page }) => {
    const result = await page.evaluate(async () => {
      const ao = globalThis.g.ao
      const tokenSrc = `
local balances = {}
Handlers.add("Mint", "Mint", function(msg)
  local qty = tonumber(msg.Tags.Quantity or "0")
  balances[msg.From or "self"] = (balances[msg.From or "self"] or 0) + qty
  msg.reply({ Data = "Minted" })
end)
Handlers.add("Transfer", "Transfer", function(msg)
  local qty = tonumber(msg.Tags.Quantity or "0")
  local from = msg.From or "self"
  balances[from] = (balances[from] or 0) - qty
  local to = msg.Tags.Recipient
  balances[to] = (balances[to] or 0) + qty
  ao.send({ Target = to, Action = "Credit-Notice", Quantity = tostring(qty), Sender = from })
  msg.reply({ Data = "Transferred" })
end)
Handlers.add("Balance", "Balance", function(msg)
  msg.reply({ Data = tostring(balances[msg.From or "self"] or 0) })
end)
`
      const receiverSrc = `
local credits = 0
Handlers.add("Credit-Notice", "Credit-Notice", function(msg)
  credits = credits + tonumber(msg.Tags.Quantity or "0")
end)
Handlers.add("GetCredits", "GetCredits", function(msg)
  msg.reply({ Data = tostring(credits) })
end)
`
      const { p: token, pid: tokenPid } = await ao.deploy({
        src_data: tokenSrc,
      })
      const { p: receiver, pid: receiverPid } = await ao.deploy({
        src_data: receiverSrc,
      })

      await token.m("Mint", { Quantity: "100" })
      await token.m("Transfer", { Recipient: receiverPid, Quantity: "30" })
      const senderBal = await token.d("Balance")
      const credits = await receiver.d("GetCredits")
      return { senderBal, credits, tokenPid, receiverPid }
    })
    expect(result.senderBal).toBe("70")
    expect(result.credits).toBe("30")
  })

  test("multiple processes coexist in mem.env", async ({ page }) => {
    const result = await page.evaluate(async () => {
      const ao = globalThis.g.ao
      const a = await ao.deploy({ src_data: "-- a\n" })
      const b = await ao.deploy({ src_data: "-- b\n" })
      const c = await ao.deploy({ src_data: "-- c\n" })
      return {
        all: [a.pid, b.pid, c.pid],
        uniq: new Set([a.pid, b.pid, c.pid]).size,
        envSize: Object.keys(ao.mem.env).length,
      }
    })
    expect(result.uniq).toBe(3)
    expect(result.envSize).toBeGreaterThanOrEqual(3)
  })
})
