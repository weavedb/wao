// @ts-check
// Drive the app's wired-up AO instance through real runtime ops. With
// HB running on localhost:10001, the in-browser AO instance may run in
// in-memory mode (fallback) — either way we can exercise the SDK.
import { test, expect } from "@playwright/test"

test.describe("WAO Studio — runtime operations (real HB up)", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("/")
    await page.waitForLoadState("networkidle", { timeout: 60000 })
    await page.waitForFunction(
      () => Boolean(globalThis.g?.ao?.mem),
      null,
      { timeout: 60000 },
    )
  })

  test("deploys a Counter process and increments via app AO", async ({
    page,
  }) => {
    const result = await page.evaluate(async () => {
      const src = `
local count = 0
Handlers.add("Inc", "Inc", function(msg)
  count = count + 1
  msg.reply({ Data = "ok" })
end)
Handlers.add("Get", "Get", function(msg)
  msg.reply({ Data = tostring(count) })
end)
`
      const ao = globalThis.g.ao
      const { p, pid, err } = await ao.deploy({ src_data: src })
      if (err) return { err: String(err) }
      await p.m("Inc")
      await p.m("Inc")
      await p.m("Inc")
      const out = await p.d("Get")
      return { pid, out }
    })
    expect(result.err).toBeUndefined()
    expect(result.out).toBe("3")
  })

  test("dry-run with arguments returns reply", async ({ page }) => {
    const result = await page.evaluate(async () => {
      const src = `
Handlers.add("Echo", "Echo", function(msg)
  msg.reply({ Data = "Hello, " .. (msg.Tags.Name or "anon") })
end)
`
      const ao = globalThis.g.ao
      const { p } = await ao.deploy({ src_data: src })
      const reply = await p.d("Echo", { Name: "world" }, false)
      return { reply }
    })
    expect(result.reply).toBe("Hello, world")
  })

  test("Token Mint + Balance round-trip", async ({ page }) => {
    const result = await page.evaluate(async () => {
      const src = `
local balances = {}
Handlers.add("Mint", "Mint", function(msg)
  local qty = tonumber(msg.Tags.Quantity or "0")
  local from = msg.From or "self"
  balances[from] = (balances[from] or 0) + qty
  msg.reply({ Data = "Minted" })
end)
Handlers.add("Balance", "Balance", function(msg)
  local from = msg.From or "self"
  msg.reply({ Data = tostring(balances[from] or 0) })
end)
`
      const ao = globalThis.g.ao
      const { p } = await ao.deploy({ src_data: src })
      await p.m("Mint", { Quantity: "42" })
      const bal = await p.d("Balance")
      return { bal }
    })
    expect(result.bal).toBe("42")
  })

  test("deployed process shows up in mem.env", async ({ page }) => {
    const result = await page.evaluate(async () => {
      const before = Object.keys(globalThis.g.ao.mem.env).length
      const { pid } = await globalThis.g.ao.deploy({
        src_data: '-- empty handler\n',
      })
      const after = Object.keys(globalThis.g.ao.mem.env).length
      const isPresent = Boolean(globalThis.g.ao.mem.env[pid])
      return { before, after, isPresent }
    })
    expect(result.after).toBeGreaterThan(result.before)
    expect(result.isPresent).toBe(true)
  })
})
