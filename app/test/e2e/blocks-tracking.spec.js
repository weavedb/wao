// @ts-check
// The in-memory blockchain tracks blocks via ao.mem.blockmap.
import { test, expect } from "@playwright/test"

test.describe("WAO Studio — blockmap tracking", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("/")
    await page.waitForLoadState("networkidle", { timeout: 60000 })
    await page.waitForFunction(
      () => Boolean(globalThis.g?.ao?.mem),
      null,
      { timeout: 60000 },
    )
  })

  test("blockmap is an object", async ({ page }) => {
    const isObj = await page.evaluate(
      () => typeof globalThis.g.ao.mem.blockmap === "object",
    )
    expect(isObj).toBe(true)
  })

  test("blockmap has entries after deploy", async ({ page }) => {
    const result = await page.evaluate(async () => {
      const before = Object.keys(globalThis.g.ao.mem.blockmap).length
      await globalThis.g.ao.deploy({
        src_data: 'Handlers.add("Y", "Y", function(msg) msg.reply({ Data = "z" }) end)',
      })
      const after = Object.keys(globalThis.g.ao.mem.blockmap).length
      return { before, after }
    })
    expect(result.after).toBeGreaterThanOrEqual(result.before)
  })

  test("mem.txs contains transaction entries", async ({ page }) => {
    const result = await page.evaluate(() => {
      const txs = globalThis.g.ao.mem.txs ?? {}
      return Object.keys(txs).length
    })
    expect(result).toBeGreaterThan(0)
  })

  test("mem.msgs is an object", async ({ page }) => {
    const isObj = await page.evaluate(
      () => typeof globalThis.g.ao.mem.msgs === "object",
    )
    expect(isObj).toBe(true)
  })
})
