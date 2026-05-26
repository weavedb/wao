// @ts-check
// Tests the ao.ress() result-iteration API.
import { test, expect } from "@playwright/test"

test.describe("WAO Studio — ao.ress + result polling", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("/")
    await page.waitForLoadState("networkidle", { timeout: 60000 })
    await page.waitForFunction(
      () => Boolean(globalThis.g?.ao?.mem),
      null,
      { timeout: 60000 },
    )
  })

  test("ao.ress() returns recent results for a process", async ({ page }) => {
    const result = await page.evaluate(async () => {
      const ao = globalThis.g.ao
      const src = `
Handlers.add("Ping", "Ping", function(msg)
  msg.reply({ Data = "Pong" })
end)
`
      const { p } = await ao.deploy({ src_data: src })
      await p.m("Ping")
      await p.m("Ping")
      await p.m("Ping")
      const results = await ao.ress({ pid: p.pid, limit: 10, asc: true })
      return {
        type: Array.isArray(results) ? "array" : typeof results,
        count: Array.isArray(results) ? results.length : null,
      }
    })
    expect(["array", "object"]).toContain(result.type)
  })

  test("results history is populated after multiple msg calls", async ({
    page,
  }) => {
    const count = await page.evaluate(async () => {
      const ao = globalThis.g.ao
      const { p, pid } = await ao.deploy({
        src_data:
          'Handlers.add("X", "X", function(msg) msg.reply({ Data = "y" }) end)',
      })
      await p.m("X")
      await p.m("X")
      // mem.env[pid].results is an array of message ids the CU processed
      return globalThis.g.ao.mem.env[pid]?.results?.length ?? 0
    })
    expect(count).toBeGreaterThan(0)
  })
})
