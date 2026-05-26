// @ts-check
import { test, expect } from "@playwright/test"

test.describe("WAO Studio — ao.eval + ao.load + ao.wait", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("/")
    await page.waitForLoadState("networkidle", { timeout: 60000 })
    await page.waitForFunction(
      () => Boolean(globalThis.g?.ao?.mem),
      null,
      { timeout: 60000 },
    )
  })

  test("ao.eval() runs Lua against a process", async ({ page }) => {
    const out = await page.evaluate(async () => {
      const ao = globalThis.g.ao
      const { pid } = await ao.spwn()
      await ao.wait({ pid })
      const r = await ao.eval({ pid, data: 'return "hello"' })
      return { ok: Boolean(r), keys: Object.keys(r ?? {}) }
    })
    expect(out.ok).toBe(true)
  })

  test("ao.load() injects + computes script", async ({ page }) => {
    const out = await page.evaluate(async () => {
      const ao = globalThis.g.ao
      const { pid } = await ao.spwn()
      await ao.wait({ pid })
      const r = await ao.load({
        pid,
        data: 'Handlers.add("X","X",function(m) m.reply({Data="ok"}) end)',
      })
      return { ok: Boolean(r) }
    })
    expect(out.ok).toBe(true)
  })

  test("ao.wait({ pid }) resolves for an existing process", async ({
    page,
  }) => {
    const result = await page.evaluate(async () => {
      const ao = globalThis.g.ao
      const { pid } = await ao.spwn()
      await ao.wait({ pid, attempts: 10 })
      return { exists: Boolean(ao.mem.env[pid]) }
    })
    expect(result.exists).toBe(true)
  })
})
