// @ts-check
// The `check` option on p.m() / p.d() polls the result and asserts via spec.
import { test, expect } from "@playwright/test"

test.describe("WAO Studio — check spec on messages", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("/")
    await page.waitForLoadState("networkidle", { timeout: 60000 })
    await page.waitForFunction(
      () => Boolean(globalThis.g?.ao?.mem),
      null,
      { timeout: 60000 },
    )
  })

  test("p.m() with check: string passes on matching tag", async ({ page }) => {
    const out = await page.evaluate(async () => {
      const { p } = await globalThis.g.ao.deploy({
        src_data: `
Handlers.add("Stat","Stat",function(m)
  m.reply({Data="ok", Tags={["X-Status"]="green"}})
end)
`,
      })
      const r = await p.msg("Stat", null, { check: { "X-Status": "green" } })
      return { err: r.err ?? null, hasOut: typeof r.out !== "undefined" }
    })
    expect(out.err).toBeNull()
  })

  test("p.d() returns raw Data when get: false", async ({ page }) => {
    const out = await page.evaluate(async () => {
      const { p } = await globalThis.g.ao.deploy({
        src_data: `
Handlers.add("Stat","Stat",function(m)
  m.reply({Data = '{"score":42,"label":"green"}'})
end)
`,
      })
      return await p.d("Stat", null, false)
    })
    expect(typeof out).toBe("string")
    expect(out).toContain("score")
  })

  test("p.d() with get: 'X-Tag' returns single tag value", async ({
    page,
  }) => {
    const out = await page.evaluate(async () => {
      const { p } = await globalThis.g.ao.deploy({
        src_data: `
Handlers.add("Stat","Stat",function(m)
  m.reply({Data="ok", Tags={Tier="gold"}})
end)
`,
      })
      return await p.d("Stat", null, "Tier")
    })
    expect(out).toBe("gold")
  })
})
