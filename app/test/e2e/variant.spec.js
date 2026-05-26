// @ts-check
// Tests the cross-mode helpers + variant-aware behaviors: ao.TN.1 vs ao.DN.1.
import { test, expect } from "@playwright/test"

test.describe("WAO Studio — ao variant behavior", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("/")
    await page.waitForLoadState("networkidle", { timeout: 60000 })
    await page.waitForFunction(
      () => Boolean(globalThis.g?.ao?.mem),
      null,
      { timeout: 60000 },
    )
  })

  test("default variant is ao.TN.1 (per cache store key)", async ({ page }) => {
    const v = await page.evaluate(
      () => globalThis.__waoStore.getState().cache,
    )
    expect(v).toBe("ao.TN.1")
  })

  test("AO instance carries the variant", async ({ page }) => {
    const v = await page.evaluate(
      () => globalThis.g.ao.variant ?? null,
    )
    expect(v).toBe("ao.TN.1")
  })

  test("networks list contains ao.TN.1", async ({ page }) => {
    const tags = await page.evaluate(() => {
      const nets = globalThis.__waoStore.getState().networks ?? []
      return nets.map(n => n.tag)
    })
    expect(tags).toContain("ao.TN.1")
  })

  test("setCache to ao.DN.1 updates the store", async ({ page }) => {
    await page.evaluate(() => {
      globalThis.__waoStore.getState().setCache("ao.DN.1")
    })
    const v = await page.evaluate(
      () => globalThis.__waoStore.getState().cache,
    )
    expect(v).toBe("ao.DN.1")
  })
})
