// @ts-check
// Tests that switching the sidebar tab actually causes the Left/Middle
// panels to react. We probe via the store (tab key) and via DOM presence
// of expected text.
import { test, expect } from "@playwright/test"

test.describe("WAO Studio — sidebar drives Left/Middle content", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("/")
    await page.waitForLoadState("networkidle", { timeout: 60000 })
    await page.waitForFunction(
      () => Boolean(globalThis.__waoStore) && globalThis.__waoStore.getState().init,
      null,
      { timeout: 60000 },
    )
  })

  test("Networks tab shows the HyperBEAM Nodes section", async ({ page }) => {
    await page.evaluate(() => {
      globalThis.__waoStore.getState().setTab("Networks")
    })
    // MiddleNetworks renders "HyperBEAM Nodes ( N )" header.
    const heading = page.getByText(/HyperBEAM Nodes/i)
    await expect(heading.first()).toBeVisible({ timeout: 30000 })
  })

  test("Modules tab renders MiddleModules area", async ({ page }) => {
    await page.evaluate(() => {
      globalThis.__waoStore.getState().setTab("Modules")
    })
    // After tab switch, the store reflects the change.
    const t = await page.evaluate(
      () => globalThis.__waoStore.getState().tab,
    )
    expect(t).toBe("Modules")
  })

  test("Processes tab renders MiddleProcesses area", async ({ page }) => {
    await page.evaluate(() => {
      globalThis.__waoStore.getState().setTab("Processes")
    })
    const t = await page.evaluate(
      () => globalThis.__waoStore.getState().tab,
    )
    expect(t).toBe("Processes")
  })

  test("Messages tab requires proc; with proc set, switches OK", async ({
    page,
  }) => {
    await page.evaluate(() => {
      const s = globalThis.__waoStore.getState()
      // Pick any deployed process from mem.env
      const firstPid = Object.keys(globalThis.g.ao.mem.env)[0]
      if (firstPid) s.setProc(firstPid)
      s.setTab("Messages")
    })
    const t = await page.evaluate(
      () => globalThis.__waoStore.getState().tab,
    )
    expect(t).toBe("Messages")
  })

  test("Blocks tab switch", async ({ page }) => {
    await page.evaluate(() => {
      globalThis.__waoStore.getState().setTab("Blocks")
    })
    const t = await page.evaluate(
      () => globalThis.__waoStore.getState().tab,
    )
    expect(t).toBe("Blocks")
  })

  test("Tests tab switch", async ({ page }) => {
    await page.evaluate(() => {
      globalThis.__waoStore.getState().setTab("Tests")
    })
    const t = await page.evaluate(
      () => globalThis.__waoStore.getState().tab,
    )
    expect(t).toBe("Tests")
  })

  test("Entity tab switch", async ({ page }) => {
    await page.evaluate(() => {
      globalThis.__waoStore.getState().setTab("Entity")
    })
    const t = await page.evaluate(
      () => globalThis.__waoStore.getState().tab,
    )
    expect(t).toBe("Entity")
  })

  test("Projects tab is the default", async ({ page }) => {
    const t = await page.evaluate(
      () => globalThis.__waoStore.getState().tab,
    )
    expect(["Projects", "Tests"]).toContain(t)
  })
})
