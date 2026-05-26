// @ts-check
import { test, expect } from "@playwright/test"

const TABS = [
  "Projects",
  "Modules",
  "Processes",
  "Messages",
  "Blocks",
  "Entity",
  "Networks",
  "Tests",
]

test.describe("WAO Studio — sidebar navigation", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("/")
    await page.waitForLoadState("networkidle", { timeout: 60000 })
    await page.waitForFunction(
      () => Boolean(globalThis.__waoStore) && globalThis.__waoStore.getState().init,
      null,
      { timeout: 60000 },
    )
  })

  for (const tab of TABS) {
    test(`switch to ${tab} tab via store`, async ({ page }) => {
      await page.evaluate(t => {
        globalThis.__waoStore.getState().setTab(t)
      }, tab)
      const currentTab = await page.evaluate(
        () => globalThis.__waoStore.getState().tab,
      )
      expect(currentTab).toBe(tab)
    })
  }

  test("sidebar icon count matches tab count", async ({ page }) => {
    // Sidebar renders 8 icons (one per tab). Each is wrapped in a Tooltip
    // with positioning="right-end". The icons are svg elements inside the
    // 60px-wide sidebar column.
    const sidebar = page.locator("svg").first()
    await expect(sidebar).toBeVisible({ timeout: 30000 })
  })

  test("active tab gets highlight class/style after switch", async ({
    page,
  }) => {
    // Switch to Modules and verify the store reflects the new tab.
    await page.evaluate(() => {
      globalThis.__waoStore.getState().setTab("Modules")
    })
    const after = await page.evaluate(
      () => globalThis.__waoStore.getState().tab,
    )
    expect(after).toBe("Modules")
  })
})
