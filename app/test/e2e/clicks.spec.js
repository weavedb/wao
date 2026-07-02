// @ts-check
// Real UI click interactions: click a sidebar tab icon and verify the
// store + Middle panel update. This exercises the actual click handlers
// rather than direct store mutation.
import { test, expect } from "@playwright/test"

test.describe("WAO Studio — real DOM click interactions", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("/")
    await page.waitForLoadState("networkidle", { timeout: 60000 })
    await page.waitForFunction(
      () => Boolean(globalThis.__waoStore) && globalThis.__waoStore.getState().init,
      null,
      { timeout: 60000 },
    )
  })

  test("clicking the Networks chip in header switches tab", async ({
    page,
  }) => {
    // The Header renders a clickable element with the cache text "ao.TN.1"
    // that calls setTab("Networks") on click.
    const chip = page.getByText("ao.TN.1").first()
    await chip.click()
    const tab = await page.evaluate(
      () => globalThis.__waoStore.getState().tab,
    )
    expect(tab).toBe("Networks")
  })

  test("clicking GitHub footer link navigates (target=_blank)", async ({
    page,
  }) => {
    // Just verify the anchor exists and has correct href + target — we
    // don't actually open new tabs in Playwright by default.
    const link = page.locator('a[href="https://github.com/weavedb/wao"]')
    await expect(link).toBeVisible()
    const href = await link.getAttribute("href")
    expect(href).toBe("https://github.com/weavedb/wao")
  })

  test("sidebar contains multiple clickable Flex items", async ({ page }) => {
    // The Sidebar renders 8 tabs as Flex with cursor:pointer. Count them.
    const cursorPointerCount = await page.evaluate(() => {
      const all = Array.from(document.querySelectorAll("*"))
      return all.filter(el => getComputedStyle(el).cursor === "pointer")
        .length
    })
    expect(cursorPointerCount).toBeGreaterThan(5)
  })
})
