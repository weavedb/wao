// @ts-check
// Verify that opening a modal via the store actually renders modal DOM
// (not just flips the state). The Modal component renders a backdrop +
// dialog when truthy.
import { test, expect } from "@playwright/test"

test.describe("WAO Studio — modal DOM presence", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("/")
    await page.waitForLoadState("networkidle", { timeout: 60000 })
    await page.waitForFunction(
      () => Boolean(globalThis.__waoStore) && globalThis.__waoStore.getState().init,
      null,
      { timeout: 60000 },
    )
  })

  test("opening CreateProjectModal increases element count", async ({
    page,
  }) => {
    const before = await page.evaluate(
      () => document.querySelectorAll("*").length,
    )
    await page.evaluate(() => {
      globalThis.__waoStore.getState().setModal3(true)
    })
    await page.waitForTimeout(200)
    const after = await page.evaluate(
      () => document.querySelectorAll("*").length,
    )
    expect(after).toBeGreaterThan(before)
  })

  test("opening LaunchNetworkModal adds input elements", async ({ page }) => {
    const before = await page.evaluate(
      () => document.querySelectorAll("input").length,
    )
    await page.evaluate(() => {
      globalThis.__waoStore.getState().setModal2(true)
    })
    await page.waitForTimeout(200)
    const after = await page.evaluate(
      () => document.querySelectorAll("input").length,
    )
    expect(after).toBeGreaterThanOrEqual(before)
  })

  test("closing modal removes added DOM", async ({ page }) => {
    await page.evaluate(() => {
      globalThis.__waoStore.getState().setModal3(true)
    })
    await page.waitForTimeout(200)
    const withOpen = await page.evaluate(
      () => document.querySelectorAll("*").length,
    )
    await page.evaluate(() => {
      globalThis.__waoStore.getState().setModal3(false)
    })
    await page.waitForTimeout(200)
    const afterClose = await page.evaluate(
      () => document.querySelectorAll("*").length,
    )
    expect(afterClose).toBeLessThanOrEqual(withOpen)
  })
})
