// @ts-check
import { test, expect } from "@playwright/test"

test.describe("WAO Studio — footer", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("/")
    await page.waitForLoadState("networkidle", { timeout: 60000 })
  })

  test("github link exists and opens in new tab", async ({ page }) => {
    const link = page.locator('a[href="https://github.com/weavedb/wao"]')
    await expect(link).toHaveCount(1)
    const target = await link.getAttribute("target")
    expect(target).toBe("_blank")
  })

  test("docs link exists and opens in new tab", async ({ page }) => {
    const link = page.locator('a[href="https://docs.wao.eco"]')
    await expect(link).toHaveCount(1)
    const target = await link.getAttribute("target")
    expect(target).toBe("_blank")
  })

  test("twitter/x link exists and opens in new tab", async ({ page }) => {
    const link = page.locator('a[href="https://x.com/waoeco"]')
    await expect(link).toHaveCount(1)
    const target = await link.getAttribute("target")
    expect(target).toBe("_blank")
  })
})
