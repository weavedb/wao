// @ts-check
import { test, expect } from "@playwright/test"

test.describe("WaoLlama — smoke", () => {
  test("app boots and renders", async ({ page }) => {
    const fatal = []
    page.on("pageerror", err => fatal.push(String(err)))

    await page.goto("/")
    await expect(page.locator("body")).toBeVisible({ timeout: 30000 })
    await page.waitForLoadState("networkidle", { timeout: 60000 })

    // The landing has a "Deploy Agent on AO" button (or "Initializing...")
    // when not yet inited.
    const deployBtn = page.getByText(/Deploy Agent|Initializing/i)
    await expect(deployBtn.first()).toBeVisible({ timeout: 30000 })

    expect(fatal, `Fatal errors:\n${fatal.join("\n")}`).toEqual([])
  })

  test("WaoLlama heading is present", async ({ page }) => {
    await page.goto("/")
    await page.waitForLoadState("networkidle", { timeout: 60000 })
    // The page mentions WaoLlama prominently.
    await expect(page.getByText(/WaoLlama|wao/i).first()).toBeVisible({
      timeout: 30000,
    })
  })
})
