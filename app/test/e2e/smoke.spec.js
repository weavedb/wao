// @ts-check
import { test, expect } from "@playwright/test"

test.describe("WAO Studio — smoke", () => {
  test("app boots and renders the IDE shell", async ({ page }) => {
    const errors = []
    page.on("pageerror", err => errors.push(String(err)))

    await page.goto("/")
    await expect(page.locator("body")).toBeVisible({ timeout: 30000 })
    // Wait for the initial AO.init + mem.init to finish — the Logo splash
    // is rendered when `!init`; once init flips to true it disappears.
    await page.waitForLoadState("networkidle", { timeout: 60000 })

    // The Sidebar renders icon-buttons (Projects/Modules/Processes/...) once
    // init is true. Pull the first as a presence probe.
    const sidebar = page.locator('button, [role="button"]').first()
    await expect(sidebar).toBeVisible({ timeout: 30000 })

    expect(errors, `Uncaught page errors:\n${errors.join("\n")}`).toEqual([])
  })

  test("no fatal page errors on load", async ({ page }) => {
    // Only fail on TRUE page errors (uncaught exceptions), not console warnings.
    // The app has known noisy React key warnings from Chakra Flex/Terminal that
    // are unrelated to the wao upgrade — filtered below.
    const fatal = []
    page.on("pageerror", err => fatal.push(String(err)))

    await page.goto("/")
    await page.waitForLoadState("networkidle", { timeout: 60000 })

    expect(fatal, `Fatal errors:\n${fatal.join("\n")}`).toEqual([])
  })
})
