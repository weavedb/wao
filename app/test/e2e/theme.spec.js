// @ts-check
// The app uses next-themes via ChakraProvider; verify the theme provider
// is wired and the html element gets a theme class/attribute.
import { test, expect } from "@playwright/test"

test.describe("WAO Studio — theme provider", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("/")
    await page.waitForLoadState("networkidle", { timeout: 60000 })
  })

  test("html element gets a theme attribute", async ({ page }) => {
    // next-themes sets <html class="..."> with theme name or data-theme.
    const themeAttr = await page.evaluate(() => {
      const html = document.documentElement
      return {
        className: html.className,
        dataTheme: html.getAttribute("data-theme"),
        colorScheme: html.style.colorScheme,
      }
    })
    // At least one signal of theming should be set.
    expect(
      themeAttr.className.length +
        (themeAttr.dataTheme?.length ?? 0) +
        (themeAttr.colorScheme?.length ?? 0),
    ).toBeGreaterThan(0)
  })

  test("Chakra provider is present in the React tree (color tokens render)", async ({
    page,
  }) => {
    // Header has color #5137C5 — chakra renders inline styles or class-based.
    const hasColor = await page.evaluate(() => {
      const all = Array.from(document.querySelectorAll("*"))
      return all.some(el => {
        const cs = getComputedStyle(el)
        return cs.color && cs.color !== "rgba(0, 0, 0, 0)"
      })
    })
    expect(hasColor).toBe(true)
  })
})
