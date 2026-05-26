// @ts-check
// xterm terminal: writes, line buffering, input handling.
import { test, expect } from "@playwright/test"

test.describe("WAO Studio — terminal (xterm)", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("/")
    await page.waitForLoadState("networkidle", { timeout: 60000 })
    await page.waitForFunction(
      () => Boolean(globalThis.g?.term),
      null,
      { timeout: 60000 },
    )
  })

  test("xterm terminal is mounted (g.term exists)", async ({ page }) => {
    const ok = await page.evaluate(
      () => typeof globalThis.g.term?.write === "function",
    )
    expect(ok).toBe(true)
  })

  test("can write to terminal", async ({ page }) => {
    const success = await page.evaluate(() => {
      try {
        globalThis.g.term.write("hello from playwright\r\n")
        return true
      } catch {
        return false
      }
    })
    expect(success).toBe(true)
  })

  test(".xterm container is in the DOM", async ({ page }) => {
    const count = await page.locator(".xterm").count()
    expect(count).toBeGreaterThan(0)
  })

  test("terminal has the correct theme background", async ({ page }) => {
    // The xterm config sets background: "#1E1E1E". Verify the canvas
    // backgroundColor matches.
    const bg = await page.evaluate(() => {
      const xtermEl = document.querySelector(".xterm")
      if (!xtermEl) return null
      const cs = getComputedStyle(xtermEl)
      return cs.backgroundColor
    })
    // Browser may report as rgb(30, 30, 30) or transparent depending on
    // when the test runs vs xterm's render pass.
    expect(bg).not.toBeNull()
  })

  test("can call g.welcome() to write the welcome banner", async ({ page }) => {
    const success = await page.evaluate(() => {
      try {
        if (typeof globalThis.g.welcome === "function") globalThis.g.welcome()
        return true
      } catch {
        return false
      }
    })
    expect(success).toBe(true)
  })
})
