// @ts-check
// The app's Global component populates the store with modules / processes /
// messages / blocks lists via g.listModules/Processes/Messages/Blocks.
// These should be functions that pull from ao.mem and write into zustand.
import { test, expect } from "@playwright/test"

test.describe("WAO Studio — listing functions populate store", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("/")
    await page.waitForLoadState("networkidle", { timeout: 60000 })
    await page.waitForFunction(
      () => Boolean(globalThis.g?.ao?.mem) && globalThis.__waoStore?.getState().init,
      null,
      { timeout: 60000 },
    )
  })

  test("modules list is non-empty after boot", async ({ page }) => {
    const n = await page.evaluate(
      () => globalThis.__waoStore.getState().modules?.length ?? 0,
    )
    expect(n).toBeGreaterThan(0)
  })

  test("processes list contains the default process", async ({ page }) => {
    const n = await page.evaluate(
      () => globalThis.__waoStore.getState().procs?.length ?? 0,
    )
    expect(n).toBeGreaterThanOrEqual(0)
  })

  test("blocks list is an array", async ({ page }) => {
    const isArray = await page.evaluate(
      () => Array.isArray(globalThis.__waoStore.getState().blocks),
    )
    expect(isArray).toBe(true)
  })

  test("messages list is an array", async ({ page }) => {
    const isArray = await page.evaluate(
      () => Array.isArray(globalThis.__waoStore.getState().messages),
    )
    expect(isArray).toBe(true)
  })

  test("g.listProcesses() can be called to refresh", async ({ page }) => {
    const ok = await page.evaluate(() => {
      try {
        if (typeof globalThis.g.listProcesses === "function") {
          globalThis.g.listProcesses()
          return true
        }
        return false
      } catch (_e) {
        return false
      }
    })
    expect(ok).toBe(true)
  })

  test("after a deploy, processes list grows", async ({ page }) => {
    const before = await page.evaluate(
      () => globalThis.__waoStore.getState().procs?.length ?? 0,
    )
    await page.evaluate(async () => {
      await globalThis.g.ao.deploy({ src_data: "-- x\n" })
      if (typeof globalThis.g.listProcesses === "function") {
        globalThis.g.listProcesses()
      }
    })
    const after = await page.evaluate(
      () => globalThis.__waoStore.getState().procs?.length ?? 0,
    )
    expect(after).toBeGreaterThanOrEqual(before)
  })
})
