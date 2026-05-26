// @ts-check
// ao.postModule() registers a WASM module in the in-memory environment
// or posts to AR. The waollama landing page uses this for the llama wasm.
import { test, expect } from "@playwright/test"

test.describe("WAO Studio — module registration", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("/")
    await page.waitForLoadState("networkidle", { timeout: 60000 })
    await page.waitForFunction(
      () => Boolean(globalThis.g?.ao?.mem),
      null,
      { timeout: 60000 },
    )
  })

  test("default modules are populated in mem.modules", async ({ page }) => {
    const modules = await page.evaluate(
      () => Object.keys(globalThis.g.ao.mem.modules ?? {}),
    )
    expect(modules.length).toBeGreaterThan(0)
  })

  test("aos2_0_4_32 wasm module is registered", async ({ page }) => {
    const has = await page.evaluate(() => {
      const wasms = globalThis.g.ao.mem.wasms ?? {}
      return Object.values(wasms).some(w => /aos2_0_4_32/.test(w.file ?? ""))
    })
    expect(has).toBe(true)
  })

  test("mem.txs has Module entries for each registered wasm", async ({
    page,
  }) => {
    const result = await page.evaluate(() => {
      const txs = globalThis.g.ao.mem.txs ?? {}
      const moduleCount = Object.values(txs).filter(tx => {
        const tags = tx?.tags ?? []
        return tags.some(t => t.name === "Type" && t.value === "Module")
      }).length
      return moduleCount
    })
    expect(result).toBeGreaterThan(0)
  })

  test("default module ID matches data.js mod constant", async ({ page }) => {
    const result = await page.evaluate(() => {
      const wasms = globalThis.g.ao.mem.wasms ?? {}
      // The app's data.js sets mod = "WASM32-D8q2OmZ4Mok00sD2Y_6SYEQ7Hjx-6VZ_jl3g"
      return {
        keys: Object.keys(wasms),
        hasWao: Boolean(wasms["WASM32-D8q2OmZ4Mok00sD2Y_6SYEQ7Hjx-6VZ_jl3g"]),
      }
    })
    expect(result.hasWao).toBe(true)
  })
})
