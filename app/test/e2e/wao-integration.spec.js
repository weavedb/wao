// @ts-check
import { test, expect } from "@playwright/test"

test.describe("WAO Studio — wao integration", () => {
  test("AO instance is created and the default process is deployed", async ({
    page,
  }) => {
    await page.goto("/")
    await page.waitForLoadState("networkidle", { timeout: 60000 })

    // The Global component sets `g.ao` after `new AO(...).init(...)`. The
    // zustand store flips `init` to true once everything is ready. Probe
    // both via window globals.
    await page.waitForFunction(
      () => Boolean(globalThis.g?.ao?.mem),
      null,
      { timeout: 60000 },
    )

    const probe = await page.evaluate(() => {
      const g = globalThis.g
      return {
        hasAO: Boolean(g?.ao),
        hasMem: Boolean(g?.ao?.mem),
        variant: g?.ao?.variant,
        moduleCount: Object.keys(g?.ao?.mem?.modules ?? {}).length,
        wasmCount: Object.keys(g?.ao?.mem?.wasms ?? {}).length,
      }
    })
    expect(probe.hasAO).toBe(true)
    expect(probe.hasMem).toBe(true)
    expect(probe.variant).toBe("ao.TN.1")
    // Default modules registered by armem.init() — there must be at least one.
    expect(probe.moduleCount).toBeGreaterThan(0)
    expect(probe.wasmCount).toBeGreaterThan(0)
  })

  test("a default process exists in the in-memory env after boot", async ({
    page,
  }) => {
    await page.goto("/")
    await page.waitForLoadState("networkidle", { timeout: 60000 })

    await page.waitForFunction(
      () => Object.keys(globalThis.g?.ao?.mem?.env ?? {}).length > 0,
      null,
      { timeout: 60000 },
    )

    const procCount = await page.evaluate(
      () => Object.keys(globalThis.g.ao.mem.env).length,
    )
    expect(procCount).toBeGreaterThan(0)
  })
})
