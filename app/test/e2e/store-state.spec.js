// @ts-check
import { test, expect } from "@playwright/test"

test.describe("WAO Studio — store state operations", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("/")
    await page.waitForLoadState("networkidle", { timeout: 60000 })
    await page.waitForFunction(
      () => Boolean(globalThis.__waoStore) && globalThis.__waoStore.getState().init,
      null,
      { timeout: 60000 },
    )
  })

  test("dryrun toggle round-trips", async ({ page }) => {
    const before = await page.evaluate(
      () => globalThis.__waoStore.getState().dryrun,
    )
    await page.evaluate(() => {
      globalThis.__waoStore.getState().setDryrun(false)
    })
    const after1 = await page.evaluate(
      () => globalThis.__waoStore.getState().dryrun,
    )
    expect(after1).toBe(false)
    await page.evaluate(() => {
      globalThis.__waoStore.getState().setDryrun(true)
    })
    const after2 = await page.evaluate(
      () => globalThis.__waoStore.getState().dryrun,
    )
    expect(after2).toBe(true)
  })

  test("ctype switching: ao.TN.1 → ao.DN.1 → back", async ({ page }) => {
    await page.evaluate(() => {
      globalThis.__waoStore.getState().setCtype("ao.DN.1")
    })
    const dn = await page.evaluate(
      () => globalThis.__waoStore.getState().ctype,
    )
    expect(dn).toBe("ao.DN.1")
    await page.evaluate(() => {
      globalThis.__waoStore.getState().setCtype("ao.TN.1")
    })
    const tn = await page.evaluate(
      () => globalThis.__waoStore.getState().ctype,
    )
    expect(tn).toBe("ao.TN.1")
  })

  test("ttab (terminal tab) accepts lua/js", async ({ page }) => {
    for (const v of ["js", "lua"]) {
      await page.evaluate(t => {
        globalThis.__waoStore.getState().setTtab(t)
      }, v)
      const cur = await page.evaluate(
        () => globalThis.__waoStore.getState().ttab,
      )
      expect(cur).toBe(v)
    }
  })

  test("bundled docs are loaded into files state", async ({ page }) => {
    const fileCount = await page.evaluate(
      () => globalThis.__waoStore.getState().files.length,
    )
    expect(fileCount).toBeGreaterThan(0)
  })

  test("openFiles starts with one file", async ({ page }) => {
    const openCount = await page.evaluate(
      () => globalThis.__waoStore.getState().openFiles.length,
    )
    expect(openCount).toBeGreaterThanOrEqual(1)
  })
})
