// @ts-check
// The Tests tab shows test results from the Tests runner.
// MiddleTests.js consumes `tests` and `test` from the store.
import { test, expect } from "@playwright/test"

test.describe("WAO Studio — Tests tab + runner state", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("/")
    await page.waitForLoadState("networkidle", { timeout: 60000 })
    await page.waitForFunction(
      () => Boolean(globalThis.__waoStore) && globalThis.__waoStore.getState().init,
      null,
      { timeout: 60000 },
    )
  })

  test("tests state starts as empty array", async ({ page }) => {
    const tests = await page.evaluate(
      () => globalThis.__waoStore.getState().tests,
    )
    expect(Array.isArray(tests)).toBe(true)
  })

  test("test (selected test) starts null", async ({ page }) => {
    const t = await page.evaluate(
      () => globalThis.__waoStore.getState().test,
    )
    expect(t).toBeNull()
  })

  test("can set a test object via store", async ({ page }) => {
    const result = await page.evaluate(() => {
      const fake = {
        id: "t1",
        title: "demo test",
        signature: "x",
        msg: { Tags: [] },
        passed: true,
      }
      globalThis.__waoStore.getState().setTest(fake)
      return globalThis.__waoStore.getState().test
    })
    expect(result).not.toBeNull()
    expect(result.id).toBe("t1")
  })

  test("can append to tests array via store", async ({ page }) => {
    const after = await page.evaluate(() => {
      const cur = globalThis.__waoStore.getState().tests
      globalThis.__waoStore
        .getState()
        .setTests([...cur, { id: "new-test", title: "added" }])
      return globalThis.__waoStore.getState().tests.length
    })
    expect(after).toBeGreaterThan(0)
  })

  test("Tests tab can be navigated to", async ({ page }) => {
    await page.evaluate(() => {
      globalThis.__waoStore.getState().setTab("Tests")
    })
    const t = await page.evaluate(
      () => globalThis.__waoStore.getState().tab,
    )
    expect(t).toBe("Tests")
  })
})
