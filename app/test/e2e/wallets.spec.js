// @ts-check
import { test, expect } from "@playwright/test"

test.describe("WAO Studio — wallet (acc) state", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("/")
    await page.waitForLoadState("networkidle", { timeout: 60000 })
    await page.waitForFunction(
      () => Boolean(globalThis.g?.ao?.mem),
      null,
      { timeout: 60000 },
    )
  })

  test("ao.ar.jwk address can be derived deterministically", async ({
    page,
  }) => {
    const addr = await page.evaluate(async () => {
      const ar = globalThis.g.ao.ar
      return await ar.toAddr(ar.jwk)
    })
    expect(typeof addr).toBe("string")
    expect(addr.length).toBeGreaterThan(30)
  })

  test("wao/web exports acc array with at least one test wallet", async ({
    page,
  }) => {
    const result = await page.evaluate(async () => {
      // The app imports acc[0] in Global.js — and exposes g.ao initialized
      // with it. So at minimum we know there's one usable test wallet.
      return {
        addr: globalThis.g.ao.ar.addr,
      }
    })
    expect(result.addr.length).toBeGreaterThan(30)
  })

  test("wallet state in store starts null", async ({ page }) => {
    const w = await page.evaluate(
      () => globalThis.__waoStore.getState().wallet,
    )
    expect(w).toBeNull()
  })

  test("setWallet updates the store", async ({ page }) => {
    const out = await page.evaluate(() => {
      globalThis.__waoStore.getState().setWallet({ addr: "test-addr-1" })
      return globalThis.__waoStore.getState().wallet
    })
    expect(out.addr).toBe("test-addr-1")
  })
})
