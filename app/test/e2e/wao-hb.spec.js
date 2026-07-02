// @ts-check
// Tests that exercise the real local HyperBEAM via the app.
// Requires global-setup.mjs to boot HB on http://localhost:10001.
import { test, expect } from "@playwright/test"

test.describe("WAO Studio — HyperBEAM integration (real local HB)", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("/")
    await page.waitForLoadState("networkidle", { timeout: 60000 })
    await page.waitForFunction(
      () => Boolean(globalThis.g?.ao?.mem),
      null,
      { timeout: 60000 },
    )
  })

  test("Global.js created an in-memory AO instance with mem ready", async ({
    page,
  }) => {
    // Global.js runs the IDE in in-memory mode (the sidebar/middle panels
    // read from ao.mem). HB integration is exposed via the Adaptor in
    // ProxyModal/FSModal, not directly through the AO constructor.
    const aoInfo = await page.evaluate(() => {
      const ao = globalThis.g.ao
      return {
        hasAO: Boolean(ao),
        hasMem: Boolean(ao?.mem),
        memInitialized:
          Boolean(ao?.mem?.env) && Boolean(ao?.mem?.modules),
      }
    })
    expect(aoInfo.hasAO).toBe(true)
    expect(aoInfo.hasMem).toBe(true)
    expect(aoInfo.memInitialized).toBe(true)
  })

  test("can fetch /~meta@1.0/info/address directly from HB", async ({
    request,
  }) => {
    const res = await request.get(
      "http://localhost:10001/~meta@1.0/info/address",
    )
    expect(res.status()).toBe(200)
    const body = await res.text()
    expect(body.length).toBeGreaterThan(20)
  })

  test("can hit HB /~meta@1.0/info JSON endpoint", async ({ request }) => {
    const res = await request.get(
      "http://localhost:10001/~meta@1.0/info/serialize~json@1.0",
    )
    expect(res.status()).toBe(200)
  })
})
