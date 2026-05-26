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

  test("Global.js created an AO instance (HB-connected or in-memory fallback)", async ({
    page,
  }) => {
    // Global.js tries `new AO({ hb: hb_url })` first. With CORS/init quirks
    // in a browser context the constructor may still throw and fall back to
    // in-memory mode — either path is acceptable here. The important thing
    // is that g.ao exists and mem is initialized.
    const aoInfo = await page.evaluate(() => {
      const ao = globalThis.g.ao
      return {
        hasAO: Boolean(ao),
        hasMem: Boolean(ao?.mem),
        hasHb: Boolean(ao?.hb),
        hbUrl: ao?.hb?.url ?? null,
      }
    })
    expect(aoInfo.hasAO).toBe(true)
    expect(aoInfo.hasMem).toBe(true)
    if (aoInfo.hasHb) {
      expect(aoInfo.hbUrl).toMatch(/localhost:10001|127\.0\.0\.1:10001/)
    }
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
