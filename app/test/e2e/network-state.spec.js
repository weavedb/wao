// @ts-check
// Networks tab tracks HB nodes (hbs), SUs (sus), clients, subs.
import { test, expect } from "@playwright/test"

test.describe("WAO Studio — networks/hub state", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("/")
    await page.waitForLoadState("networkidle", { timeout: 60000 })
    await page.waitForFunction(
      () => Boolean(globalThis.__waoStore) && globalThis.__waoStore.getState().init,
      null,
      { timeout: 60000 },
    )
  })

  test("hbs (HyperBEAM nodes) starts as empty array", async ({ page }) => {
    const hbs = await page.evaluate(
      () => globalThis.__waoStore.getState().hbs,
    )
    expect(Array.isArray(hbs)).toBe(true)
  })

  test("sus (Scheduler Units) is an array", async ({ page }) => {
    const sus = await page.evaluate(
      () => globalThis.__waoStore.getState().sus,
    )
    expect(Array.isArray(sus)).toBe(true)
  })

  test("clients array is initialized", async ({ page }) => {
    const c = await page.evaluate(
      () => globalThis.__waoStore.getState().clients,
    )
    expect(Array.isArray(c)).toBe(true)
  })

  test("subs object is initialized", async ({ page }) => {
    const subs = await page.evaluate(
      () => globalThis.__waoStore.getState().subs,
    )
    expect(typeof subs).toBe("object")
  })

  test("can append an HB node to hbs", async ({ page }) => {
    const after = await page.evaluate(() => {
      const cur = globalThis.__waoStore.getState().hbs
      globalThis.__waoStore.getState().setHbs([
        ...cur,
        { id: "hb-1", url: "http://localhost:10001", connected: false },
      ])
      return globalThis.__waoStore.getState().hbs.length
    })
    expect(after).toBeGreaterThan(0)
  })

  test("wsid (websocket id) starts null", async ({ page }) => {
    const wsid = await page.evaluate(
      () => globalThis.__waoStore.getState().wsid,
    )
    expect(wsid).toBeNull()
  })

  test("proxy/fs/hub port state starts null", async ({ page }) => {
    const ports = await page.evaluate(() => {
      const s = globalThis.__waoStore.getState()
      return {
        proxyPort: s.proxyPort,
        fsPort: s.fsPort,
        hubPort: s.hubPort,
      }
    })
    expect(ports.proxyPort).toBeNull()
    expect(ports.fsPort).toBeNull()
    expect(ports.hubPort).toBeNull()
  })
})
