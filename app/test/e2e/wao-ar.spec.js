// @ts-check
import { test, expect } from "@playwright/test"

test.describe("WAO Studio — ar (AR wallet/network) surface", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("/")
    await page.waitForLoadState("networkidle", { timeout: 60000 })
    await page.waitForFunction(
      () => Boolean(globalThis.g?.ao?.ar?.addr),
      null,
      { timeout: 60000 },
    )
  })

  test("ar.jwk is loaded and looks like a JWK", async ({ page }) => {
    const info = await page.evaluate(() => {
      const jwk = globalThis.g.ao.ar?.jwk
      return {
        hasJwk: Boolean(jwk),
        hasN: typeof jwk?.n === "string",
        hasKty: jwk?.kty,
      }
    })
    expect(info.hasJwk).toBe(true)
    expect(info.hasN).toBe(true)
    expect(info.hasKty).toBe("RSA")
  })

  test("ar.checkWallet({ jwk }) returns jwk", async ({ page }) => {
    const info = await page.evaluate(async () => {
      const ar = globalThis.g.ao.ar
      const result = await ar.checkWallet({ jwk: ar.jwk })
      return {
        hasJwk: Boolean(result?.jwk),
        err: result?.err ?? null,
      }
    })
    expect(info.hasJwk).toBe(true)
    expect(info.err).toBeNull()
  })

  test("ar.toAddr(jwk) returns the same addr each call", async ({ page }) => {
    const info = await page.evaluate(async () => {
      const ar = globalThis.g.ao.ar
      const a = await ar.toAddr(ar.jwk)
      const b = await ar.toAddr(ar.jwk)
      return { a, b, match: a === b, expected: ar.addr }
    })
    expect(info.match).toBe(true)
    expect(info.a).toBe(info.expected)
  })
})
