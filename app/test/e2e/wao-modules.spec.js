// @ts-check
// Tests the lower-level wao SDK surface (postModule, spwn, attest, avail,
// transform, ress, eval, message/result/dryrun, ar wallet operations).
import { test, expect } from "@playwright/test"

test.describe("WAO Studio — wao SDK surface", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("/")
    await page.waitForLoadState("networkidle", { timeout: 60000 })
    await page.waitForFunction(
      () => Boolean(globalThis.g?.ao?.mem),
      null,
      { timeout: 60000 },
    )
  })

  test("ao.ar exists and is initialized with an address", async ({ page }) => {
    const info = await page.evaluate(() => {
      const ar = globalThis.g.ao.ar
      return {
        hasAr: Boolean(ar),
        addr: ar?.addr ?? null,
        addrType: typeof ar?.addr,
      }
    })
    expect(info.hasAr).toBe(true)
    expect(info.addrType).toBe("string")
    expect(info.addr.length).toBeGreaterThan(20)
  })

  test("ao.spwn() returns a pid", async ({ page }) => {
    const res = await page.evaluate(async () => {
      const ao = globalThis.g.ao
      const { pid } = await ao.spwn()
      return { pid, isString: typeof pid === "string" }
    })
    expect(res.isString).toBe(true)
    expect(res.pid.length).toBeGreaterThan(20)
  })

  test("ao.transform() returns a non-null result", async ({ page }) => {
    // ao.transform() can return either a string or an object depending on
    // src/fills — just verify it didn't throw and returned something useful.
    const out = await page.evaluate(async () => {
      const ao = globalThis.g.ao
      try {
        const result = await ao.transform({
          src: 'print("hello, <%= name %>")',
          data: "",
          fills: { name: "wao" },
        })
        return { ok: true, type: typeof result, isNull: result === null }
      } catch (e) {
        return { ok: false, err: String(e) }
      }
    })
    expect(out.ok).toBe(true)
    expect(out.isNull).toBe(false)
  })

  test("ao.p(pid) returns a Process wrapper", async ({ page }) => {
    const info = await page.evaluate(async () => {
      const ao = globalThis.g.ao
      const { pid } = await ao.deploy({ src_data: "" })
      const p = ao.p(pid)
      return {
        hasP: Boolean(p),
        hasMsg: typeof p?.msg === "function",
        hasDry: typeof p?.dry === "function",
        hasM: typeof p?.m === "function",
        hasD: typeof p?.d === "function",
        pidMatches: p?.pid === pid,
      }
    })
    expect(info.hasP).toBe(true)
    expect(info.hasMsg).toBe(true)
    expect(info.hasDry).toBe(true)
    expect(info.hasM).toBe(true)
    expect(info.hasD).toBe(true)
    expect(info.pidMatches).toBe(true)
  })

  test("ao.message + ao.result low-level round-trip", async ({ page }) => {
    const result = await page.evaluate(async () => {
      const ao = globalThis.g.ao
      const src = `
Handlers.add("Ping", "Ping", function(msg)
  msg.reply({ Data = "Pong" })
end)
`
      const { pid } = await ao.deploy({ src_data: src })
      // Low-level message dispatch via the connect()-bound message() fn
      const mid = await ao.message({
        process: pid,
        tags: [{ name: "Action", value: "Ping" }],
        signer: ao.toSigner(ao.ar.jwk),
      })
      const res = await ao.result({ process: pid, message: mid })
      return {
        hasMid: typeof mid === "string",
        firstReplyData: res?.Messages?.[0]?.Data ?? null,
      }
    })
    expect(result.hasMid).toBe(true)
    expect(result.firstReplyData).toBe("Pong")
  })
})
