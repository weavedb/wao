// @ts-check
// ao.var reads global variables from a Lua process state.
import { test, expect } from "@playwright/test"

test.describe("WAO Studio — ao.var (read process globals)", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("/")
    await page.waitForLoadState("networkidle", { timeout: 60000 })
    await page.waitForFunction(
      () => Boolean(globalThis.g?.ao?.mem),
      null,
      { timeout: 60000 },
    )
  })

  test("ao.var returns a global int", async ({ page }) => {
    const out = await page.evaluate(async () => {
      const ao = globalThis.g.ao
      const { pid } = await ao.deploy({ src_data: "Counter = 42\n" })
      const v = await ao.var({ pid, data: "Counter" })
      return v
    })
    expect(out).toBe(42)
  })

  test("ao.var returns a global string", async ({ page }) => {
    const out = await page.evaluate(async () => {
      const ao = globalThis.g.ao
      const { pid } = await ao.deploy({ src_data: 'Greet = "hello"\n' })
      return await ao.var({ pid, data: "Greet" })
    })
    expect(out).toBe("hello")
  })

  test("ao.var returns a global table", async ({ page }) => {
    const out = await page.evaluate(async () => {
      const ao = globalThis.g.ao
      const { pid } = await ao.deploy({
        src_data: 'Pair = { a = 1, b = "two" }\n',
      })
      return await ao.var({ pid, data: "Pair" })
    })
    expect(out).toEqual({ a: 1, b: "two" })
  })

  test("Process.v shorthand works", async ({ page }) => {
    const out = await page.evaluate(async () => {
      const { p } = await globalThis.g.ao.deploy({ src_data: "Foo = 7\n" })
      return await p.v("Foo")
    })
    expect(out).toBe(7)
  })
})
