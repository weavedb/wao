// @ts-check
// Process class shorthand methods: p.m(), p.d(), p.r(), p.v(), p.o().
import { test, expect } from "@playwright/test"

test.describe("WAO Studio — Process class shorthand methods", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("/")
    await page.waitForLoadState("networkidle", { timeout: 60000 })
    await page.waitForFunction(
      () => Boolean(globalThis.g?.ao?.mem),
      null,
      { timeout: 60000 },
    )
  })

  test("p.m(action) sends a message and returns reply Data", async ({
    page,
  }) => {
    const out = await page.evaluate(async () => {
      const { p } = await globalThis.g.ao.deploy({
        src_data:
          'Handlers.add("Hi","Hi",function(m) m.reply({Data="Hello"}) end)',
      })
      return await p.m("Hi", false)
    })
    expect(out).toBe("Hello")
  })

  test("p.d(action) does a dry run", async ({ page }) => {
    const out = await page.evaluate(async () => {
      const { p } = await globalThis.g.ao.deploy({
        src_data:
          'Handlers.add("Hi","Hi",function(m) m.reply({Data="World"}) end)',
      })
      return await p.d("Hi", false)
    })
    expect(out).toBe("World")
  })

  test("p.m with tags object", async ({ page }) => {
    const out = await page.evaluate(async () => {
      const { p } = await globalThis.g.ao.deploy({
        src_data: `
Handlers.add("Echo","Echo",function(m)
  m.reply({Data = m.Tags.Name or "anon"})
end)
`,
      })
      return await p.m("Echo", { Name: "alice" }, false)
    })
    expect(out).toBe("alice")
  })

  test("p.d with get: 'X-Status' tag value", async ({ page }) => {
    const out = await page.evaluate(async () => {
      const { p } = await globalThis.g.ao.deploy({
        src_data: `
Handlers.add("Stat","Stat",function(m)
  m.reply({Data="ok", Tags={["X-Status"]="green"}})
end)
`,
      })
      // p.d third arg can be get spec — string returns the named tag value
      return await p.d("Stat", null, "X-Status")
    })
    expect(out).toBe("green")
  })

  test("p.pid is exposed", async ({ page }) => {
    const out = await page.evaluate(async () => {
      const { p, pid } = await globalThis.g.ao.deploy({ src_data: "" })
      return { pPid: p.pid, pid, match: p.pid === pid }
    })
    expect(out.match).toBe(true)
  })
})
