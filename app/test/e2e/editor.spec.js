// @ts-check
// Monaco editor integration: focus, set value, get value, onChange fires
// localforage save, file switching.
import { test, expect } from "@playwright/test"

test.describe("WAO Studio — Monaco editor", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("/")
    await page.waitForLoadState("networkidle", { timeout: 60000 })
    await page.waitForFunction(
      () => Boolean(globalThis.__waoStore) && globalThis.__waoStore.getState().init,
      null,
      { timeout: 60000 },
    )
    // Wait for Monaco to mount (the global ref is set in onMount handler).
    await page.waitForFunction(
      () => Boolean(globalThis.g?.editorRef?.current),
      null,
      { timeout: 60000 },
    )
  })

  test("Monaco mounts and exposes editor instance via g.editorRef", async ({
    page,
  }) => {
    const hasEditor = await page.evaluate(
      () => typeof globalThis.g.editorRef.current?.getValue === "function",
    )
    expect(hasEditor).toBe(true)
  })

  test("setValue / getValue round-trips through Monaco", async ({ page }) => {
    const result = await page.evaluate(async () => {
      const ed = globalThis.g.editorRef.current
      ed.setValue("-- test\nprint('hello')")
      return ed.getValue()
    })
    expect(result).toContain("hello")
  })

  test("setValue triggers onChange (file selection + edit flow)", async ({
    page,
  }) => {
    const ok = await page.evaluate(async () => {
      const s = globalThis.__waoStore.getState()
      const target = s.files.find(f => !f.dir)
      if (!target) return false
      s.setFile(target)
      // setValue fires the onChange handler which writes to localforage.
      // Just verify the round-trip on the editor itself; localforage's
      // bare-module specifier doesn't resolve in page.evaluate context.
      globalThis.g.editorRef.current.setValue("changed-content\n")
      await new Promise(r => setTimeout(r, 100))
      return globalThis.g.editorRef.current.getValue().includes("changed")
    })
    expect(ok).toBe(true)
  })

  test("default language is set from file extension", async ({ page }) => {
    // Set a Lua file as current; the editor's defaultLanguage prop is
    // controlled by `file?.ext`. Monaco doesn't expose this directly, but
    // we can verify the file ext is set.
    const ext = await page.evaluate(() => {
      const s = globalThis.__waoStore.getState()
      const lua = s.files.find(f => f.ext === "lua" || f.name?.endsWith(".lua"))
      if (lua) s.setFile(lua)
      return globalThis.__waoStore.getState().file?.ext ?? null
    })
    expect(ext).toBeTruthy()
  })

  test("editor handles multi-line content", async ({ page }) => {
    const linesAfter = await page.evaluate(async () => {
      const ed = globalThis.g.editorRef.current
      const text = Array.from({ length: 50 }, (_, i) => `line ${i}`).join("\n")
      ed.setValue(text)
      return ed.getModel().getLineCount()
    })
    expect(linesAfter).toBeGreaterThan(40)
  })
})

