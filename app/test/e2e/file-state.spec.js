// @ts-check
import { test, expect } from "@playwright/test"

test.describe("WAO Studio — file/project state", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("/")
    await page.waitForLoadState("networkidle", { timeout: 60000 })
    await page.waitForFunction(
      () => Boolean(globalThis.__waoStore) && globalThis.__waoStore.getState().init,
      null,
      { timeout: 60000 },
    )
  })

  test("can set the current file via store", async ({ page }) => {
    const newFile = await page.evaluate(() => {
      const s = globalThis.__waoStore.getState()
      const target = s.files.find(f => !f.dir) ?? s.files[0]
      s.setFile(target)
      return globalThis.__waoStore.getState().file
    })
    expect(newFile).toBeTruthy()
  })

  test("can append a file to openFiles", async ({ page }) => {
    const after = await page.evaluate(() => {
      const s = globalThis.__waoStore.getState()
      const newFile = {
        id: "test-id-1",
        name: "test.lua",
        ext: "lua",
        path: "/",
        pid: "1",
        content: "-- test\n",
      }
      s.setOpenFiles([...s.openFiles, newFile])
      return globalThis.__waoStore.getState().openFiles.length
    })
    expect(after).toBeGreaterThan(1)
  })

  test("selDir state can be updated", async ({ page }) => {
    await page.evaluate(() => {
      globalThis.__waoStore.getState().setSelDir({ pid: "2", path: "/sub/" })
    })
    const cur = await page.evaluate(
      () => globalThis.__waoStore.getState().selDir,
    )
    expect(cur).toEqual({ pid: "2", path: "/sub/" })
  })

  test("preview toggle flips", async ({ page }) => {
    const before = await page.evaluate(
      () => globalThis.__waoStore.getState().preview,
    )
    await page.evaluate(b => {
      globalThis.__waoStore.getState().setPreview(!b)
    }, before)
    const after = await page.evaluate(
      () => globalThis.__waoStore.getState().preview,
    )
    expect(after).toBe(!before)
  })

  test("logs array can be appended via store", async ({ page }) => {
    await page.evaluate(() => {
      const s = globalThis.__waoStore.getState()
      s.setLogs([...s.logs, { id: "x", message: "test" }])
    })
    const count = await page.evaluate(
      () => globalThis.__waoStore.getState().logs.length,
    )
    expect(count).toBeGreaterThan(0)
  })
})
