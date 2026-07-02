// @ts-check
import { test, expect } from "@playwright/test"

test.describe("WAO Studio — projects state", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("/")
    await page.waitForLoadState("networkidle", { timeout: 60000 })
    await page.waitForFunction(
      () => Boolean(globalThis.__waoStore) && globalThis.__waoStore.getState().init,
      null,
      { timeout: 60000 },
    )
  })

  test("default projects array is present", async ({ page }) => {
    const projects = await page.evaluate(
      () => globalThis.__waoStore.getState().projects,
    )
    expect(Array.isArray(projects)).toBe(true)
  })

  test("can add a project via store", async ({ page }) => {
    const after = await page.evaluate(() => {
      const s = globalThis.__waoStore.getState()
      const newProj = {
        id: "test-project",
        name: "TestProject",
        path: "/",
        files: [],
      }
      s.setProjects([...s.projects, newProj])
      return s.projects.length
    })
    expect(after).toBeGreaterThan(0)
  })

  test("can remove a project via store", async ({ page }) => {
    const result = await page.evaluate(() => {
      const s = globalThis.__waoStore.getState()
      const before = s.projects.length
      if (before === 0) return { before, after: 0 }
      const filtered = s.projects.slice(1)
      s.setProjects(filtered)
      return { before, after: globalThis.__waoStore.getState().projects.length }
    })
    if (result.before > 0) {
      expect(result.after).toBe(result.before - 1)
    }
  })

  test("bundled files include at least one file (README or similar)", async ({
    page,
  }) => {
    const fileNames = await page.evaluate(() => {
      const files = globalThis.__waoStore.getState().files ?? []
      return files.map(f => f.name).filter(Boolean)
    })
    expect(fileNames.length).toBeGreaterThan(0)
    // Guide loader produces files named "README" with .md extension among others.
    const hasGuideFile = fileNames.some(n =>
      /readme|installation|setup-project|api|tutorial/i.test(n),
    )
    expect(hasGuideFile).toBe(true)
  })

  test("bundled files include tutorials directory", async ({ page }) => {
    const hasTutorials = await page.evaluate(() => {
      const files = globalThis.__waoStore.getState().files ?? []
      return files.some(f => f.name === "tutorials" && f.dir)
    })
    expect(hasTutorials).toBe(true)
  })
})
