// @ts-check
// Entity tab + entity selection: the Entity panel renders different
// content based on what's selected (Module/Process/Message/Tx/Block/Account).
import { test, expect } from "@playwright/test"

test.describe("WAO Studio — entity selection", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("/")
    await page.waitForLoadState("networkidle", { timeout: 60000 })
    await page.waitForFunction(
      () => Boolean(globalThis.__waoStore) && globalThis.__waoStore.getState().init,
      null,
      { timeout: 60000 },
    )
  })

  test("entity state starts null", async ({ page }) => {
    const e = await page.evaluate(
      () => globalThis.__waoStore.getState().entity,
    )
    expect(e).toBeNull()
  })

  test("can set entity to a Module", async ({ page }) => {
    const result = await page.evaluate(() => {
      globalThis.__waoStore
        .getState()
        .setEntity({ id: "mod-id-1", type: "Module" })
      return globalThis.__waoStore.getState().entity
    })
    expect(result.type).toBe("Module")
  })

  test("can set entity to a Process", async ({ page }) => {
    const result = await page.evaluate(() => {
      globalThis.__waoStore
        .getState()
        .setEntity({ id: "proc-id-1", type: "Process" })
      return globalThis.__waoStore.getState().entity
    })
    expect(result.type).toBe("Process")
  })

  test("can set entity to a Message", async ({ page }) => {
    const result = await page.evaluate(() => {
      globalThis.__waoStore
        .getState()
        .setEntity({ id: "msg-id-1", type: "Message" })
      return globalThis.__waoStore.getState().entity
    })
    expect(result.type).toBe("Message")
  })

  test("can clear entity by setting null", async ({ page }) => {
    await page.evaluate(() => {
      globalThis.__waoStore.getState().setEntity({ id: "x", type: "Tx" })
    })
    await page.evaluate(() => {
      globalThis.__waoStore.getState().setEntity(null)
    })
    const cur = await page.evaluate(
      () => globalThis.__waoStore.getState().entity,
    )
    expect(cur).toBeNull()
  })
})
