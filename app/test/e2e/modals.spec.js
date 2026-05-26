// @ts-check
import { test, expect } from "@playwright/test"

// Modal store keys (see app/lib/use.js). The Modal helper in
// components/modals/Modal.js renders a backdrop + dialog when truthy.
const MODALS = [
  { key: "modal", name: "CreateFileModal" },
  { key: "modal2", name: "LaunchNetworkModal" },
  { key: "modal3", name: "CreateProjectModal" },
  { key: "modal4", name: "CreateFolderModal" },
  { key: "modal5", name: "ProxyModal" },
  { key: "modal6", name: "FSModal" },
  { key: "modal7", name: "ImportModal" },
  { key: "modal8", name: "RenameFileModal" },
]

test.describe("WAO Studio — modal lifecycle", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("/")
    await page.waitForLoadState("networkidle", { timeout: 60000 })
    await page.waitForFunction(
      () => Boolean(globalThis.__waoStore) && globalThis.__waoStore.getState().init,
      null,
      { timeout: 60000 },
    )
  })

  for (const { key, name } of MODALS) {
    test(`${name}: open via store, then close`, async ({ page }) => {
      // Open
      await page.evaluate(k => {
        const setter = `set${k[0].toUpperCase()}${k.slice(1)}`
        globalThis.__waoStore.getState()[setter](true)
      }, key)
      const openState = await page.evaluate(
        k => globalThis.__waoStore.getState()[k],
        key,
      )
      expect(openState).toBeTruthy()

      // Close
      await page.evaluate(k => {
        const setter = `set${k[0].toUpperCase()}${k.slice(1)}`
        globalThis.__waoStore.getState()[setter](false)
      }, key)
      const closedState = await page.evaluate(
        k => globalThis.__waoStore.getState()[k],
        key,
      )
      expect(closedState).toBeFalsy()
    })
  }

  test("opening one modal does not open the others", async ({ page }) => {
    await page.evaluate(() => {
      globalThis.__waoStore.getState().setModal(true)
    })
    const states = await page.evaluate(() => {
      const s = globalThis.__waoStore.getState()
      return {
        modal: s.modal,
        modal2: s.modal2,
        modal3: s.modal3,
        modal4: s.modal4,
        modal5: s.modal5,
        modal6: s.modal6,
        modal7: s.modal7,
        modal8: s.modal8,
      }
    })
    expect(states.modal).toBeTruthy()
    expect(states.modal2).toBeFalsy()
    expect(states.modal3).toBeFalsy()
    expect(states.modal4).toBeFalsy()
    expect(states.modal5).toBeFalsy()
    expect(states.modal6).toBeFalsy()
    expect(states.modal7).toBeFalsy()
    expect(states.modal8).toBeFalsy()
  })
})
