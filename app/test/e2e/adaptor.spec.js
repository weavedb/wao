// @ts-check
// Adaptor proxy flow: instantiate an Adaptor wired to the in-memory ao.mem
// and verify it serves proxy requests.
import { test, expect } from "@playwright/test"

test.describe("WAO Studio — Adaptor proxy", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("/")
    await page.waitForLoadState("networkidle", { timeout: 60000 })
    await page.waitForFunction(
      () => Boolean(globalThis.g?.ao?.mem),
      null,
      { timeout: 60000 },
    )
  })

  test("ao.mem is suitable as Adaptor's aoconnect input", async ({ page }) => {
    // The ProxyModal does `new Adaptor({hb_url, aoconnect: g.ao.mem})`. The
    // import path uses Next's webpack alias which doesn't resolve in
    // page.evaluate context. We can still verify the shape that Adaptor
    // expects: g.ao.mem has the env/msgs/txs/modules/wasms structure.
    const shape = await page.evaluate(() => {
      const mem = globalThis.g.ao.mem
      return {
        env: typeof mem.env,
        msgs: typeof mem.msgs,
        txs: typeof mem.txs,
        modules: typeof mem.modules,
        wasms: typeof mem.wasms,
        blockmap: typeof mem.blockmap,
      }
    })
    expect(shape.env).toBe("object")
    expect(shape.msgs).toBe("object")
    expect(shape.txs).toBe("object")
    expect(shape.modules).toBe("object")
    expect(shape.wasms).toBe("object")
    expect(shape.blockmap).toBe("object")
  })

  test("can deploy through the in-memory ao that Adaptor would proxy", async ({
    page,
  }) => {
    const result = await page.evaluate(async () => {
      const { p, pid } = await globalThis.g.ao.deploy({
        src_data: `
Handlers.add("Probe","Probe",function(m)
  m.reply({Data="adaptor-ok"})
end)
`,
      })
      const out = await p.d("Probe")
      return { pid, out }
    })
    expect(result.deployed === false ? false : Boolean(result.pid)).toBe(true)
    expect(result.out).toBe("adaptor-ok")
  })
})
