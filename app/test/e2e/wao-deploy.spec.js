// @ts-check
import { test, expect } from "@playwright/test"

test.describe("WAO Studio — runtime AO ops", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("/")
    await page.waitForLoadState("networkidle", { timeout: 60000 })
    await page.waitForFunction(
      () => Boolean(globalThis.g?.ao?.mem),
      null,
      { timeout: 60000 },
    )
  })

  test("can deploy a new process via g.ao.deploy()", async ({ page }) => {
    // Drive the wao SDK directly from the page context — this verifies the
    // 0.41.x AO surface (spwn + deploy + msg + dry) round-trips correctly
    // through the in-memory emulator.
    const result = await page.evaluate(async () => {
      const src = `
Handlers.add("Inc", "Inc", function(msg)
  count = (count or 0) + 1
  msg.reply({ Data = "ok" })
end)
Handlers.add("Get", "Get", function(msg)
  msg.reply({ Data = tostring(count or 0) })
end)
`
      const ao = globalThis.g.ao
      const { p, pid, err } = await ao.deploy({ src_data: src })
      if (err) return { err: String(err) }
      const beforeStr = await p.d("Get")
      await p.m("Inc")
      const afterStr = await p.d("Get")
      return { pid, beforeStr, afterStr }
    })
    expect(result.err).toBeUndefined()
    expect(typeof result.pid).toBe("string")
    expect(result.pid.length).toBeGreaterThan(0)
    expect(result.beforeStr).toBe("0")
    expect(result.afterStr).toBe("1")
  })

  test("Send().receive() coroutine works (the v0.41.1 fix)", async ({
    page,
  }) => {
    const result = await page.evaluate(async () => {
      const src = `
Handlers.add("SelfReceive", "SelfReceive", function(msg)
  local reply = Send({ Target = ao.id, Action = "SelfReply" }).receive()
  msg.reply({ Data = "Got: " .. (reply and reply.Data or "nil") })
end)
Handlers.add("SelfReply", "SelfReply", function(msg)
  msg.reply({ Data = "SelfData" })
end)
`
      const ao = globalThis.g.ao
      const { p, err } = await ao.deploy({ src_data: src })
      if (err) return { err: String(err) }
      const out = await p.m("SelfReceive", false)
      return { out }
    })
    expect(result.err).toBeUndefined()
    // mode1 (in-memory) Send().receive() should resolve to "Got: SelfData"
    // after the 0.41.1 fix.
    expect(result.out).toBe("Got: SelfData")
  })

  test("mem.env tracks deployed processes", async ({ page }) => {
    const before = await page.evaluate(
      () => Object.keys(globalThis.g.ao.mem.env).length,
    )
    await page.evaluate(async () => {
      await globalThis.g.ao.deploy({ src_data: "-- empty\n" })
    })
    const after = await page.evaluate(
      () => Object.keys(globalThis.g.ao.mem.env).length,
    )
    expect(after).toBeGreaterThan(before)
  })
})
