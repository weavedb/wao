// @ts-check
// Connects to the real WAO signaling hub (src/hub/index.js) booted by
// global-setup.mjs on ws://localhost:7777. Drives the WebSocket protocol
// directly (the same shape lib/hub.js sends/receives).
//
// IMPORTANT: the server sends `registered` immediately on connect. The
// inbox helper attaches the message listener synchronously *before* the
// socket completes connecting, then queues any messages that arrive
// before the test calls recv() for that type. Without this we'd race the
// server's initial 'registered' frame.
import { test, expect } from "@playwright/test"

const HUB_URL = "ws://localhost:7777"

const helperSrc = String.raw`
window.__hub = {
  mksock(url) {
    const ws = new WebSocket(url)
    const queue = []
    const waiters = []
    ws.addEventListener("message", e => {
      const m = JSON.parse(e.data)
      for (let i = 0; i < waiters.length; i++) {
        if (waiters[i].type === m.type) {
          waiters[i].resolve(m)
          waiters.splice(i, 1)
          return
        }
      }
      queue.push(m)
    })
    const recv = type => {
      const idx = queue.findIndex(x => x.type === type)
      if (idx !== -1) return Promise.resolve(queue.splice(idx, 1)[0])
      return new Promise(resolve => waiters.push({ type, resolve }))
    }
    const open = new Promise(r => ws.addEventListener("open", r))
    const close = msg => ws.close()
    return { ws, recv, open, close,
      send: o => ws.send(JSON.stringify(o)),
    }
  }
}
`

test.describe("WAO Studio — signaling hub (real server, lib/hub.js protocol)", () => {
  test.beforeEach(async ({ page }) => {
    await page.addInitScript(helperSrc)
    await page.goto("/")
    await page.waitForLoadState("networkidle", { timeout: 60000 })
  })

  test("WebSocket connects and the server sends registered id", async ({
    page,
  }) => {
    const result = await page.evaluate(async url => {
      const s = window.__hub.mksock(url)
      await s.open
      const reg = await s.recv("registered")
      s.close()
      return { id: reg.id, isString: typeof reg.id === "string" }
    }, HUB_URL)
    expect(result.isString).toBe(true)
    expect(result.id.length).toBeGreaterThan(0)
  })

  test("register + sus returns a non-empty SU list", async ({ page }) => {
    const result = await page.evaluate(async url => {
      const a = window.__hub.mksock(url)
      const b = window.__hub.mksock(url)
      await a.open
      await b.open
      await a.recv("registered")
      await b.recv("registered")
      a.send({ type: "register" })
      // Server has no ack for register; small wait then query.
      await new Promise(r => setTimeout(r, 200))
      b.send({ type: "sus" })
      const sus = await b.recv("sus")
      a.close()
      b.close()
      return { count: sus.ids?.length ?? 0 }
    }, HUB_URL)
    expect(result.count).toBeGreaterThan(0)
  })

  test("offer relay: client→SU through the hub", async ({ page }) => {
    const result = await page.evaluate(async url => {
      const su = window.__hub.mksock(url)
      const client = window.__hub.mksock(url)
      await Promise.all([su.open, client.open])
      await Promise.all([su.recv("registered"), client.recv("registered")])
      su.send({ type: "register" })
      await new Promise(r => setTimeout(r, 200))
      client.send({ type: "sus" })
      const sus = await client.recv("sus")
      client.send({
        type: "offer",
        offer: { sdp: "fake-offer" },
        su: sus.ids[0],
      })
      const offerMsg = await su.recv("offer")
      su.close()
      client.close()
      return { offerSdp: offerMsg.offer?.sdp, fromId: offerMsg.id }
    }, HUB_URL)
    expect(result.offerSdp).toBe("fake-offer")
    expect(typeof result.fromId).toBe("string")
  })

  test("answer relay: SU→client through the hub", async ({ page }) => {
    const result = await page.evaluate(async url => {
      const su = window.__hub.mksock(url)
      const client = window.__hub.mksock(url)
      await Promise.all([su.open, client.open])
      const clientReg = await client.recv("registered")
      await su.recv("registered")
      su.send({ type: "register" })
      await new Promise(r => setTimeout(r, 200))
      client.send({ type: "sus" })
      const sus = await client.recv("sus")
      client.send({
        type: "offer",
        offer: { sdp: "x" },
        su: sus.ids[0],
      })
      const offerMsg = await su.recv("offer")
      su.send({
        type: "answer",
        answer: { sdp: "fake-answer" },
        client: offerMsg.id,
        clientId: clientReg.id,
      })
      const ans = await client.recv("answer")
      su.close()
      client.close()
      return { answerSdp: ans.answer?.sdp }
    }, HUB_URL)
    expect(result.answerSdp).toBe("fake-answer")
  })

  test("list target=hb returns at least one entry", async ({ page }) => {
    const result = await page.evaluate(async url => {
      const s = window.__hub.mksock(url)
      await s.open
      await s.recv("registered")
      s.send({ type: "list", target: "hb" })
      const list = await s.recv("list")
      s.close()
      return { count: list.hb?.length ?? 0 }
    }, HUB_URL)
    expect(result.count).toBeGreaterThanOrEqual(1)
  })

  test("disconnect closes the socket", async ({ page }) => {
    const closed = await page.evaluate(async url => {
      const s = window.__hub.mksock(url)
      await s.open
      return await new Promise(r => {
        s.ws.addEventListener("close", () => r(true))
        s.ws.close()
        setTimeout(() => r(false), 2000)
      })
    }, HUB_URL)
    expect(closed).toBe(true)
  })
})
