/**
 * Integration test: full devnet lifecycle via wrangler dev.
 *
 * - Starts wrangler dev on a free port
 * - Spawns a process via MU (raw DataItem)
 * - Installs a Lua handler via Eval message
 * - Sends an action message, checks CU result
 * - Queries GQL for blocks, transactions, process
 * - Verifies D1-backed indexed queries
 * - WebSocket broadcast on mutations
 */
import { describe, it, expect, beforeAll, afterAll } from "vitest"
import { startWrangler } from "./helpers.js"
import { resolve } from "node:path"
import { createData, ArweaveSigner } from "arbundles"
import { acc, su as suAcc } from "../../src/accounts.js"

const PORT = 8790
let server
let BASE
let WS_URL

function tag(name, value) {
  return { name, value: String(value) }
}

async function postMU(jwk, { data = "", tags = [], target } = {}) {
  const signer = new ArweaveSigner(jwk)
  const item = createData(data, signer, { tags, target: target || "" })
  await item.sign(signer)
  const res = await fetch(`${BASE}/mu`, {
    method: "POST",
    headers: { "Content-Type": "application/octet-stream" },
    body: item.getRaw(),
  })
  const json = await res.json().catch(() => ({}))
  return { status: res.status, json, id: json.id || item.id }
}

async function gql(query) {
  const res = await fetch(`${BASE}/ar/graphql`, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ query }),
  })
  return res.json()
}

// Module known to be pre-registered in armem-base.js
const MODULE = "ISShJH1ij-hPPt9St5UFFr_8Ys3Kj5cyg7zrMGt7H9s"

let SCHEDULER
let processId

beforeAll(async () => {
  server = await startWrangler({
    port: PORT,
    cwd: resolve(import.meta.dirname, ".."),
  })
  BASE = server.baseUrl
  WS_URL = `ws://localhost:${PORT}/ws`

  // Get scheduler address
  const suInfo = await fetch(`${BASE}/su`).then(r => r.json())
  SCHEDULER = suInfo.Address
}, 180_000)

afterAll(async () => {
  if (server) await server.kill()
}, 15_000)

describe("Devnet flow", () => {
  it("should spawn a process via MU", async () => {
    const { status, json, id } = await postMU(acc[0].jwk, {
      data: "-- counter process",
      tags: [
        tag("Data-Protocol", "ao"),
        tag("Variant", "ao.TN.1"),
        tag("Type", "Process"),
        tag("Module", MODULE),
        tag("Scheduler", SCHEDULER),
        tag("Name", "TestCounter"),
        tag("Authority", suAcc.addr),
        tag("Content-Type", "text/plain"),
      ],
    })
    expect(status).toBe(200)
    expect(id).toBeTruthy()
    processId = id
  }, 30_000)

  it("should install a handler via Eval message", async () => {
    const src = `local count = 0
Handlers.add("Inc", Handlers.utils.hasMatchingTag("Action", "Inc"), function(msg)
  count = count + 1
  msg.reply({ Data = tostring(count) })
end)
Handlers.add("Get", Handlers.utils.hasMatchingTag("Action", "Get"), function(msg)
  msg.reply({ Data = tostring(count) })
end)
return "handlers installed"`

    const { status, id } = await postMU(acc[0].jwk, {
      data: src,
      target: processId,
      tags: [
        tag("Data-Protocol", "ao"),
        tag("Variant", "ao.TN.1"),
        tag("Type", "Message"),
        tag("Action", "Eval"),
        tag("Content-Type", "text/plain"),
      ],
    })
    expect(status).toBe(200)
    expect(id).toBeTruthy()
  }, 30_000)

  it("should send Inc and get result from CU", async () => {
    const { status, id: msgId } = await postMU(acc[0].jwk, {
      data: "inc",
      target: processId,
      tags: [
        tag("Data-Protocol", "ao"),
        tag("Variant", "ao.TN.1"),
        tag("Type", "Message"),
        tag("Action", "Inc"),
        tag("Content-Type", "text/plain"),
      ],
    })
    expect(status).toBe(200)

    // Check result via CU
    const res = await fetch(
      `${BASE}/cu/result/${msgId}?process-id=${processId}`
    )
    expect(res.status).toBe(200)
    const result = await res.json()
    // The Inc handler replies with the count
    expect(result.Messages).toBeInstanceOf(Array)
    expect(result.Messages.length).toBeGreaterThan(0)
    expect(result.Messages[0].Data).toBe("1")
  }, 30_000)

  it("should increment again and return 2", async () => {
    const { status, id: msgId } = await postMU(acc[0].jwk, {
      data: "inc2",
      target: processId,
      tags: [
        tag("Data-Protocol", "ao"),
        tag("Variant", "ao.TN.1"),
        tag("Type", "Message"),
        tag("Action", "Inc"),
        tag("Content-Type", "text/plain"),
      ],
    })
    expect(status).toBe(200)

    const res = await fetch(
      `${BASE}/cu/result/${msgId}?process-id=${processId}`
    )
    const result = await res.json()
    expect(result.Messages[0].Data).toBe("2")
  }, 30_000)

  it("should list the process via SU", async () => {
    const res = await fetch(`${BASE}/su`)
    const data = await res.json()
    expect(data.Processes).toBeInstanceOf(Array)
    expect(data.Processes.length).toBeGreaterThan(0)
    // Our process should be listed
    const found = data.Processes.some(p => p === processId || p.id === processId)
    expect(found).toBe(true)
  }, 15_000)

  it("should return results list from CU", async () => {
    const res = await fetch(`${BASE}/cu/results/${processId}?limit=10`)
    expect(res.status).toBe(200)
    const data = await res.json()
    expect(data.edges).toBeInstanceOf(Array)
    // We sent 3 messages: Eval + 2x Inc
    expect(data.edges.length).toBeGreaterThanOrEqual(3)
  }, 15_000)

  it("should return blocks via GQL", async () => {
    const result = await gql(`{
      blocks(first: 10, sort: HEIGHT_DESC) {
        edges { node { id height timestamp } }
      }
    }`)
    expect(result.data.blocks.edges).toBeInstanceOf(Array)
    expect(result.data.blocks.edges.length).toBeGreaterThan(0)
  }, 15_000)

  it("should return transactions via GQL", async () => {
    const result = await gql(`{
      transactions(first: 20, sort: HEIGHT_DESC) {
        edges { node { id owner { address } tags { name value } } }
      }
    }`)
    const edges = result.data.transactions.edges
    expect(edges).toBeInstanceOf(Array)
    expect(edges.length).toBeGreaterThan(0)

    // Should include our process spawn (Type=Process)
    const processEdge = edges.find(e =>
      e.node.tags.some(t => t.name === "Type" && t.value === "Process")
    )
    expect(processEdge).toBeTruthy()
  }, 15_000)

  it("should filter transactions by tag via GQL", async () => {
    const result = await gql(`{
      transactions(
        first: 10,
        tags: [{ name: "Type", values: ["Process"] }]
      ) {
        edges { node { id tags { name value } } }
      }
    }`)
    const edges = result.data.transactions.edges
    expect(edges).toBeInstanceOf(Array)
    expect(edges.length).toBeGreaterThan(0)
    // All returned txs should have Type=Process
    for (const e of edges) {
      const typeTag = e.node.tags.find(t => t.name === "Type")
      expect(typeTag.value).toBe("Process")
    }
  }, 15_000)

  it("should filter transactions by owner via GQL", async () => {
    const result = await gql(`{
      transactions(
        first: 10,
        owners: ["${acc[0].addr}"]
      ) {
        edges { node { id owner { address } } }
      }
    }`)
    const edges = result.data.transactions.edges
    expect(edges).toBeInstanceOf(Array)
    // All returned txs should be owned by acc[0]
    for (const e of edges) {
      expect(e.node.owner.address).toBe(acc[0].addr)
    }
  }, 15_000)

  it("should get process state from CU", async () => {
    const res = await fetch(`${BASE}/cu/state/${processId}`)
    expect(res.status).toBe(200)
    const buf = await res.arrayBuffer()
    // WASM memory should be non-trivial size
    expect(buf.byteLength).toBeGreaterThan(0)
  }, 30_000)
})

describe("WebSocket broadcast", () => {
  it("should connect to /ws endpoint", async () => {
    const ws = new WebSocket(WS_URL)
    await new Promise((resolve, reject) => {
      ws.onopen = resolve
      ws.onerror = reject
      setTimeout(() => reject(new Error("WS connect timeout")), 5000)
    })
    expect(ws.readyState).toBe(WebSocket.OPEN)
    ws.close()
  }, 10_000)

  it("should receive broadcast when spawning a process", async () => {
    const ws = new WebSocket(WS_URL)
    await new Promise((resolve, reject) => {
      ws.onopen = resolve
      ws.onerror = reject
      setTimeout(() => reject(new Error("WS connect timeout")), 5000)
    })

    // Collect messages
    const messages = []
    ws.onmessage = (e) => {
      messages.push(JSON.parse(e.data))
    }

    // Spawn a process — should trigger a broadcast
    const { status, id } = await postMU(acc[0].jwk, {
      data: "-- ws test process",
      tags: [
        tag("Data-Protocol", "ao"),
        tag("Variant", "ao.TN.1"),
        tag("Type", "Process"),
        tag("Module", MODULE),
        tag("Scheduler", SCHEDULER),
        tag("Name", "WS-Test"),
        tag("Authority", suAcc.addr),
        tag("Content-Type", "text/plain"),
      ],
    })
    expect(status).toBe(200)

    // Wait for broadcast
    await new Promise(r => setTimeout(r, 500))

    expect(messages.length).toBeGreaterThan(0)
    const txMsg = messages.find(m => m.type === "tx" && m.id === id)
    expect(txMsg).toBeTruthy()
    expect(txMsg.device).toBe("mu")
    expect(txMsg.timestamp).toBeTypeOf("number")

    ws.close()
  }, 30_000)

  it("should receive broadcast when sending a message", async () => {
    const ws = new WebSocket(WS_URL)
    await new Promise((resolve, reject) => {
      ws.onopen = resolve
      ws.onerror = reject
      setTimeout(() => reject(new Error("WS connect timeout")), 5000)
    })

    const messages = []
    ws.onmessage = (e) => {
      messages.push(JSON.parse(e.data))
    }

    // Send a message to the existing process
    const { status, id: msgId } = await postMU(acc[0].jwk, {
      data: "ws-msg-test",
      target: processId,
      tags: [
        tag("Data-Protocol", "ao"),
        tag("Variant", "ao.TN.1"),
        tag("Type", "Message"),
        tag("Action", "Inc"),
        tag("Content-Type", "text/plain"),
      ],
    })
    expect(status).toBe(200)

    await new Promise(r => setTimeout(r, 500))

    expect(messages.length).toBeGreaterThan(0)
    const txMsg = messages.find(m => m.type === "tx" && m.id === msgId)
    expect(txMsg).toBeTruthy()
    expect(txMsg.device).toBe("mu")

    ws.close()
  }, 30_000)

  it("should handle multiple concurrent WS clients", async () => {
    const ws1 = new WebSocket(WS_URL)
    const ws2 = new WebSocket(WS_URL)
    await Promise.all([
      new Promise((resolve, reject) => {
        ws1.onopen = resolve
        ws1.onerror = reject
        setTimeout(() => reject(new Error("WS1 timeout")), 5000)
      }),
      new Promise((resolve, reject) => {
        ws2.onopen = resolve
        ws2.onerror = reject
        setTimeout(() => reject(new Error("WS2 timeout")), 5000)
      }),
    ])

    const msgs1 = []
    const msgs2 = []
    ws1.onmessage = (e) => msgs1.push(JSON.parse(e.data))
    ws2.onmessage = (e) => msgs2.push(JSON.parse(e.data))

    // Send a message
    const { status, id: msgId } = await postMU(acc[0].jwk, {
      data: "multi-client-test",
      target: processId,
      tags: [
        tag("Data-Protocol", "ao"),
        tag("Variant", "ao.TN.1"),
        tag("Type", "Message"),
        tag("Action", "Inc"),
        tag("Content-Type", "text/plain"),
      ],
    })
    expect(status).toBe(200)

    await new Promise(r => setTimeout(r, 500))

    // Both clients should receive the same broadcast
    expect(msgs1.find(m => m.id === msgId)).toBeTruthy()
    expect(msgs2.find(m => m.id === msgId)).toBeTruthy()

    ws1.close()
    ws2.close()
  }, 30_000)
})
