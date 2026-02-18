/**
 * Comprehensive scheduler test: Scheduler-Location and Scheduler-Transfer
 * via MU POST, GQL queries, and scheduler move (transfer + re-register).
 *
 * Usage: npx vitest run devnet/test/scheduler.test.js
 */
import { describe, it, expect, beforeAll, afterAll } from "vitest"
import { startWrangler } from "./helpers.js"
import { resolve } from "node:path"
import { createData, ArweaveSigner } from "arbundles"
import { acc, su as suAcc } from "../../src/accounts.js"

const PORT = 8793
let server
let BASE
let SCHEDULER

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

async function gql(query, variables = {}) {
  const res = await fetch(`${BASE}/ar/graphql`, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ query, variables }),
  })
  return res.json()
}

// ---- Setup ----

beforeAll(async () => {
  server = await startWrangler({
    port: PORT,
    cwd: resolve(import.meta.dirname, ".."),
  })
  BASE = server.baseUrl

  const suInfo = await fetch(`${BASE}/su`).then(r => r.json())
  SCHEDULER = suInfo.Address

  // Small delay to let genesis txs settle
  await new Promise(r => setTimeout(r, 500))
}, 300_000)

afterAll(async () => {
  if (server) await server.kill()
}, 15_000)

// ============================================================
// Genesis Scheduler-Location
// ============================================================

describe("Genesis Scheduler-Location", () => {
  it("exists on startup (seeded by initSync)", async () => {
    const result = await gql(`
      query($tags: [TagFilter!]) {
        transactions(first: 10, tags: $tags) {
          edges { node { id owner { address } tags { name value } } }
        }
      }
    `, { tags: [{ name: "Type", values: ["Scheduler-Location"] }] })
    expect(result.error).toBeUndefined()
    const edges = result.data.transactions.edges
    expect(edges.length).toBeGreaterThan(0)

    // At least one should be from the SU account (genesis)
    const genesis = edges.find(e => e.node.owner.address === SCHEDULER)
    expect(genesis).toBeTruthy()
  })

  it("genesis Scheduler-Location has correct tags", async () => {
    const result = await gql(`
      query($tags: [TagFilter!]) {
        transactions(first: 1, tags: $tags, owners: ["${SCHEDULER}"]) {
          edges { node { id tags { name value } } }
        }
      }
    `, { tags: [{ name: "Type", values: ["Scheduler-Location"] }] })
    const node = result.data.transactions.edges[0].node
    const tagsMap = Object.fromEntries(node.tags.map(t => [t.name, t.value]))

    expect(tagsMap["Type"]).toBe("Scheduler-Location")
    expect(tagsMap["Data-Protocol"]).toBe("ao")
    expect(tagsMap["Url"]).toBeTruthy()
    expect(tagsMap["Time-To-Live"]).toBeTruthy()
    expect(Number(tagsMap["Time-To-Live"])).toBeGreaterThan(0)
  })

  it("genesis Scheduler-Location Url points to the SU", async () => {
    const result = await gql(`
      query($tags: [TagFilter!]) {
        transactions(first: 1, tags: $tags, owners: ["${SCHEDULER}"]) {
          edges { node { tags { name value } } }
        }
      }
    `, { tags: [{ name: "Type", values: ["Scheduler-Location"] }] })
    const tagsMap = Object.fromEntries(
      result.data.transactions.edges[0].node.tags.map(t => [t.name, t.value])
    )
    // The Url should be a valid URL string
    expect(tagsMap["Url"]).toMatch(/^https?:\/\//)
  })
})

// ============================================================
// POST Scheduler-Location via MU
// ============================================================

describe("POST Scheduler-Location via MU", () => {
  let schedLocId

  it("accepts a new Scheduler-Location DataItem", async () => {
    const { status, json, id } = await postMU(acc[0].jwk, {
      tags: [
        tag("Data-Protocol", "ao"),
        tag("Variant", "ao.TN.1"),
        tag("Type", "Scheduler-Location"),
        tag("Url", "http://localhost:9999/su-new"),
        tag("Time-To-Live", "3600000"),
      ],
    })
    expect(status).toBe(200)
    expect(id).toBeTruthy()
    schedLocId = id
  })

  it("posted Scheduler-Location appears in GQL", async () => {
    // Wait for block inclusion
    await new Promise(r => setTimeout(r, 1000))

    const result = await gql(`{
      transactions(first: 50, ids: ["${schedLocId}"]) {
        edges { node { id tags { name value } owner { address } } }
      }
    }`)
    expect(result.error).toBeUndefined()
    const edges = result.data.transactions.edges
    expect(edges.length).toBe(1)
    const tagsMap = Object.fromEntries(edges[0].node.tags.map(t => [t.name, t.value]))
    expect(tagsMap["Type"]).toBe("Scheduler-Location")
    expect(tagsMap["Url"]).toBe("http://localhost:9999/su-new")
    expect(tagsMap["Time-To-Live"]).toBe("3600000")
  })

  it("GQL filter by Type=Scheduler-Location returns the new tx", async () => {
    const result = await gql(`
      query($tags: [TagFilter!]) {
        transactions(first: 50, tags: $tags) {
          edges { node { id } }
        }
      }
    `, { tags: [{ name: "Type", values: ["Scheduler-Location"] }] })
    const ids = result.data.transactions.edges.map(e => e.node.id)
    expect(ids).toContain(schedLocId)
  })

  it("posted Scheduler-Location has correct owner", async () => {
    const result = await gql(`{
      transactions(ids: ["${schedLocId}"]) {
        edges { node { owner { address } } }
      }
    }`)
    expect(result.data.transactions.edges[0].node.owner.address).toBe(acc[0].addr)
  })
})

// ============================================================
// POST Scheduler-Transfer via MU
// ============================================================

describe("POST Scheduler-Transfer via MU", () => {
  let schedTransId

  it("accepts a Scheduler-Transfer DataItem", async () => {
    const { status, json, id } = await postMU(acc[0].jwk, {
      tags: [
        tag("Data-Protocol", "ao"),
        tag("Variant", "ao.TN.1"),
        tag("Type", "Scheduler-Transfer"),
        tag("New-Scheduler", acc[1].addr),
      ],
    })
    expect(status).toBe(200)
    expect(id).toBeTruthy()
    schedTransId = id
  })

  it("posted Scheduler-Transfer appears in GQL", async () => {
    await new Promise(r => setTimeout(r, 1000))

    const result = await gql(`{
      transactions(ids: ["${schedTransId}"]) {
        edges { node { id tags { name value } owner { address } } }
      }
    }`)
    expect(result.error).toBeUndefined()
    const edges = result.data.transactions.edges
    expect(edges.length).toBe(1)
    const tagsMap = Object.fromEntries(edges[0].node.tags.map(t => [t.name, t.value]))
    expect(tagsMap["Type"]).toBe("Scheduler-Transfer")
    expect(tagsMap["New-Scheduler"]).toBe(acc[1].addr)
  })

  it("GQL filter by Type=Scheduler-Transfer returns the new tx", async () => {
    const result = await gql(`
      query($tags: [TagFilter!]) {
        transactions(first: 50, tags: $tags) {
          edges { node { id tags { name value } } }
        }
      }
    `, { tags: [{ name: "Type", values: ["Scheduler-Transfer"] }] })
    const edges = result.data.transactions.edges
    expect(edges.length).toBeGreaterThan(0)
    const ids = edges.map(e => e.node.id)
    expect(ids).toContain(schedTransId)
    // All returned should have Type=Scheduler-Transfer
    for (const e of edges) {
      const typeTag = e.node.tags.find(t => t.name === "Type")
      expect(typeTag.value).toBe("Scheduler-Transfer")
    }
  })
})

// ============================================================
// Combined GQL queries for both scheduler types
// ============================================================

describe("GQL combined scheduler queries", () => {
  it("filter by both Scheduler-Location and Scheduler-Transfer", async () => {
    const result = await gql(`
      query($tags: [TagFilter!]) {
        transactions(first: 50, tags: $tags, sort: HEIGHT_DESC) {
          edges { node { id tags { name value } } }
        }
      }
    `, { tags: [{ name: "Type", values: ["Scheduler-Location", "Scheduler-Transfer"] }] })
    expect(result.error).toBeUndefined()
    const edges = result.data.transactions.edges
    // Should have both genesis + our posted ones
    expect(edges.length).toBeGreaterThanOrEqual(3)

    const types = new Set()
    for (const e of edges) {
      const typeTag = e.node.tags.find(t => t.name === "Type")
      expect(["Scheduler-Location", "Scheduler-Transfer"]).toContain(typeTag.value)
      types.add(typeTag.value)
    }
    // Both types should be represented
    expect(types.has("Scheduler-Location")).toBe(true)
    expect(types.has("Scheduler-Transfer")).toBe(true)
  })

  it("scheduler txs have correct Data-Protocol=ao tag", async () => {
    const result = await gql(`
      query($tags: [TagFilter!]) {
        transactions(first: 50, tags: $tags) {
          edges { node { tags { name value } } }
        }
      }
    `, { tags: [{ name: "Type", values: ["Scheduler-Location", "Scheduler-Transfer"] }] })
    for (const e of result.data.transactions.edges) {
      const dp = e.node.tags.find(t => t.name === "Data-Protocol")
      expect(dp).toBeTruthy()
      expect(dp.value).toBe("ao")
    }
  })

  it("pagination works for scheduler queries", async () => {
    const page1 = await gql(`
      query($tags: [TagFilter!]) {
        transactions(first: 1, tags: $tags, sort: HEIGHT_DESC) {
          pageInfo { hasNextPage }
          edges { cursor node { id } }
        }
      }
    `, { tags: [{ name: "Type", values: ["Scheduler-Location", "Scheduler-Transfer"] }] })
    const edges1 = page1.data.transactions.edges
    expect(edges1.length).toBe(1)

    if (page1.data.transactions.pageInfo.hasNextPage) {
      const cursor = edges1[0].cursor
      const page2 = await gql(`
        query($tags: [TagFilter!]) {
          transactions(first: 1, tags: $tags, sort: HEIGHT_DESC, after: "${cursor}") {
            edges { node { id } }
          }
        }
      `, { tags: [{ name: "Type", values: ["Scheduler-Location", "Scheduler-Transfer"] }] })
      expect(page2.data.transactions.edges.length).toBe(1)
      expect(page2.data.transactions.edges[0].node.id).not.toBe(edges1[0].node.id)
    }
  })

  it("filter scheduler txs by owner", async () => {
    const result = await gql(`
      query($tags: [TagFilter!]) {
        transactions(first: 50, tags: $tags, owners: ["${acc[0].addr}"]) {
          edges { node { id owner { address } tags { name value } } }
        }
      }
    `, { tags: [{ name: "Type", values: ["Scheduler-Location", "Scheduler-Transfer"] }] })
    expect(result.error).toBeUndefined()
    for (const e of result.data.transactions.edges) {
      expect(e.node.owner.address).toBe(acc[0].addr)
    }
  })
})

// ============================================================
// Move scheduler: Transfer + new Location
// ============================================================

describe("Move scheduler (Transfer then re-register)", () => {
  let transferId
  let newLocationId
  const NEW_URL = "http://localhost:7777/su-moved"

  it("Step 1: post Scheduler-Transfer from acc[0] to acc[1]", async () => {
    const { status, id } = await postMU(acc[0].jwk, {
      tags: [
        tag("Data-Protocol", "ao"),
        tag("Variant", "ao.TN.1"),
        tag("Type", "Scheduler-Transfer"),
        tag("New-Scheduler", acc[1].addr),
      ],
    })
    expect(status).toBe(200)
    transferId = id
  })

  it("Step 2: post new Scheduler-Location from acc[1] with new URL", async () => {
    const { status, id } = await postMU(acc[1].jwk, {
      tags: [
        tag("Data-Protocol", "ao"),
        tag("Variant", "ao.TN.1"),
        tag("Type", "Scheduler-Location"),
        tag("Url", NEW_URL),
        tag("Time-To-Live", "7200000"),
      ],
    })
    expect(status).toBe(200)
    newLocationId = id
  })

  it("both transfer and new location appear in GQL", async () => {
    await new Promise(r => setTimeout(r, 1000))

    const result = await gql(`{
      transactions(ids: ["${transferId}", "${newLocationId}"]) {
        edges { node { id tags { name value } owner { address } } }
      }
    }`)
    expect(result.error).toBeUndefined()
    const edges = result.data.transactions.edges
    expect(edges.length).toBe(2)

    const ids = edges.map(e => e.node.id)
    expect(ids).toContain(transferId)
    expect(ids).toContain(newLocationId)
  })

  it("transfer tx has correct owner and New-Scheduler tag", async () => {
    const result = await gql(`{
      transactions(ids: ["${transferId}"]) {
        edges { node { owner { address } tags { name value } } }
      }
    }`)
    const node = result.data.transactions.edges[0].node
    expect(node.owner.address).toBe(acc[0].addr)
    const tagsMap = Object.fromEntries(node.tags.map(t => [t.name, t.value]))
    expect(tagsMap["Type"]).toBe("Scheduler-Transfer")
    expect(tagsMap["New-Scheduler"]).toBe(acc[1].addr)
  })

  it("new location tx has correct owner and Url tag", async () => {
    const result = await gql(`{
      transactions(ids: ["${newLocationId}"]) {
        edges { node { owner { address } tags { name value } } }
      }
    }`)
    const node = result.data.transactions.edges[0].node
    expect(node.owner.address).toBe(acc[1].addr)
    const tagsMap = Object.fromEntries(node.tags.map(t => [t.name, t.value]))
    expect(tagsMap["Type"]).toBe("Scheduler-Location")
    expect(tagsMap["Url"]).toBe(NEW_URL)
    expect(tagsMap["Time-To-Live"]).toBe("7200000")
  })

  it("latest Scheduler-Location (by height DESC) is the moved one", async () => {
    // Query by acc[1] as owner — they now own the scheduler
    const result = await gql(`
      query($tags: [TagFilter!]) {
        transactions(first: 1, tags: $tags, owners: ["${acc[1].addr}"], sort: HEIGHT_DESC) {
          edges { node { id tags { name value } } }
        }
      }
    `, { tags: [{ name: "Type", values: ["Scheduler-Location"] }] })
    const edges = result.data.transactions.edges
    expect(edges.length).toBe(1)
    const tagsMap = Object.fromEntries(edges[0].node.tags.map(t => [t.name, t.value]))
    expect(tagsMap["Url"]).toBe(NEW_URL)
  })

  it("chronological order: transfer before new location", async () => {
    const result = await gql(`
      query($tags: [TagFilter!]) {
        transactions(first: 50, tags: $tags, sort: HEIGHT_ASC) {
          edges { node { id block { height } tags { name value } } }
        }
      }
    `, { tags: [{ name: "Type", values: ["Scheduler-Location", "Scheduler-Transfer"] }] })
    const edges = result.data.transactions.edges
    const transferIdx = edges.findIndex(e => e.node.id === transferId)
    const newLocIdx = edges.findIndex(e => e.node.id === newLocationId)
    expect(transferIdx).toBeGreaterThanOrEqual(0)
    expect(newLocIdx).toBeGreaterThanOrEqual(0)
    // Transfer should come before or at the same height as the new location
    expect(transferIdx).toBeLessThanOrEqual(newLocIdx)
  })
})

// ============================================================
// Scheduler discovery via GQL
// ============================================================

describe("Scheduler discovery via GQL", () => {
  it("can discover SU URL by querying Scheduler-Location for an address", async () => {
    // Discover the genesis scheduler URL
    const result = await gql(`
      query($tags: [TagFilter!]) {
        transactions(first: 1, tags: $tags, owners: ["${SCHEDULER}"], sort: HEIGHT_DESC) {
          edges { node { tags { name value } } }
        }
      }
    `, { tags: [{ name: "Type", values: ["Scheduler-Location"] }] })
    const edges = result.data.transactions.edges
    expect(edges.length).toBe(1)
    const tagsMap = Object.fromEntries(edges[0].node.tags.map(t => [t.name, t.value]))
    expect(tagsMap["Url"]).toBeTruthy()
    // Should be a valid URL
    expect(tagsMap["Url"]).toMatch(/^https?:\/\//)
  })

  it("can discover multiple schedulers", async () => {
    // All Scheduler-Location txs, grouped by owner
    const result = await gql(`
      query($tags: [TagFilter!]) {
        transactions(first: 100, tags: $tags, sort: HEIGHT_DESC) {
          edges { node { owner { address } tags { name value } } }
        }
      }
    `, { tags: [{ name: "Type", values: ["Scheduler-Location"] }] })
    const edges = result.data.transactions.edges
    // Should have at least 2 distinct scheduler owners (genesis + acc[0] or acc[1])
    const owners = new Set(edges.map(e => e.node.owner.address))
    expect(owners.size).toBeGreaterThanOrEqual(2)
  })
})

// ============================================================
// WebSocket broadcast for scheduler txs
// ============================================================

describe("WebSocket broadcast for scheduler txs", () => {
  it("broadcasts Scheduler-Location via WS", async () => {
    const ws = new WebSocket(`ws://localhost:${PORT}/ws`)
    await new Promise((resolve, reject) => {
      ws.onopen = resolve
      ws.onerror = reject
      setTimeout(() => reject(new Error("WS connect timeout")), 5000)
    })

    const messages = []
    ws.onmessage = (e) => messages.push(JSON.parse(e.data))

    const { status, id } = await postMU(acc[0].jwk, {
      tags: [
        tag("Data-Protocol", "ao"),
        tag("Variant", "ao.TN.1"),
        tag("Type", "Scheduler-Location"),
        tag("Url", "http://localhost:5555/su-ws-test"),
        tag("Time-To-Live", "1800000"),
      ],
    })
    expect(status).toBe(200)

    await new Promise(r => setTimeout(r, 1500))

    const txMsg = messages.find(m => m.type === "tx" && m.id === id)
    expect(txMsg).toBeTruthy()
    expect(typeof txMsg.timestamp).toBe("number")

    ws.close()
  }, 15_000)

  it("broadcasts Scheduler-Transfer via WS", async () => {
    const ws = new WebSocket(`ws://localhost:${PORT}/ws`)
    await new Promise((resolve, reject) => {
      ws.onopen = resolve
      ws.onerror = reject
      setTimeout(() => reject(new Error("WS connect timeout")), 5000)
    })

    const messages = []
    ws.onmessage = (e) => messages.push(JSON.parse(e.data))

    const { status, id } = await postMU(acc[0].jwk, {
      tags: [
        tag("Data-Protocol", "ao"),
        tag("Variant", "ao.TN.1"),
        tag("Type", "Scheduler-Transfer"),
        tag("New-Scheduler", acc[1].addr),
      ],
    })
    expect(status).toBe(200)

    await new Promise(r => setTimeout(r, 1500))

    const txMsg = messages.find(m => m.type === "tx" && m.id === id)
    expect(txMsg).toBeTruthy()

    ws.close()
  }, 15_000)
})

// ============================================================
// Error cases
// ============================================================

describe("Scheduler error cases", () => {
  it("rejects DataItem with unknown Type", async () => {
    const { status } = await postMU(acc[0].jwk, {
      tags: [
        tag("Data-Protocol", "ao"),
        tag("Type", "Unknown-Type"),
      ],
    })
    expect(status).toBe(400)
  })

  it("Scheduler-Location without Url tag still accepted (tag is optional at MU level)", async () => {
    // The MU just stores it — validation of Url presence is up to consumers
    const { status, id } = await postMU(acc[0].jwk, {
      tags: [
        tag("Data-Protocol", "ao"),
        tag("Variant", "ao.TN.1"),
        tag("Type", "Scheduler-Location"),
        tag("Time-To-Live", "60000"),
      ],
    })
    expect(status).toBe(200)
    expect(id).toBeTruthy()
  })
})
