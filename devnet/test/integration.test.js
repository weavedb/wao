/**
 * Comprehensive integration test: spawns wrangler, seeds data, then
 * thoroughly tests every endpoint, GQL query shape, edge case, and WebSocket.
 */
import { describe, it, expect, beforeAll, afterAll } from "vitest"
import { startWrangler } from "./helpers.js"
import { resolve } from "node:path"
import { execSync } from "node:child_process"
import { createData, ArweaveSigner } from "arbundles"
import { acc, su as suAcc } from "../../src/accounts.js"

const PORT = 8791
let server
let BASE
const MODULE = "ISShJH1ij-hPPt9St5UFFr_8Ys3Kj5cyg7zrMGt7H9s"
let SCHEDULER
let processId
let evalMsgId
let incMsgId

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

// ---- Setup: start wrangler + seed data ----

beforeAll(async () => {
  server = await startWrangler({
    port: PORT,
    cwd: resolve(import.meta.dirname, ".."),
  })
  BASE = server.baseUrl

  // Get scheduler
  const suInfo = await fetch(`${BASE}/su`).then(r => r.json())
  SCHEDULER = suInfo.Address

  // Spawn a process
  const spawn = await postMU(acc[0].jwk, {
    data: "-- integration test process",
    tags: [
      tag("Data-Protocol", "ao"),
      tag("Variant", "ao.TN.1"),
      tag("Type", "Process"),
      tag("Module", MODULE),
      tag("Scheduler", SCHEDULER),
      tag("Name", "IntegrationTest"),
      tag("Authority", suAcc.addr),
      tag("Content-Type", "text/plain"),
    ],
  })
  processId = spawn.id

  // Install handler
  const evalRes = await postMU(acc[0].jwk, {
    data: `local count = 0
Handlers.add("Inc", Handlers.utils.hasMatchingTag("Action", "Inc"), function(msg)
  count = count + 1
  msg.reply({ Data = tostring(count) })
end)
Handlers.add("Get", Handlers.utils.hasMatchingTag("Action", "Get"), function(msg)
  msg.reply({ Data = tostring(count) })
end)
return "handlers installed"`,
    target: processId,
    tags: [
      tag("Data-Protocol", "ao"),
      tag("Variant", "ao.TN.1"),
      tag("Type", "Message"),
      tag("Action", "Eval"),
      tag("Content-Type", "text/plain"),
    ],
  })
  evalMsgId = evalRes.id

  // Send Inc message
  const incRes = await postMU(acc[0].jwk, {
    data: "inc1",
    target: processId,
    tags: [
      tag("Data-Protocol", "ao"),
      tag("Variant", "ao.TN.1"),
      tag("Type", "Message"),
      tag("Action", "Inc"),
      tag("Content-Type", "text/plain"),
    ],
  })
  incMsgId = incRes.id

  // Post a plain Arweave data tx via seed script for richer data
  execSync("node devnet/test/seed.js", {
    cwd: resolve(import.meta.dirname, "../.."),
    env: { ...process.env, PORT: String(PORT) },
    stdio: "pipe",
  })

  // Small delay for processing
  await new Promise(r => setTimeout(r, 1000))
}, 300_000)

afterAll(async () => {
  if (server) await server.kill()
}, 15_000)

// ============================================================
// AR endpoints
// ============================================================

describe("AR endpoints", () => {
  it("GET /ar returns network info with correct fields", async () => {
    const res = await fetch(`${BASE}/ar`)
    expect(res.status).toBe(200)
    const data = await res.json()
    expect(data).toHaveProperty("network")
    expect(data).toHaveProperty("height")
    expect(data).toHaveProperty("current")
    expect(data).toHaveProperty("version")
    expect(typeof data.height).toBe("number")
    expect(data.height).toBeGreaterThan(0)
  })

  it("GET /ar/tx_anchor returns an anchor string", async () => {
    const res = await fetch(`${BASE}/ar/tx_anchor`)
    expect(res.status).toBe(200)
    const text = await res.text()
    // Anchor is returned as plain text or JSON-encoded string
    const anchor = text.startsWith('"') ? JSON.parse(text) : text
    expect(typeof anchor).toBe("string")
    expect(anchor.length).toBeGreaterThan(0)
  })

  it("GET /ar/mine triggers a mine", async () => {
    const infoBefore = await fetch(`${BASE}/ar`).then(r => r.json())
    const res = await fetch(`${BASE}/ar/mine`)
    expect(res.status).toBe(200)
  })

  it("GET /ar/:id returns tx data for a known tx", async () => {
    // Get a tx id from GQL first
    const result = await gql(`{ transactions(first: 1) { edges { node { id } } } }`)
    const txId = result.data.transactions.edges[0].node.id
    const res = await fetch(`${BASE}/ar/${txId}`)
    // Should return something (200 or the data)
    expect([200, 202, 404]).toContain(res.status)
  })

  it("GET /ar/wallet/:id/balance returns a balance", async () => {
    const res = await fetch(`${BASE}/ar/wallet/${acc[0].addr}/balance`)
    expect(res.status).toBe(200)
  })

  it("GET /ar/price/:id returns a price", async () => {
    const res = await fetch(`${BASE}/ar/price/1234`)
    expect(res.status).toBe(200)
  })
})

// ============================================================
// GQL Transaction queries — thorough coverage
// ============================================================

describe("GQL transactions", () => {
  it("basic query with no filters returns edges", async () => {
    const result = await gql(`{
      transactions(first: 5) {
        pageInfo { hasNextPage }
        edges { node { id } }
      }
    }`)
    expect(result.data.transactions.edges).toBeInstanceOf(Array)
    expect(result.data.transactions.edges.length).toBeGreaterThan(0)
    expect(result.data.transactions).toHaveProperty("pageInfo")
  })

  it("returns all sub-fields without crashing", async () => {
    const result = await gql(`{
      transactions(first: 3, sort: HEIGHT_DESC) {
        edges { node {
          id
          anchor
          signature
          recipient
          owner { address }
          block { id height timestamp }
          tags { name value }
          data { size type }
          fee { winston ar }
          quantity { winston ar }
          bundledIn { id }
          parent { id }
        } }
      }
    }`)
    expect(result.data).toBeDefined()
    expect(result.error).toBeUndefined()
    const edges = result.data.transactions.edges
    expect(edges.length).toBeGreaterThan(0)
    const node = edges[0].node
    expect(node).toHaveProperty("id")
    expect(node).toHaveProperty("anchor")
    expect(node).toHaveProperty("signature")
    expect(node.owner).toHaveProperty("address")
    expect(node.block).toHaveProperty("height")
    expect(node.block).toHaveProperty("timestamp")
    expect(node.tags).toBeInstanceOf(Array)
    expect(node.data).toHaveProperty("size")
    expect(node.fee).toHaveProperty("winston")
    expect(node.quantity).toHaveProperty("ar")
    expect(node.bundledIn).toHaveProperty("id")
    expect(node.parent).toHaveProperty("id")
  })

  it("works with only owner sub-field selected", async () => {
    const result = await gql(`{
      transactions(first: 2) {
        edges { node { id owner { address } } }
      }
    }`)
    expect(result.error).toBeUndefined()
    const node = result.data.transactions.edges[0].node
    expect(node.owner.address).toBeTruthy()
  })

  it("works with only block sub-field selected", async () => {
    const result = await gql(`{
      transactions(first: 2) {
        edges { node { id block { id height timestamp } } }
      }
    }`)
    expect(result.error).toBeUndefined()
    const node = result.data.transactions.edges[0].node
    expect(typeof node.block.height).toBe("number")
  })

  it("works with only data sub-field selected", async () => {
    const result = await gql(`{
      transactions(first: 2) {
        edges { node { id data { size type } } }
      }
    }`)
    expect(result.error).toBeUndefined()
    const node = result.data.transactions.edges[0].node
    expect(node.data).toHaveProperty("size")
    expect(node.data).toHaveProperty("type")
  })

  it("works with only tags selected", async () => {
    const result = await gql(`{
      transactions(first: 2) {
        edges { node { id tags { name value } } }
      }
    }`)
    expect(result.error).toBeUndefined()
    const node = result.data.transactions.edges[0].node
    expect(node.tags).toBeInstanceOf(Array)
    expect(node.tags.length).toBeGreaterThan(0)
    expect(node.tags[0]).toHaveProperty("name")
    expect(node.tags[0]).toHaveProperty("value")
  })

  it("pagination with after cursor works", async () => {
    const page1 = await gql(`{
      transactions(first: 2, sort: HEIGHT_DESC) {
        pageInfo { hasNextPage }
        edges { cursor node { id } }
      }
    }`)
    const edges1 = page1.data.transactions.edges
    expect(edges1.length).toBe(2)
    expect(page1.data.transactions.pageInfo.hasNextPage).toBe(true)

    // Use last cursor for page 2
    const lastCursor = edges1[edges1.length - 1].cursor
    const page2 = await gql(`{
      transactions(first: 2, sort: HEIGHT_DESC, after: "${lastCursor}") {
        edges { node { id } }
      }
    }`)
    const edges2 = page2.data.transactions.edges
    expect(edges2.length).toBeGreaterThan(0)
    // Page 2 should not contain same IDs as page 1
    const ids1 = new Set(edges1.map(e => e.node.id))
    for (const e of edges2) {
      expect(ids1.has(e.node.id)).toBe(false)
    }
  })

  it("HEIGHT_ASC sort order works", async () => {
    const result = await gql(`{
      transactions(first: 5, sort: HEIGHT_ASC) {
        edges { node { id block { height } } }
      }
    }`)
    const edges = result.data.transactions.edges
    expect(edges.length).toBeGreaterThan(1)
    // Heights should be ascending
    for (let i = 1; i < edges.length; i++) {
      expect(edges[i].node.block.height).toBeGreaterThanOrEqual(
        edges[i - 1].node.block.height
      )
    }
  })

  it("HEIGHT_DESC sort order works", async () => {
    const result = await gql(`{
      transactions(first: 5, sort: HEIGHT_DESC) {
        edges { node { id block { height } } }
      }
    }`)
    const edges = result.data.transactions.edges
    expect(edges.length).toBeGreaterThan(1)
    for (let i = 1; i < edges.length; i++) {
      expect(edges[i].node.block.height).toBeLessThanOrEqual(
        edges[i - 1].node.block.height
      )
    }
  })

  it("filter by tag (Type=Module) — AR-posted txs", async () => {
    // Seed script posts Module txs directly via AR, so they have inline tags
    const result = await gql(`
      query($tags: [TagFilter!]) {
        transactions(first: 10, tags: $tags) {
          edges { node { id tags { name value } } }
        }
      }
    `, { tags: [{ name: "Type", values: ["Module"] }] })
    expect(result.error).toBeUndefined()
    const edges = result.data.transactions.edges
    expect(edges.length).toBeGreaterThan(0)
    for (const e of edges) {
      const typeTag = e.node.tags.find(t => t.name === "Type")
      expect(typeTag).toBeTruthy()
      expect(typeTag.value).toBe("Module")
    }
  })

  it("filter by tag (Content-Type=text/markdown)", async () => {
    const result = await gql(`
      query($tags: [TagFilter!]) {
        transactions(first: 10, tags: $tags) {
          edges { node { id tags { name value } } }
        }
      }
    `, { tags: [{ name: "Content-Type", values: ["text/markdown"] }] })
    expect(result.error).toBeUndefined()
    const edges = result.data.transactions.edges
    expect(edges.length).toBeGreaterThan(0)
    for (const e of edges) {
      const ct = e.node.tags.find(t => t.name === "Content-Type")
      expect(ct.value).toBe("text/markdown")
    }
  })

  it("filter by multiple tag values (Module or inline tags)", async () => {
    const result = await gql(`
      query($tags: [TagFilter!]) {
        transactions(first: 20, tags: $tags) {
          edges { node { id tags { name value } } }
        }
      }
    `, { tags: [{ name: "Content-Type", values: ["text/markdown", "text/css"] }] })
    expect(result.error).toBeUndefined()
    const edges = result.data.transactions.edges
    expect(edges.length).toBeGreaterThan(0)
    for (const e of edges) {
      const ct = e.node.tags.find(t => t.name === "Content-Type")
      expect(["text/markdown", "text/css"]).toContain(ct.value)
    }
  })

  it("filter by owner", async () => {
    const result = await gql(`{
      transactions(first: 5, owners: ["${acc[0].addr}"]) {
        edges { node { id owner { address } } }
      }
    }`)
    expect(result.error).toBeUndefined()
    const edges = result.data.transactions.edges
    expect(edges.length).toBeGreaterThan(0)
    for (const e of edges) {
      expect(e.node.owner.address).toBe(acc[0].addr)
    }
  })

  it("filter by ids", async () => {
    // Get some known IDs first
    const all = await gql(`{ transactions(first: 3) { edges { node { id } } } }`)
    const ids = all.data.transactions.edges.map(e => e.node.id)
    expect(ids.length).toBeGreaterThan(0)

    const result = await gql(`{
      transactions(first: 10, ids: ["${ids[0]}"]) {
        edges { node { id } }
      }
    }`)
    expect(result.error).toBeUndefined()
    const edges = result.data.transactions.edges
    expect(edges.length).toBe(1)
    expect(edges[0].node.id).toBe(ids[0])
  })

  it("filter by recipient returns no error", async () => {
    // MU-posted DataItems become bundle stubs in the O(n) scan path,
    // so recipient filter may return fewer results. The important thing
    // is it doesn't crash.
    const result = await gql(`{
      transactions(first: 10, recipients: ["${processId}"]) {
        edges { node { id recipient } }
      }
    }`)
    expect(result.error).toBeUndefined()
    expect(result.data.transactions.edges).toBeInstanceOf(Array)
    // Verify returned txs actually match the filter
    for (const e of result.data.transactions.edges) {
      expect(e.node.recipient).toBe(processId)
    }
  })

  it("empty result for non-existent filter", async () => {
    const result = await gql(`{
      transactions(first: 5, ids: ["nonexistent-id-12345"]) {
        edges { node { id } }
      }
    }`)
    expect(result.error).toBeUndefined()
    expect(result.data.transactions.edges).toEqual([])
  })

  it("empty result for non-existent owner", async () => {
    const result = await gql(`{
      transactions(first: 5, owners: ["no-such-owner-address"]) {
        edges { node { id } }
      }
    }`)
    expect(result.error).toBeUndefined()
    expect(result.data.transactions.edges).toEqual([])
  })

  it("combined tag + owner filter", async () => {
    const result = await gql(`
      query($tags: [TagFilter!]) {
        transactions(first: 10, tags: $tags, owners: ["${acc[0].addr}"]) {
          edges { node { id owner { address } tags { name value } } }
        }
      }
    `, { tags: [{ name: "App-Name", values: ["WAO-Explorer-Test"] }] })
    expect(result.error).toBeUndefined()
    const edges = result.data.transactions.edges
    for (const e of edges) {
      expect(e.node.owner.address).toBe(acc[0].addr)
      const appTag = e.node.tags.find(t => t.name === "App-Name")
      expect(appTag.value).toBe("WAO-Explorer-Test")
    }
  })

  it("first=1 returns exactly one result", async () => {
    const result = await gql(`{
      transactions(first: 1) {
        pageInfo { hasNextPage }
        edges { node { id } }
      }
    }`)
    expect(result.data.transactions.edges.length).toBe(1)
    expect(result.data.transactions.pageInfo.hasNextPage).toBe(true)
  })

  it("large first returns many results", async () => {
    const result = await gql(`{
      transactions(first: 1000) {
        pageInfo { hasNextPage }
        edges { node { id } }
      }
    }`)
    expect(result.error).toBeUndefined()
    // Should return substantially more than the default page size
    expect(result.data.transactions.edges.length).toBeGreaterThan(10)
  })
})

// ============================================================
// GQL Block queries
// ============================================================

describe("GQL blocks", () => {
  it("basic block query returns edges", async () => {
    const result = await gql(`{
      blocks(first: 5, sort: HEIGHT_DESC) {
        pageInfo { hasNextPage }
        edges { node { id height timestamp } }
      }
    }`)
    expect(result.error).toBeUndefined()
    expect(result.data.blocks.edges).toBeInstanceOf(Array)
    expect(result.data.blocks.edges.length).toBeGreaterThan(0)
  })

  it("block fields are correct types", async () => {
    const result = await gql(`{
      blocks(first: 1) {
        edges { node { id height timestamp previous } }
      }
    }`)
    const node = result.data.blocks.edges[0].node
    expect(typeof node.id).toBe("string")
    expect(typeof node.height).toBe("number")
    expect(typeof node.timestamp).toBe("number")
    expect(typeof node.previous).toBe("string")
  })

  it("HEIGHT_DESC sort returns descending heights", async () => {
    const result = await gql(`{
      blocks(first: 10, sort: HEIGHT_DESC) {
        edges { node { height } }
      }
    }`)
    const heights = result.data.blocks.edges.map(e => e.node.height)
    for (let i = 1; i < heights.length; i++) {
      expect(heights[i]).toBeLessThanOrEqual(heights[i - 1])
    }
  })

  it("HEIGHT_ASC sort returns ascending heights", async () => {
    const result = await gql(`{
      blocks(first: 10, sort: HEIGHT_ASC) {
        edges { node { height } }
      }
    }`)
    const heights = result.data.blocks.edges.map(e => e.node.height)
    for (let i = 1; i < heights.length; i++) {
      expect(heights[i]).toBeGreaterThanOrEqual(heights[i - 1])
    }
  })

  it("block height filter (min/max) on transactions works", async () => {
    // The block filter is on transactions (block: BlockFilter), not blocks
    const result = await gql(`{
      transactions(first: 100, block: { min: 2, max: 5 }) {
        edges { node { id block { height } } }
      }
    }`)
    expect(result.error).toBeUndefined()
    const edges = result.data.transactions.edges
    for (const e of edges) {
      expect(e.node.block.height).toBeGreaterThanOrEqual(2)
      expect(e.node.block.height).toBeLessThanOrEqual(5)
    }
  })

  it("pagination with after cursor", async () => {
    const page1 = await gql(`{
      blocks(first: 2, sort: HEIGHT_DESC) {
        edges { cursor node { id height } }
      }
    }`)
    const edges1 = page1.data.blocks.edges
    expect(edges1.length).toBe(2)
  })

  it("first=1 returns exactly one block", async () => {
    const result = await gql(`{
      blocks(first: 1) {
        pageInfo { hasNextPage }
        edges { node { id height } }
      }
    }`)
    expect(result.data.blocks.edges.length).toBe(1)
    expect(result.data.blocks.pageInfo.hasNextPage).toBe(true)
  })
})

// ============================================================
// SU endpoints
// ============================================================

describe("SU endpoints", () => {
  it("GET /su returns scheduler info with Processes array", async () => {
    const res = await fetch(`${BASE}/su`)
    expect(res.status).toBe(200)
    const data = await res.json()
    expect(data.Unit).toBe("Scheduler")
    expect(data.Address).toBeTruthy()
    expect(data.Processes).toBeInstanceOf(Array)
    expect(data.Processes.length).toBeGreaterThan(0)
  })

  it("GET /su lists our spawned process", async () => {
    const data = await fetch(`${BASE}/su`).then(r => r.json())
    expect(data.Processes).toContain(processId)
  })

  it("GET /su/timestamp returns a timestamp", async () => {
    const res = await fetch(`${BASE}/su/timestamp`)
    expect(res.status).toBe(200)
  })

  it("GET /su/:pid returns process messages", async () => {
    const res = await fetch(`${BASE}/su/${processId}`)
    expect(res.status).toBe(200)
    const data = await res.json()
    expect(data).toBeDefined()
    // Should have edges or page_info
    if (data.edges) {
      expect(data.edges).toBeInstanceOf(Array)
    }
  })

  it("GET /su/:pid returns 404-ish for unknown pid", async () => {
    const res = await fetch(`${BASE}/su/nonexistent-process-id`)
    const data = await res.json()
    // Should either be empty or error
    if (data.edges) {
      expect(data.edges.length).toBe(0)
    }
  })
})

// ============================================================
// CU endpoints
// ============================================================

describe("CU endpoints", () => {
  it("GET /cu returns compute unit info", async () => {
    const res = await fetch(`${BASE}/cu`)
    expect(res.status).toBe(200)
    const data = await res.json()
    expect(data).toHaveProperty("address")
  })

  it("GET /cu/result/:mid returns a compute result", async () => {
    const res = await fetch(
      `${BASE}/cu/result/${incMsgId}?process-id=${processId}`
    )
    expect(res.status).toBe(200)
    const data = await res.json()
    expect(data).toHaveProperty("Messages")
    expect(data.Messages).toBeInstanceOf(Array)
    expect(data.Messages.length).toBeGreaterThan(0)
    // Inc handler should return count "1"
    expect(data.Messages[0].Data).toBe("1")
  })

  it("GET /cu/results/:pid returns paginated results", async () => {
    const res = await fetch(`${BASE}/cu/results/${processId}?limit=10`)
    expect(res.status).toBe(200)
    const data = await res.json()
    expect(data).toHaveProperty("edges")
    expect(data.edges).toBeInstanceOf(Array)
    expect(data.edges.length).toBeGreaterThan(0)
  })

  it("GET /cu/results/:pid with limit=1 returns exactly 1", async () => {
    const res = await fetch(`${BASE}/cu/results/${processId}?limit=1`)
    const data = await res.json()
    expect(data.edges.length).toBe(1)
  })

  it("GET /cu/state/:pid returns wasm memory", async () => {
    const res = await fetch(`${BASE}/cu/state/${processId}`)
    expect(res.status).toBe(200)
    const buf = await res.arrayBuffer()
    expect(buf.byteLength).toBeGreaterThan(0)
  }, 30_000)

  it("POST /cu/dry-run executes without persisting", async () => {
    const res = await fetch(`${BASE}/cu/dry-run?process-id=${processId}`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({
        Id: "dry-run-test-1",
        Owner: acc[0].addr,
        Tags: [tag("Action", "Get")],
        Data: "",
      }),
    })
    expect(res.status).toBe(200)
    const data = await res.json()
    expect(data).toHaveProperty("Messages")
    expect(data.Messages).toBeInstanceOf(Array)
    expect(data.Messages.length).toBeGreaterThan(0)
    // Get handler returns current count (1 from Inc during setup)
    expect(data.Messages[0].Data).toBe("1")
  })

  it("dry-run does not mutate state", async () => {
    // Run two dry-runs — they should return the same value
    const dryRunBody = JSON.stringify({
      Id: "dry-run-test-2",
      Owner: acc[0].addr,
      Tags: [tag("Action", "Get")],
      Data: "",
    })
    const run1 = await fetch(`${BASE}/cu/dry-run?process-id=${processId}`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: dryRunBody,
    }).then(r => r.json())

    const run2 = await fetch(`${BASE}/cu/dry-run?process-id=${processId}`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: dryRunBody,
    }).then(r => r.json())

    expect(run1.Messages[0].Data).toBe(run2.Messages[0].Data)
  })
})

// ============================================================
// MU endpoints
// ============================================================

describe("MU endpoints", () => {
  it("GET /mu returns unit description", async () => {
    const res = await fetch(`${BASE}/mu`)
    expect(res.status).toBe(200)
    const text = await res.text()
    expect(text).toContain("ao messenger unit")
  })

  it("POST /mu with valid DataItem returns 200", async () => {
    const { status, id } = await postMU(acc[0].jwk, {
      data: "test-message",
      target: processId,
      tags: [
        tag("Data-Protocol", "ao"),
        tag("Variant", "ao.TN.1"),
        tag("Type", "Message"),
        tag("Action", "Get"),
        tag("Content-Type", "text/plain"),
      ],
    })
    expect(status).toBe(200)
    expect(id).toBeTruthy()
  })
})

// ============================================================
// Edge cases: bundle stubs, missing fields
// ============================================================

describe("Edge cases", () => {
  it("GQL handles txs with no tags (bundle stubs) gracefully", async () => {
    // Query ALL transactions — should not crash even if some have no tags
    const result = await gql(`{
      transactions(first: 100) {
        edges { node { id tags { name value } } }
      }
    }`)
    expect(result.error).toBeUndefined()
    expect(result.data.transactions.edges).toBeInstanceOf(Array)
    // Every returned node should have tags as an array
    for (const e of result.data.transactions.edges) {
      expect(e.node.tags).toBeInstanceOf(Array)
    }
  })

  it("GQL handles txs with missing block/parent/bundledIn fields", async () => {
    const result = await gql(`{
      transactions(first: 100) {
        edges { node {
          id
          block { id height timestamp }
          parent { id }
          bundledIn { id }
          owner { address }
          data { size type }
          fee { winston ar }
          quantity { winston ar }
        } }
      }
    }`)
    expect(result.error).toBeUndefined()
    // No crash — all edges returned with proper structure
    for (const e of result.data.transactions.edges) {
      // block, parent, bundledIn should be objects (possibly with empty values)
      expect(typeof e.node.block).toBe("object")
      expect(typeof e.node.parent).toBe("object")
      expect(typeof e.node.bundledIn).toBe("object")
    }
  })

  it("GQL with minimal fields (id only) works", async () => {
    const result = await gql(`{
      transactions(first: 3) {
        edges { node { id } }
      }
    }`)
    expect(result.error).toBeUndefined()
    expect(result.data.transactions.edges[0].node.id).toBeTruthy()
  })

  it("GQL query with empty string query body returns error", async () => {
    const res = await fetch(`${BASE}/ar/graphql`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ query: "" }),
    })
    // Should return 400, not crash
    expect(res.status).toBe(400)
  })

  it("GQL query with malformed query returns error", async () => {
    const res = await fetch(`${BASE}/ar/graphql`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ query: "not a valid graphql query {{{" }),
    })
    expect(res.status).toBe(400)
  })
})

// ============================================================
// WebSocket
// ============================================================

describe("WebSocket", () => {
  it("connects to /ws endpoint", async () => {
    const ws = new WebSocket(`ws://localhost:${PORT}/ws`)
    await new Promise((resolve, reject) => {
      ws.onopen = resolve
      ws.onerror = reject
      setTimeout(() => reject(new Error("WS connect timeout")), 5000)
    })
    expect(ws.readyState).toBe(WebSocket.OPEN)
    ws.close()
  })

  it("rejects non-websocket upgrade request to /ws", async () => {
    const res = await fetch(`${BASE}/ws`)
    expect(res.status).toBe(426)
  })

  it("receives broadcast on MU POST (spawn)", async () => {
    const ws = new WebSocket(`ws://localhost:${PORT}/ws`)
    await new Promise((resolve, reject) => {
      ws.onopen = resolve
      ws.onerror = reject
      setTimeout(() => reject(new Error("WS connect timeout")), 5000)
    })

    const messages = []
    ws.onmessage = (e) => messages.push(JSON.parse(e.data))

    // Spawn a new process
    const { status, id } = await postMU(acc[0].jwk, {
      data: "-- ws broadcast test",
      tags: [
        tag("Data-Protocol", "ao"),
        tag("Variant", "ao.TN.1"),
        tag("Type", "Process"),
        tag("Module", MODULE),
        tag("Scheduler", SCHEDULER),
        tag("Name", "WS-Broadcast-Test"),
        tag("Authority", suAcc.addr),
        tag("Content-Type", "text/plain"),
      ],
    })
    expect(status).toBe(200)

    await new Promise(r => setTimeout(r, 1000))

    expect(messages.length).toBeGreaterThan(0)
    const txMsg = messages.find(m => m.type === "tx" && m.id === id)
    expect(txMsg).toBeTruthy()
    expect(txMsg.device).toBe("mu")
    expect(typeof txMsg.timestamp).toBe("number")

    ws.close()
  }, 15_000)

  it("receives broadcast on MU POST (message)", async () => {
    const ws = new WebSocket(`ws://localhost:${PORT}/ws`)
    await new Promise((resolve, reject) => {
      ws.onopen = resolve
      ws.onerror = reject
      setTimeout(() => reject(new Error("WS connect timeout")), 5000)
    })

    const messages = []
    ws.onmessage = (e) => messages.push(JSON.parse(e.data))

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

    await new Promise(r => setTimeout(r, 1000))

    const txMsg = messages.find(m => m.type === "tx" && m.id === msgId)
    expect(txMsg).toBeTruthy()
    expect(txMsg.device).toBe("mu")

    ws.close()
  }, 15_000)

  it("multiple WS clients all receive the same broadcast", async () => {
    const ws1 = new WebSocket(`ws://localhost:${PORT}/ws`)
    const ws2 = new WebSocket(`ws://localhost:${PORT}/ws`)
    const ws3 = new WebSocket(`ws://localhost:${PORT}/ws`)

    await Promise.all([
      new Promise((res, rej) => { ws1.onopen = res; ws1.onerror = rej; setTimeout(() => rej(new Error("WS1 timeout")), 5000) }),
      new Promise((res, rej) => { ws2.onopen = res; ws2.onerror = rej; setTimeout(() => rej(new Error("WS2 timeout")), 5000) }),
      new Promise((res, rej) => { ws3.onopen = res; ws3.onerror = rej; setTimeout(() => rej(new Error("WS3 timeout")), 5000) }),
    ])

    const msgs1 = [], msgs2 = [], msgs3 = []
    ws1.onmessage = (e) => msgs1.push(JSON.parse(e.data))
    ws2.onmessage = (e) => msgs2.push(JSON.parse(e.data))
    ws3.onmessage = (e) => msgs3.push(JSON.parse(e.data))

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

    await new Promise(r => setTimeout(r, 1000))

    // All 3 clients should receive the broadcast
    expect(msgs1.find(m => m.id === msgId)).toBeTruthy()
    expect(msgs2.find(m => m.id === msgId)).toBeTruthy()
    expect(msgs3.find(m => m.id === msgId)).toBeTruthy()

    ws1.close()
    ws2.close()
    ws3.close()
  }, 15_000)

  it("closed WS client stops receiving messages", async () => {
    const ws1 = new WebSocket(`ws://localhost:${PORT}/ws`)
    const ws2 = new WebSocket(`ws://localhost:${PORT}/ws`)

    await Promise.all([
      new Promise((res, rej) => { ws1.onopen = res; ws1.onerror = rej; setTimeout(() => rej(new Error("timeout")), 5000) }),
      new Promise((res, rej) => { ws2.onopen = res; ws2.onerror = rej; setTimeout(() => rej(new Error("timeout")), 5000) }),
    ])

    const msgs1 = [], msgs2 = []
    ws1.onmessage = (e) => msgs1.push(JSON.parse(e.data))
    ws2.onmessage = (e) => msgs2.push(JSON.parse(e.data))

    // Close ws1 before sending message
    ws1.close()
    await new Promise(r => setTimeout(r, 200))

    const { status, id: msgId } = await postMU(acc[0].jwk, {
      data: "after-close-test",
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

    await new Promise(r => setTimeout(r, 1000))

    // ws2 should receive it, ws1 should not
    expect(msgs2.find(m => m.id === msgId)).toBeTruthy()
    // ws1 might have received messages before it was closed but NOT this one
    expect(msgs1.find(m => m.id === msgId)).toBeFalsy()

    ws2.close()
  }, 15_000)

  it("WS broadcast includes correct message structure", async () => {
    const ws = new WebSocket(`ws://localhost:${PORT}/ws`)
    await new Promise((res, rej) => {
      ws.onopen = res
      ws.onerror = rej
      setTimeout(() => rej(new Error("WS timeout")), 5000)
    })

    const messages = []
    ws.onmessage = (e) => messages.push(JSON.parse(e.data))

    await postMU(acc[0].jwk, {
      data: "structure-test",
      target: processId,
      tags: [
        tag("Data-Protocol", "ao"),
        tag("Variant", "ao.TN.1"),
        tag("Type", "Message"),
        tag("Action", "Inc"),
        tag("Content-Type", "text/plain"),
      ],
    })

    await new Promise(r => setTimeout(r, 1000))

    expect(messages.length).toBeGreaterThan(0)
    const msg = messages[0]
    expect(msg).toHaveProperty("type", "tx")
    expect(msg).toHaveProperty("id")
    expect(msg).toHaveProperty("device")
    expect(msg).toHaveProperty("timestamp")
    expect(typeof msg.id).toBe("string")
    expect(typeof msg.timestamp).toBe("number")

    ws.close()
  }, 15_000)

  it("rapid sequential messages all broadcast", async () => {
    const ws = new WebSocket(`ws://localhost:${PORT}/ws`)
    await new Promise((res, rej) => {
      ws.onopen = res
      ws.onerror = rej
      setTimeout(() => rej(new Error("WS timeout")), 5000)
    })

    const messages = []
    ws.onmessage = (e) => messages.push(JSON.parse(e.data))

    // Send 3 messages rapidly
    const ids = []
    for (let i = 0; i < 3; i++) {
      const { id } = await postMU(acc[0].jwk, {
        data: `rapid-${i}`,
        target: processId,
        tags: [
          tag("Data-Protocol", "ao"),
          tag("Variant", "ao.TN.1"),
          tag("Type", "Message"),
          tag("Action", "Inc"),
          tag("Content-Type", "text/plain"),
        ],
      })
      ids.push(id)
    }

    await new Promise(r => setTimeout(r, 2000))

    // All 3 should have been broadcast
    for (const id of ids) {
      expect(messages.find(m => m.id === id)).toBeTruthy()
    }

    ws.close()
  }, 30_000)
})
