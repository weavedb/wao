import { describe, it, expect, beforeAll, afterAll } from "vitest"
import { startWrangler } from "./helpers.js"
import { resolve } from "node:path"

let server

beforeAll(async () => {
  server = await startWrangler({ port: 8789, cwd: resolve(import.meta.dirname, "..") })
})

afterAll(async () => {
  if (server) await server.kill()
})

describe("API", () => {
  it("GET / returns HTML with explorer title", async () => {
    const res = await fetch(`${server.baseUrl}/`)
    expect(res.status).toBe(200)
    const html = await res.text()
    expect(html).toContain("WAO SCAN")
    expect(html.toLowerCase()).toContain("devnet")
  })

  it("GET /ar returns network info", async () => {
    const res = await fetch(`${server.baseUrl}/ar`)
    expect(res.status).toBe(200)
    const data = await res.json()
    expect(data).toHaveProperty("network")
    expect(data).toHaveProperty("height")
  })

  it("POST /ar/graphql returns blocks", async () => {
    const res = await fetch(`${server.baseUrl}/ar/graphql`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({
        query: `{ blocks(first: 3, sort: HEIGHT_DESC) { edges { node { id height timestamp } } } }`,
      }),
    })
    expect(res.status).toBe(200)
    const json = await res.json()
    expect(json.data.blocks.edges).toBeInstanceOf(Array)
  })

  it("POST /ar/graphql returns transactions", async () => {
    const res = await fetch(`${server.baseUrl}/ar/graphql`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({
        query: `{ transactions(first: 3, sort: HEIGHT_DESC) { edges { node { id owner { address } tags { name value } } } } }`,
      }),
    })
    expect(res.status).toBe(200)
    const json = await res.json()
    expect(json.data.transactions.edges).toBeInstanceOf(Array)
  })

  it("GET /su returns process list", async () => {
    const res = await fetch(`${server.baseUrl}/su`)
    expect(res.status).toBe(200)
    const data = await res.json()
    expect(data).toBeDefined()
  })

  it("GET /cu returns compute unit info", async () => {
    const res = await fetch(`${server.baseUrl}/cu`)
    expect(res.status).toBe(200)
    const data = await res.json()
    expect(data).toHaveProperty("address")
  })
})
