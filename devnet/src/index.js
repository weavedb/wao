import html from "../dist/index.html"
import { Hono } from "hono"
import { cors } from "hono/cors"
import Adaptor from "../../src/adaptor-cf.js"
import { connect } from "../../src/aoconnect-cf.js"
import { optAO } from "../../src/utils.js"

// --- Parse UNITS env var into a Set of enabled device names ---
function parseUnits(unitsStr) {
  if (!unitsStr) return null // null = all units enabled
  const units = new Set(unitsStr.split(",").map(u => u.trim().toLowerCase()))
  if (units.has("ar")) units.add("bd") // BD always follows AR
  return units
}

// --- Route definitions (mirrors server.js) ---
const routes = {
  ar: {
    get: [
      "/",
      "/wallet/:id/balance",
      "/mint/:id/:amount",
      "/tx/:id/offset",
      "/tx_anchor",
      "/mine",
      "/:id",
      "/price/:id",
    ],
    post: ["/graphql", "/:id"],
  },
  bd: {
    post: ["/tx"],
  },
  mu: {
    get: ["/"],
    post: ["/", "/monitor/:pid"],
    delete: ["/monitor/:pid"],
  },
  su: {
    get: ["/", "/timestamp", "/:pid"],
  },
  cu: {
    get: ["/", "/result/:mid", "/state/:pid", "/results/:pid", "/dry-run"],
    post: ["/result/:mid", "/dry-run", "/evaluate"],
  },
}

// --- Hono app (runs in Worker) ---
const app = new Hono()
app.use("*", cors())

// Register all routes, forwarding to the DevnetDO singleton
for (const [device, methods] of Object.entries(routes)) {
  for (const [method, paths] of Object.entries(methods)) {
    for (const path of paths) {
      const honoPath = `/${device}${path === "/" ? "" : path}`
      const handler = async (c) => {
        const id = c.env.DEVNET.idFromName("singleton")
        const stub = c.env.DEVNET.get(id)
        const url = new URL(c.req.url)
        url.pathname = `/${device}${url.pathname.slice(`/${device}`.length)}`
        return stub.fetch(new Request(url.toString(), c.req.raw))
      }
      app.on(method.toUpperCase(), honoPath, handler)
      // Also register with trailing slash so aoconnect clients work
      if (path === "/") {
        app.on(method.toUpperCase(), `/${device}/`, handler)
      }
    }
  }
}

// WebSocket upgrade — forward to DO
app.get("/ws", async (c) => {
  const upgradeHeader = c.req.header("Upgrade")
  if (!upgradeHeader || upgradeHeader !== "websocket") {
    return c.text("Expected Upgrade: websocket", 426)
  }
  const id = c.env.DEVNET.idFromName("singleton")
  const stub = c.env.DEVNET.get(id)
  return stub.fetch(c.req.raw)
})

// Root health check
app.get("/", (c) => c.html(html))

export default { fetch: app.fetch }

// --- Durable Object ---
const MAX_WS_CONNECTIONS = 1000
const MAX_BODY_SIZE = 10 * 1024 * 1024 // 10 MB

export class DevnetDO {
  constructor(state, env) {
    this.state = state
    this.env = env
    this.storage = state.storage
    this.adaptor = null
    this.app = null
    this.sockets = new Set()
  }

  broadcast(msg) {
    const data = JSON.stringify(msg)
    const dead = []
    for (const ws of this.sockets) {
      try {
        if (ws.readyState === 1) ws.send(data)
        else dead.push(ws)
      } catch { dead.push(ws) }
    }
    for (const ws of dead) this.sockets.delete(ws)
  }

  _evictOldestSocket() {
    // Set iteration order is insertion order — first is oldest
    const oldest = this.sockets.values().next().value
    if (oldest) {
      try { oldest.close(1008, "connection limit reached") } catch {}
      this.sockets.delete(oldest)
    }
  }

  async init(requestUrl) {
    if (this.adaptor) return

    if (this.env.WAO_WALLETS && !globalThis.__WAO_WALLETS__) {
      try {
        globalThis.__WAO_WALLETS__ = JSON.parse(this.env.WAO_WALLETS)
      } catch {}
    }

    // Auto-apply D1 schema if available
    if (this.env.DB) {
      try {
        await this.env.DB.exec(`
          CREATE TABLE IF NOT EXISTS blocks (
            id TEXT PRIMARY KEY, height INTEGER NOT NULL UNIQUE,
            timestamp INTEGER NOT NULL, previous TEXT NOT NULL DEFAULT ''
          );
          CREATE INDEX IF NOT EXISTS idx_blocks_height ON blocks(height);
          CREATE TABLE IF NOT EXISTS txs (
            id TEXT PRIMARY KEY, block_id TEXT NOT NULL, block_height INTEGER NOT NULL,
            owner TEXT NOT NULL DEFAULT '', recipient TEXT NOT NULL DEFAULT '',
            anchor TEXT NOT NULL DEFAULT '', signature TEXT,
            data_size TEXT NOT NULL DEFAULT '0', data_type TEXT NOT NULL DEFAULT '',
            bundle_id TEXT, parent_id TEXT
          );
          CREATE INDEX IF NOT EXISTS idx_txs_owner ON txs(owner);
          CREATE INDEX IF NOT EXISTS idx_txs_recipient ON txs(recipient);
          CREATE INDEX IF NOT EXISTS idx_txs_block_height ON txs(block_height);
          CREATE TABLE IF NOT EXISTS tx_tags (
            tx_id TEXT NOT NULL, name TEXT NOT NULL, value TEXT NOT NULL
          );
          CREATE INDEX IF NOT EXISTS idx_tx_tags_name_value ON tx_tags(name, value);
          CREATE INDEX IF NOT EXISTS idx_tx_tags_tx_id ON tx_tags(tx_id);
          CREATE TABLE IF NOT EXISTS addrmap (address TEXT PRIMARY KEY, key TEXT NOT NULL DEFAULT '');
          CREATE TABLE IF NOT EXISTS modules (name TEXT PRIMARY KEY, wasm_id TEXT NOT NULL);
          CREATE TABLE IF NOT EXISTS wasms (
            id TEXT PRIMARY KEY, format TEXT NOT NULL DEFAULT 'wasm64-unknown-emscripten-draft_2024_02_15',
            file TEXT, variant TEXT
          );
        `)
      } catch {}
    }

    // Determine enabled units from env
    this._enabledUnits = parseUnits(this.env.UNITS)

    // Derive SU_URL from env or first request so Scheduler-Location tx is seeded
    const baseUrl = requestUrl ? new URL(requestUrl).origin : "http://localhost:8787"
    const variant = this.env.NETWORK || "ao.DN.1"
    const aoconnect = { SU_URL: this.env.SU_URL || `${baseUrl}/su` }
    const { mem } = connect(aoconnect, {
      log: false,
      variant,
      storage: this.storage,
      d1: this.env.DB || null,
      r2: this.env.BUCKET || null,
      kv: this.env.CACHE || null,
    })

    this.adaptor = new Adaptor({
      aoconnect: mem,
      log: false,
      storage: this.storage,
      d1: this.env.DB || null,
      r2: this.env.BUCKET || null,
      kv: this.env.CACHE || null,
      network: this.env.NETWORK || "ao.DN.1",
    })

    // Build internal Hono router for the DO
    const doApp = new Hono()
    doApp.use("*", cors())

    const self = this
    const enabled = this._enabledUnits
    for (const [device, methods] of Object.entries(routes)) {
      if (enabled && !enabled.has(device)) continue
      for (const [method, paths] of Object.entries(methods)) {
        for (const path of paths) {
          const honoPath = `/${device}${path === "/" ? "" : path}`
          const doHandler = async (c) => {
            const req = {
              path,
              device,
              body: await parseBody(c),
              headers: Object.fromEntries(c.req.raw.headers.entries()),
              method: c.req.method,
              params: c.req.param(),
              query: c.req.query(),
            }
            try {
              let data
              await self.adaptor.get(req, (d) => { data = d })
              const resp = toResponse(c, data)
              // Broadcast on mutations (POST to ar, mu)
              if (method === "post" && (device === "ar" || device === "mu")) {
                const txId = data?.json?.id || data?.json?.data?.transactions?.edges?.[0]?.node?.id
                if (txId && path !== "/graphql") {
                  self.broadcast({ type: "tx", id: txId, device, timestamp: Date.now() })
                }
              }
              return resp
            } catch (e) {
              console.error(`DO route error [${device}${path}]:`, e)
              return c.json({ error: e.message || "internal error" }, 500)
            }
          }
          doApp.on(method.toUpperCase(), honoPath, doHandler)
          if (path === "/") {
            doApp.on(method.toUpperCase(), `/${device}/`, doHandler)
          }
        }
      }
    }

    this.app = doApp
  }

  async fetch(request) {
    await this.init(request.url)

    // Handle WebSocket upgrade
    const url = new URL(request.url)
    if (url.pathname === "/ws" && request.headers.get("Upgrade") === "websocket") {
      // Evict oldest connection if at capacity
      while (this.sockets.size >= MAX_WS_CONNECTIONS) {
        this._evictOldestSocket()
      }
      const pair = new WebSocketPair()
      const [client, server] = Object.values(pair)
      server.accept()
      this.sockets.add(server)
      server.addEventListener("close", () => this.sockets.delete(server))
      server.addEventListener("error", () => this.sockets.delete(server))
      return new Response(null, { status: 101, webSocket: client })
    }

    return this.app.fetch(request)
  }
}

// --- Helpers ---
async function parseBody(c) {
  // Reject oversized bodies early
  const cl = parseInt(c.req.header("content-length") || "0", 10)
  if (cl > MAX_BODY_SIZE) {
    throw new Error(`Request body too large (${cl} bytes, max ${MAX_BODY_SIZE})`)
  }

  const ct = c.req.header("content-type") || ""
  if (ct.includes("application/json")) {
    try {
      return await c.req.json()
    } catch {
      return {}
    }
  }
  // Raw body for MU / BD endpoints
  try {
    const buf = await c.req.arrayBuffer()
    if (buf.byteLength > MAX_BODY_SIZE) {
      throw new Error(`Request body too large (${buf.byteLength} bytes)`)
    }
    return new Uint8Array(buf)
  } catch (e) {
    if (e.message?.includes("too large")) throw e
    return null
  }
}

function toResponse(c, data) {
  if (!data) return c.json({ error: "no response" }, 500)
  const status = data.status || 200
  if (data.error) {
    return c.json({ error: data.error }, status)
  } else if (data.json) {
    return c.json(data.json, status)
  } else if (data.send !== undefined && data.send !== null) {
    if (Buffer.isBuffer(data.send) || data.send instanceof Uint8Array) {
      const headers = data.headers || {}
      return new Response(data.send, { status, headers })
    }
    return c.text(String(data.send), status)
  }
  return c.text("", status)
}
