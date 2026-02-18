import { Hono } from "hono"
import { cors } from "hono/cors"
import Adaptor from "../../src/adaptor-cf.js"
import { connect } from "../../src/aoconnect-cf.js"

// CU-only routes
const routes = {
  cu: {
    get: ["/", "/result/:mid", "/state/:pid", "/results/:pid", "/dry-run"],
    post: ["/result/:mid", "/dry-run", "/evaluate"],
  },
}

const app = new Hono()
app.use("*", cors())

// Forward all /cu requests to the CuDO singleton
for (const [device, methods] of Object.entries(routes)) {
  for (const [method, paths] of Object.entries(methods)) {
    for (const path of paths) {
      const honoPath = `/${device}${path === "/" ? "" : path}`
      const handler = async (c) => {
        const id = c.env.CU.idFromName("singleton")
        const stub = c.env.CU.get(id)
        const url = new URL(c.req.url)
        url.pathname = `/${device}${url.pathname.slice(`/${device}`.length)}`
        return stub.fetch(new Request(url.toString(), c.req.raw))
      }
      app.on(method.toUpperCase(), honoPath, handler)
      if (path === "/") {
        app.on(method.toUpperCase(), `/${device}/`, handler)
      }
    }
  }
}

app.get("/", (c) => c.json({ unit: "CU", status: "ok" }))

export default { fetch: app.fetch }

// --- CU Durable Object ---
const MAX_BODY_SIZE = 10 * 1024 * 1024

export class CuDO {
  constructor(state, env) {
    this.state = state
    this.env = env
    this.storage = state.storage
    this.adaptor = null
    this.app = null
  }

  async init() {
    if (this.adaptor) return

    const ar_url = this.env.AR_URL
    if (!ar_url) {
      throw new Error("AR_URL environment variable is required for standalone CU")
    }

    const aoconnect = null
    const { mem } = connect(aoconnect, {
      log: false,
      storage: this.storage,
      ar_url,
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
    })

    // Build internal Hono router
    const doApp = new Hono()
    doApp.use("*", cors())

    const self = this
    for (const [device, methods] of Object.entries(routes)) {
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
              return toResponse(c, data)
            } catch (e) {
              console.error(`CU DO route error [${device}${path}]:`, e)
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
    await this.init()
    return this.app.fetch(request)
  }
}

// --- Helpers ---
async function parseBody(c) {
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
