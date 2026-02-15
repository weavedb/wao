import { createServer } from "node:http"
import { readFileSync, readdirSync, statSync, watch, existsSync } from "node:fs"
import { join, extname, relative } from "node:path"

const PORT = 3333
const ROOT = join(import.meta.dirname, "..")
const DIST = join(import.meta.dirname, "dist")

const MIME = {
  ".html": "text/html",
  ".js": "application/javascript",
  ".css": "text/css",
  ".json": "application/json",
  ".svg": "image/svg+xml",
  ".png": "image/png",
}

// --- SSE clients ---
const clients = new Set()

function broadcast(data) {
  const payload = `event: progress\ndata: ${JSON.stringify(data)}\n\n`
  for (const res of clients) {
    try { res.write(payload) } catch { clients.delete(res) }
  }
}

// --- File watching with debounce ---
let debounceTimer = null

function readProgress() {
  try {
    const tasksPath = join(ROOT, "tasks.json")
    if (!existsSync(tasksPath)) return null
    const raw = readFileSync(tasksPath, "utf8")
    return JSON.parse(raw)
  } catch {
    return null
  }
}

function onFileChange() {
  clearTimeout(debounceTimer)
  debounceTimer = setTimeout(() => {
    const data = readProgress()
    if (data) broadcast(data)
  }, 150)
}

function watchFile(filename) {
  const filepath = join(ROOT, filename)
  try {
    const w = watch(filepath, { persistent: false }, onFileChange)
    w.on("error", () => {
      setTimeout(() => watchFile(filename), 2000)
    })
    return w
  } catch {
    setTimeout(() => watchFile(filename), 2000)
    return null
  }
}

watchFile("tasks.json")
watchFile("plan.md")

// --- File scanner ---
const IGNORE_DIRS = new Set(["node_modules", ".git", "dashboard", "HyperBEAM", ".claude", "docs", "target", "dist", "_build"])

function scanFiles(dir, files = []) {
  try {
    const entries = readdirSync(dir, { withFileTypes: true })
    for (const e of entries) {
      if (e.name.startsWith(".") && e.name !== ".mcp.json") continue
      const full = join(dir, e.name)
      if (e.isDirectory()) {
        if (IGNORE_DIRS.has(e.name)) continue
        scanFiles(full, files)
      } else {
        try {
          const s = statSync(full)
          files.push({ path: relative(ROOT, full), size: s.size })
        } catch {}
      }
    }
  } catch {}
  return files
}

// --- Static file serving (production dist/) ---
function serveStatic(req, res) {
  const url = req.url === "/" ? "/index.html" : req.url.split("?")[0]
  const filepath = join(DIST, url)
  try {
    const data = readFileSync(filepath)
    const ext = extname(filepath)
    res.writeHead(200, { "Content-Type": MIME[ext] || "application/octet-stream" })
    res.end(data)
  } catch {
    try {
      const index = readFileSync(join(DIST, "index.html"))
      res.writeHead(200, { "Content-Type": "text/html" })
      res.end(index)
    } catch {
      res.writeHead(404)
      res.end("Not found")
    }
  }
}

// --- HTTP server ---
const server = createServer((req, res) => {
  res.setHeader("Access-Control-Allow-Origin", "*")
  res.setHeader("Access-Control-Allow-Methods", "GET, OPTIONS")
  res.setHeader("Access-Control-Allow-Headers", "Content-Type")

  if (req.method === "OPTIONS") {
    res.writeHead(204)
    return res.end()
  }

  const url = req.url.split("?")[0]

  // GET /api/progress
  if (url === "/api/progress") {
    const data = readProgress()
    if (!data) {
      res.writeHead(200, { "Content-Type": "application/json" })
      return res.end(JSON.stringify({ feature: null, tasks: [], tracks: [] }))
    }
    res.writeHead(200, { "Content-Type": "application/json" })
    return res.end(JSON.stringify(data))
  }

  // GET /api/plan
  if (url === "/api/plan") {
    try {
      const planPath = join(ROOT, "plan.md")
      if (!existsSync(planPath)) {
        res.writeHead(200, { "Content-Type": "application/json" })
        return res.end(JSON.stringify({ content: null }))
      }
      const content = readFileSync(planPath, "utf8")
      res.writeHead(200, { "Content-Type": "application/json" })
      return res.end(JSON.stringify({ content }))
    } catch {
      res.writeHead(200, { "Content-Type": "application/json" })
      return res.end(JSON.stringify({ content: null }))
    }
  }

  // GET /api/files
  if (url === "/api/files") {
    const files = scanFiles(ROOT)
    res.writeHead(200, { "Content-Type": "application/json" })
    return res.end(JSON.stringify({ files }))
  }

  // GET /api/file?path=...
  if (url === "/api/file") {
    const params = new URL(req.url, "http://localhost").searchParams
    const filePath = params.get("path")
    if (!filePath) {
      res.writeHead(400, { "Content-Type": "application/json" })
      return res.end(JSON.stringify({ error: "Missing path parameter" }))
    }
    const fullPath = join(ROOT, filePath)
    if (!fullPath.startsWith(ROOT + "/")) {
      res.writeHead(403, { "Content-Type": "application/json" })
      return res.end(JSON.stringify({ error: "Access denied" }))
    }
    try {
      const content = readFileSync(fullPath, "utf8")
      res.writeHead(200, { "Content-Type": "application/json" })
      return res.end(JSON.stringify({ path: filePath, content }))
    } catch {
      res.writeHead(404, { "Content-Type": "application/json" })
      return res.end(JSON.stringify({ error: "File not found" }))
    }
  }

  // GET /api/deploy
  if (url === "/api/deploy") {
    const info = { scripts: {}, wallet: false, hyperbeam: null }
    try {
      const pkg = JSON.parse(readFileSync(join(ROOT, "package.json"), "utf8"))
      info.scripts = pkg.scripts || {}
    } catch {}
    info.wallet = existsSync(join(ROOT, ".wallet.json"))
    try {
      const env = readFileSync(join(ROOT, ".env.hyperbeam"), "utf8")
      const portMatch = env.match(/PORT=(\d+)/)
      info.hyperbeam = { port: portMatch ? portMatch[1] : "10001", configured: true }
    } catch {
      info.hyperbeam = { configured: false }
    }
    res.writeHead(200, { "Content-Type": "application/json" })
    return res.end(JSON.stringify(info))
  }

  // GET /api/events — SSE
  if (url === "/api/events") {
    res.writeHead(200, {
      "Content-Type": "text/event-stream",
      "Cache-Control": "no-cache",
      Connection: "keep-alive",
    })
    res.write("\n")
    const data = readProgress()
    if (data) {
      res.write(`event: progress\ndata: ${JSON.stringify(data)}\n\n`)
    }
    clients.add(res)
    req.on("close", () => clients.delete(res))
    return
  }

  serveStatic(req, res)
})

server.listen(PORT, () => {
  console.log(`Dashboard API server running on http://localhost:${PORT}`)
  console.log(`Watching: tasks.json, plan.md`)
})
