/**
 * Upload image, video, audio, markdown, and JSON to verify the rich data viewer.
 *
 * Usage: node devnet/test/test-rich-upload.js
 */
import _Arweave from "arweave"
const Arweave = _Arweave.default ?? _Arweave
import { readFileSync } from "fs"
import { acc } from "../../src/accounts.js"

const PORT = process.env.PORT || 8787
const BASE = `http://localhost:${PORT}`

const arweave = Arweave.init()
arweave.transactions.getPrice = () => Promise.resolve("0")

async function getAnchor() {
  const info = await fetch(`${BASE}/ar`).then(r => r.json())
  return info.current || "0000000000000000000000000000000000000000000"
}
arweave.transactions.getTransactionAnchor = getAnchor

const owner = acc[0]

async function upload(name, contentType, data) {
  const tx = await arweave.createTransaction({ data })
  tx.addTag("Content-Type", contentType)
  tx.addTag("App-Name", "WAO-Explorer-Test")
  tx.addTag("Name", name)
  await arweave.transactions.sign(tx, owner.jwk)
  const body = tx.toJSON ? tx.toJSON() : JSON.parse(JSON.stringify(tx))
  const res = await fetch(`${BASE}/ar/tx`, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify(body),
  })
  const json = await res.json()
  if (res.status !== 200) {
    console.error(`  FAIL ${name}: ${res.status}`, json)
    return null
  }
  const getRes = await fetch(`${BASE}/ar/${tx.id}`)
  const ct = getRes.headers.get("content-type") || "(none)"
  const blob = await getRes.blob()
  console.log(`  ${name}: ${tx.id}  [${ct}] ${blob.size} bytes`)
  return tx.id
}

const results = {}

// ---- Image (PNG) ----
console.log("--- Image (PNG) ---")
results.png = await upload("cover.png", "image/png",
  readFileSync("/home/basque/oasis/wao-beta3/docs/docs/public/images/cover.png"))

// ---- Image (JPEG) ----
console.log("\n--- Image (JPEG) ---")
results.jpg = await upload("book.jpg", "image/jpeg",
  readFileSync("/home/basque/oasis/wao-beta3/docs/docs/public/images/book.jpg"))

// ---- Video (MP4) ----
console.log("\n--- Video (MP4) ---")
results.mp4 = await upload("clip.mp4", "video/mp4",
  readFileSync("/home/basque/Downloads/video/CBF0QrkT17Ohj1sE.mp4"))

// ---- Audio (WAV) ----
console.log("\n--- Audio (WAV) ---")
results.wav = await upload("ambient.wav", "audio/wav",
  readFileSync("/home/basque/Downloads/audio/ambient_futuristic_bgm.wav"))

// ---- Audio (MP3) ----
console.log("\n--- Audio (MP3) ---")
results.mp3 = await upload("track.mp3", "audio/mpeg",
  readFileSync("/home/basque/Downloads/audio/DALL E 2025 01 23 08 39 06 A futuristic and sleek... (1a3e0a4050814bdf98738d1e5d44de71).mp3"))

// ---- Markdown ----
console.log("\n--- Markdown ---")
results.md = await upload("README.md", "text/markdown", `# WAO Explorer

Welcome to the **WAO Transaction Explorer** — a rich viewer for the AO network.

## Features

- Browse *transactions*, **processes**, and modules
- View message trees and compute results
- Real-time updates via WebSocket
- **Rich data viewer** with support for:
  - Images (PNG, JPEG, SVG)
  - Video and audio playback
  - Markdown rendering
  - JSON tree viewer
  - Code with syntax highlighting

## Quick Start

\`\`\`bash
cd devnet
npm run dev
\`\`\`

Then open [http://localhost:8787](http://localhost:8787) in your browser.

## Architecture

1. **Frontend** — vanilla JS SPA with hash routing
2. **Backend** — Cloudflare Worker + Durable Object
3. **Storage** — DO storage with optional D1/R2

> This is a test document uploaded to verify the markdown renderer.

### Links

- [AO Documentation](https://ao.arweave.dev)
- [Arweave](https://arweave.org)
`)

// ---- JSON ----
console.log("\n--- JSON ---")
results.json = await upload("config.json", "application/json", JSON.stringify({
  name: "WAO Devnet Config",
  version: "1.0.0",
  network: {
    ar: { port: 4000, host: "localhost" },
    mu: { port: 4002, host: "localhost" },
    su: { port: 4003, host: "localhost" },
    cu: { port: 4004, host: "localhost" },
  },
  modules: [
    { id: "ISShJH1ij-hPPt9St5UFFr_8Ys3Kj5cyg7zrMGt7H9s", name: "AOS 2.0.6" },
  ],
  features: {
    websocket: true,
    explorer: true,
    rich_viewer: true,
    bundle_decomposition: true,
  },
  tags: ["devnet", "testing", "ao"],
  limits: { memory: "256MB", compute: "10s", data: "100MB" },
}, null, 2))

// ---- Summary ----
console.log("\n=== Open in browser ===")
for (const [label, id] of Object.entries(results)) {
  if (id) console.log(`  ${label.padEnd(5)} http://localhost:${PORT}/#/entity/${id}`)
}
console.log("\nDone!")
