// Boot real local HyperBEAM + WAO signaling hub before the Playwright suite.
// HyperBEAM runs on :10001 (genesis-wasm CU on :6363).
// WAO hub (src/hub/index.js) runs on :7777 — provides the WebSocket signaling
// + bundler + CU endpoints that ProxyModal/FSModal/LeftNetworks connect to.
import { HyperBEAM } from "wao/test"
import { spawn } from "node:child_process"
import { writeFileSync, mkdirSync } from "node:fs"
import { resolve, dirname } from "node:path"
import { fileURLToPath } from "node:url"

const __dirname = dirname(fileURLToPath(import.meta.url))
const APP_ROOT = resolve(__dirname, "..", "..")
const REPO_ROOT = resolve(APP_ROOT, "..")
const HB_DIR = resolve(REPO_ROOT, "HyperBEAM")
const HUB_PORT = 7777

export default async function globalSetup() {
  if (process.env.WAO_SKIP_HB === "1") {
    console.log("[playwright-globalSetup] WAO_SKIP_HB=1 — skipping HB boot")
    return
  }

  console.log(
    `[playwright-globalSetup] booting HyperBEAM on localhost:10001 (cwd=${HB_DIR}) ...`,
  )
  const t0 = Date.now()
  const origCwd = process.cwd()
  process.chdir(REPO_ROOT)
  const hb = await new HyperBEAM({
    cwd: HB_DIR,
    port: 10001,
    cu_port: 6363,
    reset: true,
    genesis_wasm: true,
  }).ready()
  console.log(
    `[playwright-globalSetup] HB ready in ${((Date.now() - t0) / 1000).toFixed(1)}s — url=${hb.url}`,
  )

  // Start the WAO signaling hub (src/hub/index.js). It opens a WebSocket
  // server on $HUB_PORT plus HTTP bundler + CU endpoints.
  console.log(
    `[playwright-globalSetup] booting WAO hub on ws://localhost:${HUB_PORT} ...`,
  )
  const hub = spawn(
    "node",
    [resolve(REPO_ROOT, "src/hub/index.js"), "--port", String(HUB_PORT)],
    {
      cwd: REPO_ROOT,
      stdio: ["ignore", "pipe", "pipe"],
      detached: false,
    },
  )
  hub.stdout?.on("data", d => {
    const s = d.toString().trim()
    if (s) console.log(`[hub] ${s}`)
  })
  hub.stderr?.on("data", d => console.error(`[hub-err] ${d}`))

  // Wait for the hub to bind its port.
  const waitForHub = async () => {
    for (let i = 0; i < 30; i++) {
      try {
        const { WebSocket } = await import("ws")
        const ws = new WebSocket(`ws://localhost:${HUB_PORT}`)
        await new Promise((res, rej) => {
          ws.once("open", () => {
            ws.close()
            res()
          })
          ws.once("error", rej)
          setTimeout(() => rej(new Error("timeout")), 1000)
        })
        return true
      } catch {
        await new Promise(r => setTimeout(r, 500))
      }
    }
    return false
  }
  const hubReady = await waitForHub()
  console.log(
    `[playwright-globalSetup] hub ${hubReady ? "ready" : "FAILED to bind"} on :${HUB_PORT}`,
  )

  process.chdir(origCwd)

  mkdirSync(resolve(APP_ROOT, "test/.tmp"), { recursive: true })
  writeFileSync(
    resolve(APP_ROOT, "test/.tmp/hb-pid.json"),
    JSON.stringify({
      hb_pid: hb._shell?.pid ?? null,
      cu_pid: hb.cuProc?.pid ?? null,
      hub_pid: hub.pid ?? null,
      hub_port: HUB_PORT,
      url: hb.url,
    }),
  )
}

// Allow direct invocation for debugging:  node test/e2e/global-setup.mjs
if (import.meta.url === `file://${process.argv[1]}`) {
  globalSetup().then(
    () => console.log("[direct] globalSetup complete"),
    err => {
      console.error("[direct] globalSetup error:", err)
      process.exit(1)
    },
  )
}
