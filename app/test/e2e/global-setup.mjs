// Boot a real local HyperBEAM once before the Playwright suite runs.
// The app's hb_url points at http://localhost:10001 — Global.js will
// connect to it when HB is up.
import { HyperBEAM } from "wao/test"
import { writeFileSync, mkdirSync } from "node:fs"
import { resolve, dirname } from "node:path"
import { fileURLToPath } from "node:url"

const __dirname = dirname(fileURLToPath(import.meta.url))
const APP_ROOT = resolve(__dirname, "..", "..")
const REPO_ROOT = resolve(APP_ROOT, "..")
const HB_DIR = resolve(REPO_ROOT, "HyperBEAM")

export default async function globalSetup() {
  if (process.env.WAO_SKIP_HB === "1") {
    console.log("[playwright-globalSetup] WAO_SKIP_HB=1 — skipping HB boot")
    return
  }

  console.log(
    `[playwright-globalSetup] booting HyperBEAM on localhost:10001 (cwd=${HB_DIR}) ...`,
  )
  const t0 = Date.now()
  // HyperBEAM class uses cwd-relative paths for wallet + cache. Chdir to
  // REPO_ROOT so .wallet.json resolves, then restore.
  const origCwd = process.cwd()
  process.chdir(REPO_ROOT)
  const hb = await new HyperBEAM({
    cwd: HB_DIR,
    port: 10001,
    cu_port: 6363,
    reset: true,
    genesis_wasm: true,
  }).ready()
  process.chdir(origCwd)
  console.log(
    `[playwright-globalSetup] HB ready in ${((Date.now() - t0) / 1000).toFixed(1)}s — url=${hb.url}`,
  )

  mkdirSync(resolve(APP_ROOT, "test/.tmp"), { recursive: true })
  writeFileSync(
    resolve(APP_ROOT, "test/.tmp/hb-pid.json"),
    JSON.stringify({
      hb_pid: hb._shell?.pid ?? null,
      cu_pid: hb.cuProc?.pid ?? null,
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
