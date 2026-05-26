// Kill the HyperBEAM instance started by global-setup.mjs.
import { readFileSync, existsSync, rmSync } from "node:fs"
import { spawnSync } from "node:child_process"
import { resolve, dirname } from "node:path"
import { fileURLToPath } from "node:url"

const __dirname = dirname(fileURLToPath(import.meta.url))
const APP_ROOT = resolve(__dirname, "..", "..")

export default async function globalTeardown() {
  if (process.env.WAO_SKIP_HB === "1") return
  const sidecar = resolve(APP_ROOT, "test/.tmp/hb-pid.json")
  if (!existsSync(sidecar)) return
  const { hb_pid, cu_pid } = JSON.parse(readFileSync(sidecar, "utf8"))
  console.log(
    `[playwright-globalTeardown] killing HB pid=${hb_pid} CU pid=${cu_pid}`,
  )
  spawnSync("bash", [
    "-c",
    `kill -9 ${hb_pid} 2>/dev/null; kill -9 ${cu_pid} 2>/dev/null; pkill -9 -f 'beam.smp.*10001' 2>/dev/null; pgrep -f ':6363 ' | xargs -r kill -9 2>/dev/null; true`,
  ])
  try {
    rmSync(sidecar)
  } catch {}
}
