// Wrapper that runs Playwright from the repository root so HyperBEAM's
// relative path resolution (.wallet.json, HyperBEAM submodule) works.
// We still load Playwright via the app's local install + config.
import { spawnSync } from "node:child_process"
import { resolve, dirname } from "node:path"
import { fileURLToPath } from "node:url"

const __dirname = dirname(fileURLToPath(import.meta.url))
const APP_ROOT = resolve(__dirname, "..", "..")
const REPO_ROOT = resolve(APP_ROOT, "..")

const args = ["test", "-c", resolve(APP_ROOT, "playwright.config.mjs")]
if (process.env.PWPLAYHEADED === "1") args.push("--headed")
if (process.env.PWUI === "1") args.push("--ui")
for (const extra of process.argv.slice(2)) args.push(extra)

const cli = resolve(APP_ROOT, "node_modules/.bin/playwright")
const result = spawnSync(cli, args, {
  cwd: REPO_ROOT,
  stdio: "inherit",
  env: { ...process.env, WAO_E2E_FROM_ROOT: "1" },
})
process.exit(result.status ?? 1)
