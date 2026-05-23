import { writeFileSync, readFileSync, cpSync, existsSync, mkdirSync } from "fs"
import { resolve } from "path"

const packageJson = resolve(import.meta.dirname, "package.json")
const packageJsonDist = resolve(import.meta.dirname, "dist/package.json")
const packageJsonDist2 = resolve(import.meta.dirname, "dist/esm/package.json")
const json = JSON.parse(readFileSync(packageJson, "utf8"))
delete json.type
delete json.scripts
json.main = "cjs/index.js"
json.module = "esm/index.js"
json.bin = {
  wao: "./cjs/cli.js",
  "wao-esm": "./esm/cli.js",
}
// Ship a postinstall script that patches @permaweb/ao-loader@0.0.44 for
// Node 24+ wasm-memory64. patch-package can't reach ao-loader when npm
// hoists it out of node_modules/wao/node_modules/, so we use a custom
// script that resolves the file directly and edits in place. The script
// is idempotent and fail-open: re-runs are no-ops, version drift skips.
const scriptSrc = resolve(
  import.meta.dirname,
  "scripts/postinstall-patch-ao-loader.cjs"
)
const scriptDst = resolve(import.meta.dirname, "dist/postinstall.cjs")
if (existsSync(scriptSrc)) {
  cpSync(scriptSrc, scriptDst)
}
json.scripts = {
  server: "node cjs/run.js",
  postinstall: "node postinstall.cjs || true",
}

writeFileSync(packageJsonDist, JSON.stringify(json, undefined, 2))

const json2 = {
  type: "module",
}
writeFileSync(packageJsonDist2, JSON.stringify(json2, undefined, 2))
