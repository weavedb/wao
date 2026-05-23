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
// Ship patches/ and a postinstall hook so end users get the Node 24+
// wasm-memory64 BigInt patch applied to @permaweb/ao-loader@0.0.44 on
// `npm install wao`. patch-package is a regular dependency here (not
// devDependency) so it's available in user-side node_modules.
const patchesSrc = resolve(import.meta.dirname, "patches")
const patchesDst = resolve(import.meta.dirname, "dist/patches")
if (existsSync(patchesSrc)) {
  mkdirSync(patchesDst, { recursive: true })
  cpSync(patchesSrc, patchesDst, { recursive: true })
}
json.dependencies = {
  ...(json.dependencies || {}),
  "patch-package": json.devDependencies?.["patch-package"] || "^8.0.1",
}
if (json.devDependencies) delete json.devDependencies["patch-package"]
json.scripts = {
  server: "node cjs/run.js",
  postinstall: "patch-package || true",
}

writeFileSync(packageJsonDist, JSON.stringify(json, undefined, 2))

const json2 = {
  type: "module",
}
writeFileSync(packageJsonDist2, JSON.stringify(json2, undefined, 2))
