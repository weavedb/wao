import { writeFileSync, readFileSync } from "fs"
import { resolve } from "path"

const packageJson = resolve(import.meta.dirname, "package.json")
const packageJsonDist = resolve(import.meta.dirname, "dist/package.json")
const packageJsonDist2 = resolve(import.meta.dirname, "dist/test/package.json")
const json = JSON.parse(readFileSync(packageJson, "utf8"))
delete json.type
delete json.devDependencies
delete json.scripts
json.main = "cjs/index.js"
json.module = "esm/index.js"

console.log(json)
writeFileSync(packageJsonDist, JSON.stringify(json, undefined, 2))

const json2 = JSON.parse(readFileSync(packageJson, "utf8"))
json2.type = "module"
delete json2.devDependencies
delete json2.scripts
delete json2.main
json2.module = "index.js"
writeFileSync(packageJsonDist2, JSON.stringify(json2, undefined, 2))
