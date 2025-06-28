import yargs from "yargs"
import { resolve } from "path"
import { readFileSync, writeFileSync } from "fs"

const {
  _: [file, out],
} = yargs(process.argv.slice(2)).argv

const src = readFileSync(resolve(process.cwd(), file)).toString("base64")

writeFileSync(resolve(process.cwd(), out), `export default "${src}"`)
