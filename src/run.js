#!/usr/bin/env -S node --experimental-wasm-memory64
import yargs from "yargs"
import { resolve } from "path"
import { unlinkSync } from "fs"
import Server from "./server.js"
let {
  reset = false,
  memory = false,
  port = 4000,
  db = ".cache",
} = yargs(process.argv.slice(2)).argv
db = memory ? null : resolve(process.cwd(), db)
if (reset) {
  try {
    unlinkSync(db)
  } catch (e) {}
}
const main = async () => new Server({ log: true, port, db })

main()
