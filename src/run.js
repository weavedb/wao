#!/usr/bin/env node
// Note: this script is normally launched by src/cli.js via pm2, which
// gates --experimental-wasm-memory64 to Node <24. If you run this file
// directly on Node <24, prefix with the flag: NODE_OPTIONS=--experimental-wasm-memory64
import yargs from "yargs"
import { resolve } from "path"
import { unlinkSync } from "fs"
import Server from "./server.js"
let {
  reset = false,
  memory = false,
  port = 4000,
  db = ".cache",
  hb,
} = yargs(process.argv.slice(2)).argv

db = memory ? null : resolve(process.cwd(), db)
if (reset) {
  try {
    unlinkSync(db)
  } catch (e) {}
}
const main = async () => new Server({ log: true, port, db, hb_url: hb })

main()
