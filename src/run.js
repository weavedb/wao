#!/usr/bin/env -S node --experimental-wasm-memory64
import yargs from "yargs"
let { port = 4000 } = yargs(process.argv.slice(2)).argv

import Server from "./server.js"

const main = async () => new Server({ log: true, port })

main()
