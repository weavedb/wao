#!/usr/bin/env -S node --experimental-wasm-memory64

import Server from "./server.js"

const main = async () => new Server({ log: true })

main()
