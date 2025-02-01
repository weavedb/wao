export * from "./waosm-node/waosm_bg.js"
import * as wasmBindings from "./waosm-node/waosm_bg.js"
import { __wbg_set_wasm } from "./waosm-node/waosm_bg.js"
import { readFileSync } from "fs"
import { resolve } from "path"
const wasm = readFileSync(
  resolve(import.meta.dirname, "./waosm-node/waosm_bg.wasm"),
)
const wasmModule = new WebAssembly.Module(wasm)
const wasmInstance = new WebAssembly.Instance(wasmModule, {
  "./waosm_bg.js": wasmBindings,
})
__wbg_set_wasm(wasmInstance.exports)
