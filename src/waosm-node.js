export * from "./waosm-node/waosm_bg.js"
import * as wasmBindings from "./waosm-node/waosm_bg.js"
import { __wbg_set_wasm } from "./waosm-node/waosm_bg.js"
import wasmBinary from "./waosm-node/waosm_bg.wasm.js"
const wasmModule = new WebAssembly.Module(wasmBinary)
const wasmInstance = new WebAssembly.Instance(wasmModule, {
  "./waosm_bg.js": wasmBindings,
})
__wbg_set_wasm(wasmInstance.exports)
