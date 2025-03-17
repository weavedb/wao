export * from "./waosm-node/waosm_bg.js"
import * as wasmBindings from "./waosm-node/waosm_bg.js"
import { __wbg_set_wasm } from "./waosm-node/waosm_bg.js"
import { readFileSync } from "fs"
import { resolve } from "path"

const run = dirname => {
  const wasm = readFileSync(resolve(dirname, "./waosm-node/waosm_bg.wasm"))
  const wasmModule = new WebAssembly.Module(wasm)
  const wasmInstance = new WebAssembly.Instance(wasmModule, {
    "./waosm_bg.js": wasmBindings,
  })
  __wbg_set_wasm(wasmInstance.exports)
}

let dirname
if (typeof __filename !== "undefined" && typeof __dirname !== "undefined")
  run(__dirname)
else {
  import("./dirname.js")
    .then(({ default: dirname }) => run(dirname))
    .catch(e => {
      console.log(e)
    })
}
