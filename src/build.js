import fs from "fs"

const wasmBytes = fs.readFileSync("./waosm/waosm_bg.wasm")
const wasmB64 = wasmBytes.toString("base64")

const exports = `export const wasmB64 = "${wasmB64}";`
fs.writeFileSync("./waosm/waosm_b64.js", exports)
