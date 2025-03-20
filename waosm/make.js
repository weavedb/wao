import fs from "fs"
import path from "path"

// Path to your WASM file
const wasmPath = path.resolve("./pkg-node/waosm_bg.wasm")

// Read the WASM file
const wasmBinary = fs.readFileSync(wasmPath)

// Convert to base64
const base64Wasm = wasmBinary.toString("base64")

// Create JavaScript module content
const jsContent = `// Auto-generated WASM module
const base64Wasm = "${base64Wasm}";

function base64ToUint8Array(base64) {
  const binaryString = atob(base64);
  const bytes = new Uint8Array(binaryString.length);
  for (let i = 0; i < binaryString.length; i++) {
    bytes[i] = binaryString.charCodeAt(i);
  }
  return bytes;
}

// Export the WASM binary as Uint8Array
export default base64ToUint8Array(base64Wasm);
`

// Write the JavaScript module
fs.writeFileSync(path.resolve("../src/waosm-node/waosm_bg.wasm.js"), jsContent)

console.log("WASM file successfully converted to base64 JavaScript module")
