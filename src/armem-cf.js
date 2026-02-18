import { cfWasmReady } from "./cf-env.js"
import sqlite from "./lua/sqlite.js"
import aos2_0_3 from "./lua/aos2_0_3.js"
import aos2_0_1 from "./lua/aos2_0_1.js"
import aos2_0_4_32 from "./lua/aos2_0_4_32.js"
import Base from "./armem-base.js"
import StorageMulti from "./storage-multi.js"
import dodb from "./dodb.js"
import { initSync, Waosm } from "./waosm/waosm.js"
import wasmModule from "./waosm/waosm_bg.wasm"

// Import .wasm files — wrangler pre-compiles them to WebAssembly.Module
import aos2_0_1_mod from "./lua/aos2_0_1.wasm"
import aos2_0_3_mod from "./lua/aos2_0_3.wasm"
import aos2_0_4_32_mod from "./lua/aos2_0_4_32.wasm"
import aos2_0_6_mod from "./lua/aos2_0_6.wasm"
import sqlite_mod from "./lua/sqlite.wasm"

const wasm = { sqlite, aos2_0_3, aos2_0_1, aos2_0_4_32 }

const precompiled = {
  aos2_0_1: aos2_0_1_mod,
  aos2_0_3: aos2_0_3_mod,
  aos2_0_4_32: aos2_0_4_32_mod,
  aos2_0_6: aos2_0_6_mod,
  sqlite: sqlite_mod,
}

let wasmReady = false
function cfInit() {
  if (wasmReady) return
  wasmReady = true
  initSync({ module: wasmModule })
}

export default class ArMem extends Base {
  constructor(args = {}) {
    super({ ...args, init: cfInit, Waosm })
    if (args.storage) {
      // Use StorageMulti when D1/R2/KV bindings are available
      if (args.d1 || args.r2 || args.kv) {
        this.db = new StorageMulti({
          storage: args.storage,
          d1: args.d1 || null,
          r2: args.r2 || null,
          kv: args.kv || null,
        })
      } else {
        this.db = dodb(args.storage)
      }
    }
    this.initSync()
  }
  async _getWasm(file) {
    // Queue the pre-compiled WebAssembly.Module so the patched
    // WebAssembly.instantiate in cf-env.js can intercept the call.
    const mod = precompiled[file]
    if (mod) cfWasmReady(mod)
    // Return the buffer — emscripten's normal loading path proceeds,
    // but our patched WebAssembly.instantiate swaps the pre-compiled module.
    if (wasm[file]) return Buffer.from(wasm[file], "base64")
    // For modules without a base64 JS file, return a dummy buffer.
    // The patched instantiate will use the pre-compiled module anyway.
    if (mod) return new Uint8Array(1)
    throw new Error(`WASM module not found: ${file}`)
  }
}
