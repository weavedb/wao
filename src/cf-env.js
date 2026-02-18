// CF Workers polyfill: make emscripten (ao-loader) detect "web" environment.
// Must be imported BEFORE ao-loader.js so it runs first.
if (typeof globalThis.window === "undefined") {
  globalThis.window = globalThis
}

// CF Workers blocks WebAssembly.instantiate(buffer, imports) — only pre-compiled
// WebAssembly.Module objects are allowed. Patch the API so that when a pre-compiled
// module is queued (via cfWasmReady), emscripten's default async loading path works
// unchanged, preserving Asyncify and all other internals.
const _wasmInstantiate = WebAssembly.instantiate.bind(WebAssembly)
let _cfWasmModule = null

export function cfWasmReady(mod) {
  _cfWasmModule = mod
}

WebAssembly.instantiate = async function (source, imports) {
  if (
    _cfWasmModule &&
    (source instanceof ArrayBuffer || ArrayBuffer.isView(source))
  ) {
    const mod = _cfWasmModule
    _cfWasmModule = null
    const instance = await _wasmInstantiate(mod, imports)
    return { instance, module: mod }
  }
  return _wasmInstantiate(source, imports)
}
