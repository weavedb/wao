#!/usr/bin/env node
// Patch @permaweb/ao-loader@0.0.44 for Node 24+ wasm-memory64.
//
// patch-package can't reach ao-loader when it's hoisted out of
// node_modules/wao/node_modules/ (the default on modern npm). This script
// locates ao-loader via require.resolve and applies the same growMemory
// patch inline — idempotent, safe to run on already-patched files,
// fail-open if the file looks different from upstream.

const fs = require("fs")
const path = require("path")

function findAoLoader() {
    try {
        return require.resolve("@permaweb/ao-loader/dist/index.cjs", {
            paths: [process.cwd(), __dirname, path.resolve(__dirname, "..")],
        })
    } catch (_) {}
    // Fall back to scanning the closest node_modules/.
    let dir = path.resolve(__dirname, "..")
    while (dir !== "/" && dir !== "") {
        const candidate = path.join(dir, "node_modules", "@permaweb", "ao-loader", "dist", "index.cjs")
        if (fs.existsSync(candidate)) return candidate
        dir = path.dirname(dir)
    }
    return null
}

const target = findAoLoader()
if (!target) {
    // Either ao-loader isn't installed (the consumer doesn't need it) or
    // we couldn't locate it — fail-open.
    process.exit(0)
}

const src = fs.readFileSync(target, "utf8")

if (src.includes("Node 24+ wasm-memory64")) {
    // Already patched.
    process.exit(0)
}

const OLD = `        var growMemory = (size) => {
          var b = wasmMemory.buffer;
          var pages = (size - b.byteLength + 65535) / 65536;
          try {
            wasmMemory.grow(pages);
            updateMemoryViews();
            return 1;
          } catch (e) {
            err(\`growMemory: Attempted to grow heap from \${b.byteLength} bytes to \${size} bytes, but got error: \${e}\`);
          }
        };`

const NEW = `        var growMemory = (size) => {
          var b = wasmMemory.buffer;
          // Node 24+ wasm-memory64: wasmMemory.grow needs an integer page
          // count, and if the underlying memory is memory64, grow() expects
          // a BigInt (Number args are rejected). Compute as integer pages,
          // then try BigInt first and fall back to Number for memory32.
          var pages = Math.ceil((size - b.byteLength + 65535) / 65536);
          try {
            try {
              wasmMemory.grow(BigInt(pages));
            } catch (_e) {
              wasmMemory.grow(pages);
            }
            updateMemoryViews();
            return 1;
          } catch (e) {
            err(\`growMemory: Attempted to grow heap from \${b.byteLength} bytes to \${size} bytes, but got error: \${e}\`);
          }
        };`

if (!src.includes(OLD)) {
    // File doesn't match the expected upstream — likely a newer ao-loader
    // version where the fix has been applied differently or upstream
    // rewrote the function. Fail-open.
    process.exit(0)
}

const out = src.split(OLD).join(NEW)
fs.writeFileSync(target, out)
console.error("wao postinstall: patched @permaweb/ao-loader growMemory for Node 24+ wasm-memory64")
