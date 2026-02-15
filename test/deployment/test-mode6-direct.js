/**
 * Test mode 6 compute using the WASM binary that works in eunit tests.
 * Caches it the same way dev_wasm.erl expects (body key via hb_cache:write).
 */
import HB from "../../src/hb.js"
import HyperBEAM from "../../src/hyperbeam.js"
import { readFileSync } from "fs"

const hbeam = await new HyperBEAM({ reset: true }).ready()
console.log("HB at", hbeam.url)

const hb = new HB({ url: hbeam.url })
await hb.init(hbeam.jwk)
console.log("addr:", hb.addr)

// Step 1: Cache the eunit WASM via wao@1.0 (JS path)
console.log("\n=== Cache via wao@1.0 (JS path) ===")
const wasmBuf = readFileSync("HyperBEAM/test/aos-2-pure-xs.wasm")
console.log("WASM size:", wasmBuf.length)
const t0 = Date.now()
const imageId = await hb.cacheBinary(wasmBuf, "application/wasm")
console.log("Image cached:", imageId, `(${Date.now() - t0}ms)`)

// Step 2: Spawn with this image
console.log("\n=== Spawn ===")
const t1 = Date.now()
const spawnRes = await hb.spawnAOS({ image: imageId })
console.log("PID:", spawnRes.pid, `(${Date.now() - t1}ms)`)

// Step 3: Schedule eval
console.log("\n=== Schedule ===")
const t2 = Date.now()
const sched = await hb.scheduleAOS({ pid: spawnRes.pid, action: "Eval", data: "return 1+1" })
console.log("Slot:", sched.slot, `(${Date.now() - t2}ms)`)

// Step 4: Compute (the critical test)
console.log("\n=== Compute (30s timeout) ===")
const t3 = Date.now()
try {
  const controller = new AbortController()
  const timer = setTimeout(() => controller.abort(), 30000)
  const url = `${hbeam.url}/${spawnRes.pid}/compute/results/outbox?slot=${sched.slot}`
  const resp = await fetch(url, {
    headers: { "accept-bundle": "true" },
    signal: controller.signal
  })
  clearTimeout(timer)
  console.log("Status:", resp.status, `(${Date.now() - t3}ms)`)
  const text = await resp.text()
  console.log("Body (first 300):", text.slice(0, 300))
} catch (e) {
  console.log("FAILED:", e.name, e.message, `(${Date.now() - t3}ms)`)
}

// Step 5: Also test via computeAOS helper
console.log("\n=== computeAOS helper (30s timeout) ===")
const t4 = Date.now()
try {
  const result = await Promise.race([
    hb.computeAOS({ pid: spawnRes.pid, slot: sched.slot }),
    new Promise((_, rej) => setTimeout(() => rej(new Error("timeout")), 30000))
  ])
  console.log("Result:", JSON.stringify(result)?.slice(0, 300), `(${Date.now() - t4}ms)`)
} catch (e) {
  console.log("FAILED:", e.message, `(${Date.now() - t4}ms)`)
}

hbeam.kill()
process.exit(0)
