/**
 * Test mode 6 with node config matching eunit tests:
 * - cache_control => "always"
 * - store: just hb_store_fs (no gateway)
 * - No scheduling_mode override
 */
import HB from "../../src/hb.js"
import HyperBEAM from "../../src/hyperbeam.js"

// Start HyperBEAM with minimal config (no gateway stores)
const hbeam = await new HyperBEAM({ reset: true }).ready()
console.log("HB at", hbeam.url)

const hb = new HB({ url: hbeam.url })
await hb.init(hbeam.jwk)
console.log("addr:", hb.addr)

// Step 1: Cache WASM
console.log("\n=== Step 1: Cache WASM ===")
const t0 = Date.now()
const imageId = await hb.cacheBinary(
  (await import("fs")).readFileSync("HyperBEAM/test/aos-2-pure-xs.wasm"),
  "application/wasm"
)
console.log("Image cached:", imageId, `(${Date.now() - t0}ms)`)

// Step 2: Spawn with eunit-like tags (Erlang list style for device-stack)
console.log("\n=== Step 2: Spawn ===")
const t1 = Date.now()
const spawnRes = await hb.spawnAOS({ image: imageId })
console.log("PID:", spawnRes.pid, `(${Date.now() - t1}ms)`)

// Step 3: Schedule eval
console.log("\n=== Step 3: Schedule ===")
const t2 = Date.now()
const sched = await hb.scheduleAOS({ pid: spawnRes.pid, action: "Eval", data: "return 1+1" })
console.log("Slot:", sched.slot, `(${Date.now() - t2}ms)`)

// Step 4: Try compute with short timeout first (30s)
console.log("\n=== Step 4: computeAOS (30s timeout) ===")
const t3 = Date.now()
try {
  const result = await Promise.race([
    hb.computeAOS({ pid: spawnRes.pid, slot: sched.slot }),
    new Promise((_, rej) => setTimeout(() => rej(new Error("timeout")), 30000))
  ])
  console.log("Result:", JSON.stringify(result)?.slice(0, 300), `(${Date.now() - t3}ms)`)
} catch (e) {
  console.log("FAILED:", e.message, `(${Date.now() - t3}ms)`)
}

// Step 5: Try raw fetch to see response
console.log("\n=== Step 5: Raw compute fetch (30s) ===")
const t4 = Date.now()
try {
  const controller = new AbortController()
  const timer = setTimeout(() => controller.abort(), 30000)
  const resp = await fetch(`${hbeam.url}/${spawnRes.pid}/compute/results`, {
    signal: controller.signal
  })
  clearTimeout(timer)
  console.log("Status:", resp.status, `(${Date.now() - t4}ms)`)
  const text = await resp.text()
  console.log("Body (first 500):", text.slice(0, 500))
} catch (e) {
  console.log("FAILED:", e.name, e.message, `(${Date.now() - t4}ms)`)
}

hbeam.kill()
process.exit(0)
