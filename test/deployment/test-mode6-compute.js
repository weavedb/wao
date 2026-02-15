/**
 * Focused test: does Mode 6 compute EVER finish?
 * Gives it 5 minutes instead of 2.
 */
import AO from "../../src/ao.js"
import HyperBEAM from "../../src/hyperbeam.js"

console.log("Starting HB (no genesis_wasm, no reset)...")
const hbeam = await new HyperBEAM({ reset: true }).ready()
console.log("HB started at", hbeam.url)

const ao = await new AO({ hb: hbeam.url, mode: "aos" }).init(hbeam.jwk)
console.log("AO initialized, mode:", ao.mode)

// Spawn
console.log("\n--- spawnAOS ---")
const t0 = Date.now()
const { pid } = await ao.spwn()
console.log("PID:", pid, `(${Date.now() - t0}ms)`)

// Schedule eval
console.log("\n--- scheduleAOS ---")
const t1 = Date.now()
const sched = await ao.hb.scheduleAOS({ pid, action: "Eval", data: "return 1+1" })
console.log("Slot:", sched.slot, `(${Date.now() - t1}ms)`)

// Compute with 5-minute timeout
console.log("\n--- computeAOS (waiting up to 5 min) ---")
const t2 = Date.now()
const controller = new AbortController()
const timer = setTimeout(() => controller.abort(), 300000) // 5 min

try {
  const url = `${hbeam.url}/${pid}/compute/results/outbox?slot=${sched.slot}`
  console.log("Fetching:", url)
  const response = await fetch(url, {
    headers: { "accept-bundle": "true" },
    signal: controller.signal,
  })
  clearTimeout(timer)
  console.log("Response status:", response.status, `(${Date.now() - t2}ms)`)
  const text = await response.text()
  console.log("Response body (first 500 chars):", text.slice(0, 500))
} catch (e) {
  clearTimeout(timer)
  console.log("Compute error:", e.name, e.message, `(${Date.now() - t2}ms)`)
}

hbeam.kill()
process.exit(0)
