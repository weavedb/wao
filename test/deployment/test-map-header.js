import HB from "../../src/hb.js"
import { readFileSync } from "fs"
import { seed } from "../../src/utils.js"

const jwk = JSON.parse(readFileSync("/home/basque/Downloads/arweave-keyfile-UCzh5BWlNp1cT1eZQjNFBTk5QAjxtiMz4wiceSpKtus.json", "utf-8"))
const REMOTE = "https://push-10.forward.computer"

const hb = new HB({ url: REMOTE, jwk })
await hb.init(jwk)
console.log("addr:", hb.addr, "operator:", hb.operator)

// Strategy: encode device-stack as a structured-field DICTIONARY string
// with ao-types: device-stack="map" so the structured codec decodes it as a map.
// This avoids multipart encoding entirely (all headers, no body).
console.log("\n=== Test: device-stack as structured-field dictionary ===")
try {
  const res = await hb.post({
    path: "/~scheduler@1.0/schedule",
    "data-protocol": "ao",
    variant: "ao.TN.1",
    image: "AZ6EXxyOaw3ZjK6htaKA6qZaIh_AbJoJnskGtj7FKZ0",
    "execution-device": "stack@1.0",
    "push-device": "push@1.0",
    // Structured-field dictionary format: key="value" pairs
    "device-stack": '1="wasi@1.0", 2="json-iface@1.0", 3="wasm-64@1.0", 4="patch@1.0", 5="multipass@1.0"',
    "output-prefix": "wasm",
    "patch-from": "/results/outbox",
    "patch-mode": "patches",
    passes: "2",  // String to avoid hbsig auto-generating ao-types
    "random-seed": seed(16),
    type: "Process",
    device: "process@1.0",
    scheduler: hb.operator ?? hb.addr,
    nonce: seed(8),
    // Manual ao-types: tell structured codec to decode device-stack as map
    // and passes as integer
    "ao-types": 'device-stack="map", passes="integer"',
  })
  const pid = res.headers?.process || res.out?.process
  console.log("SPAWN PID:", pid)

  if (pid) {
    // Try compute
    console.log("\n--- Compute (slot 0) ---")
    try {
      const comp = await hb.computeAOS({ pid, slot: 0 })
      console.log("compute:", JSON.stringify(comp)?.slice(0, 500))
      console.log("TEST: PASS")
    } catch (e) {
      console.log("compute error:", e.message?.slice(0, 500))
    }

    // Try schedule + compute
    console.log("\n--- Schedule Eval ---")
    try {
      const sched = await hb.scheduleAOS({ pid, action: "Eval", data: "return 1+1" })
      console.log("slot:", sched.slot)

      console.log("\n--- Compute ---")
      try {
        const comp = await hb.computeAOS({ pid, slot: sched.slot })
        console.log("compute:", JSON.stringify(comp)?.slice(0, 500))
        console.log("TEST: FULL PASS")
      } catch (e) {
        console.log("compute error:", e.message?.slice(0, 500))
      }
    } catch (e) {
      console.log("schedule error:", e.message?.slice(0, 500))
    }
  }
} catch (e) {
  console.log("ERROR:", e.message?.slice(0, 500))
}

// Also test: what does the signed request look like?
console.log("\n=== Signed request inspection ===")
const signed = await hb.sign({
  path: "/~scheduler@1.0/schedule",
  "data-protocol": "ao",
  variant: "ao.TN.1",
  image: "AZ6EXxyOaw3ZjK6htaKA6qZaIh_AbJoJnskGtj7FKZ0",
  "execution-device": "stack@1.0",
  "push-device": "push@1.0",
  "device-stack": '1="wasi@1.0", 2="json-iface@1.0", 3="wasm-64@1.0", 4="patch@1.0", 5="multipass@1.0"',
  "output-prefix": "wasm",
  "patch-from": "/results/outbox",
  "patch-mode": "patches",
  passes: "2",
  "random-seed": seed(16),
  type: "Process",
  device: "process@1.0",
  scheduler: hb.operator ?? hb.addr,
  nonce: seed(8),
  "ao-types": 'device-stack="map", passes="integer"',
})
console.log("Method:", signed.method)
console.log("Body:", signed.body ? "has body" : "NO BODY")
console.log("Headers:")
for (const [k, v] of Object.entries(signed.headers || {})) {
  console.log(`  ${k}: ${String(v).slice(0, 200)}`)
}
