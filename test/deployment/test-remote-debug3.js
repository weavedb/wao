import HB from "../../src/hb.js"
import { readFileSync } from "fs"
import { seed } from "../../src/utils.js"

const REMOTE = "https://push-1.forward.computer"
const jwk = JSON.parse(readFileSync("/home/basque/Downloads/arweave-keyfile-UCzh5BWlNp1cT1eZQjNFBTk5QAjxtiMz4wiceSpKtus.json", "utf-8"))

const hb = new HB({ url: REMOTE, jwk })
await hb.init(jwk)
console.log("addr:", hb.addr, "operator:", hb.operator)

// Step 1: Spawn with flat string device-stack — this works
console.log("\n=== Step 1: Spawn with flat string device-stack ===")
const spawnRes = await hb.post({
  path: "/~scheduler@1.0/schedule",
  "data-protocol": "ao",
  variant: "ao.TN.1",
  image: "AZ6EXxyOaw3ZjK6htaKA6qZaIh_AbJoJnskGtj7FKZ0",
  "execution-device": "stack@1.0",
  "push-device": "push@1.0",
  "device-stack": "wasi@1.0,json-iface@1.0,wasm-64@1.0,patch@1.0,multipass@1.0",
  "output-prefix": "wasm",
  "patch-from": "/results/outbox",
  "patch-mode": "patches",
  passes: 2,
  "random-seed": seed(16),
  type: "Process",
  device: "process@1.0",
  scheduler: hb.operator ?? hb.addr,
  nonce: seed(8),
})
const pid = spawnRes.headers?.process || spawnRes.out?.process
console.log("pid:", pid)

// Step 2: Schedule a message
console.log("\n=== Step 2: Schedule Eval ===")
try {
  const sched = await hb.scheduleAOS({ pid, action: "Eval", data: "return 1+1" })
  console.log("Schedule slot:", sched.slot)

  // Step 3: Try to compute
  console.log("\n=== Step 3: Compute ===")
  try {
    const comp = await hb.computeAOS({ pid, slot: sched.slot })
    console.log("Compute result:", JSON.stringify(comp)?.slice(0, 500))
  } catch (e) {
    console.log("Compute error:", e.message?.slice(0, 500))
  }
} catch (e) {
  console.log("Schedule error:", e.message?.slice(0, 500))
}

// Step 4: Check the process on remote
console.log("\n=== Step 4: GET process info ===")
try {
  const res = await fetch(`${REMOTE}/${pid}`, {
    headers: { accept: "application/json" },
  })
  console.log("Status:", res.status)
  const text = await res.text()
  console.log("Process info:", text.slice(0, 1000))
} catch (e) {
  console.log("Error:", e.message?.slice(0, 300))
}

// Step 5: Check node version/info
console.log("\n=== Step 5: Node info ===")
try {
  const res = await fetch(`${REMOTE}/~meta@1.0/info`, { headers: { accept: "application/json" } })
  const text = await res.text()
  console.log("Meta info:", text.slice(0, 1000))
} catch (e) {
  console.log("Error:", e.message?.slice(0, 300))
}

// Step 6: What does the remote error look like for array device-stack?
// Use low-level fetch to capture headers
console.log("\n=== Step 6: Array device-stack error details ===")
try {
  const committed = await hb.commit({
    path: "/~scheduler@1.0/schedule",
    "data-protocol": "ao",
    variant: "ao.TN.1",
    image: "AZ6EXzyOaw3ZjK6htaKA6qZaIh_AbJoJnskGtj7FKZ0",
    "execution-device": "stack@1.0",
    "push-device": "push@1.0",
    "device-stack": [
      "wasi@1.0",
      "json-iface@1.0",
      "wasm-64@1.0",
      "patch@1.0",
      "multipass@1.0",
    ],
    "output-prefix": "wasm",
    "patch-from": "/results/outbox",
    "patch-mode": "patches",
    passes: 2,
    "random-seed": seed(16),
    type: "Process",
    device: "process@1.0",
    scheduler: hb.operator ?? hb.addr,
    nonce: seed(8),
  }, { path: false })

  // hb.post uses hbsig's post function which builds multipart
  // Let me use the post104 path instead to try ANS-104
  // Actually let me look at hb.post to understand the multipart encoding
} catch (e) {
  console.log("Error:", e.message?.slice(0, 500))
}

// Step 7: Check if remote supports ao-types header
console.log("\n=== Step 7: Simple test with ao-types header ===")
try {
  // Send a simple message with an array field to see how remote handles it
  const testRes = await hb.post({
    path: "/~scheduler@1.0/schedule",
    "data-protocol": "ao",
    variant: "ao.TN.1",
    image: "AZ6EXxyOaw3ZjK6htaKA6qZaIh_AbJoJnskGtj7FKZ0",
    "execution-device": "stack@1.0",
    "push-device": "push@1.0",
    // Try numbered keys for device-stack (Erlang map style)
    "device-stack.1": "wasi@1.0",
    "device-stack.2": "json-iface@1.0",
    "device-stack.3": "wasm-64@1.0",
    "device-stack.4": "patch@1.0",
    "device-stack.5": "multipass@1.0",
    "output-prefix": "wasm",
    "patch-from": "/results/outbox",
    "patch-mode": "patches",
    passes: 2,
    "random-seed": seed(16),
    type: "Process",
    device: "process@1.0",
    scheduler: hb.operator ?? hb.addr,
    nonce: seed(8),
  })
  const pid7 = testRes.headers?.process || testRes.out?.process
  console.log("Numbered key device-stack pid:", pid7)
} catch (e) {
  console.log("Numbered key error:", e.message?.slice(0, 500))
}
