import HB from "../../src/hb.js"
import { readFileSync } from "fs"
import { seed } from "../../src/utils.js"

const jwk = JSON.parse(readFileSync("/home/basque/Downloads/arweave-keyfile-UCzh5BWlNp1cT1eZQjNFBTk5QAjxtiMz4wiceSpKtus.json", "utf-8"))
const REMOTE = "https://push-10.forward.computer"

const hb = new HB({ url: REMOTE, jwk })
await hb.init(jwk)
console.log("addr:", hb.addr, "operator:", hb.operator)

// Strategy: Use ANS-104 with:
// - device-stack tag = structured-field dictionary (single tag)
// - ao-types tag = device-stack="map", passes="integer"
// ANS-104 signatures verify against original binary, not decoded form,
// so the structured codec conversion to map won't break verification.
console.log("\n=== Test: ANS-104 with device-stack as structured-field dictionary ===")

try {
  const tags = {
    "data-protocol": "ao",
    variant: "ao.TN.1",
    image: "AZ6EXxyOaw3ZjK6htaKA6qZaIh_AbJoJnskGtj7FKZ0",
    "execution-device": "stack@1.0",
    "push-device": "push@1.0",
    // Single tag with structured-field dictionary format
    "device-stack": '1="wasi@1.0", 2="json-iface@1.0", 3="wasm-64@1.0", 4="patch@1.0", 5="multipass@1.0"',
    "output-prefix": "wasm",
    "patch-from": "/results/outbox",
    "patch-mode": "patches",
    passes: "2",
    "random-seed": seed(16),
    type: "Process",
    device: "process@1.0",
    scheduler: hb.operator ?? hb.addr,
    authority: hb.addr,
    // Tell structured codec how to decode
    "ao-types": 'device-stack="map", passes="integer"',
  }

  const res = await hb.post104({
    path: "/~scheduler@1.0/schedule",
    tags,
  })

  const pid = res.headers?.process || res.out?.process
  console.log("SPAWN PID:", pid)

  if (pid) {
    // Try compute (slot 0 = the spawn message itself)
    console.log("\n--- Compute (slot 0) ---")
    try {
      const comp = await hb.computeAOS({ pid, slot: 0 })
      console.log("compute:", JSON.stringify(comp)?.slice(0, 500))
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
        console.log("TEST: FULL PASS!")
      } catch (e) {
        console.log("compute error:", e.message?.slice(0, 500))
      }
    } catch (e) {
      console.log("schedule error:", e.message?.slice(0, 500))
    }
  }
} catch (e) {
  console.log("ERROR:", e.message?.slice(0, 500))
  if (e.stack) console.log(e.stack.slice(0, 300))
}

// Test 2: Also try on push-1 and push (primary)
for (const url of ["https://push-1.forward.computer", "https://push.forward.computer"]) {
  console.log(`\n=== Test on ${url} ===`)
  const hb2 = new HB({ url, jwk })
  await hb2.init(jwk)
  try {
    const res = await hb2.post104({
      path: "/~scheduler@1.0/schedule",
      tags: {
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
        scheduler: hb2.operator ?? hb2.addr,
        authority: hb2.addr,
        "ao-types": 'device-stack="map", passes="integer"',
      },
    })
    const pid = res.headers?.process || res.out?.process
    console.log("PID:", pid)
  } catch (e) {
    console.log("ERROR:", e.message?.slice(0, 300))
  }
}
