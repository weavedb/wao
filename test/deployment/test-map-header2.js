import HB from "../../src/hb.js"
import { readFileSync } from "fs"
import { seed } from "../../src/utils.js"

const jwk = JSON.parse(readFileSync("/home/basque/Downloads/arweave-keyfile-UCzh5BWlNp1cT1eZQjNFBTk5QAjxtiMz4wiceSpKtus.json", "utf-8"))
const REMOTE = "https://push-10.forward.computer"

const hb = new HB({ url: REMOTE, jwk })
await hb.init(jwk)
console.log("addr:", hb.addr, "operator:", hb.operator)

// Strategy: Sign the message normally via hb.sign(), then inject
// ao-types: device-stack="map" into headers AFTER signing.
// ao-types is not in the signature-input, so modifying it won't break verification.
console.log("\n=== Test: inject ao-types after signing ===")

const fields = {
  "data-protocol": "ao",
  variant: "ao.TN.1",
  image: "AZ6EXxyOaw3ZjK6htaKA6qZaIh_AbJoJnskGtj7FKZ0",
  "execution-device": "stack@1.0",
  "push-device": "push@1.0",
  // Structured-field dictionary format for device-stack
  "device-stack": '1="wasi@1.0", 2="json-iface@1.0", 3="wasm-64@1.0", 4="patch@1.0", 5="multipass@1.0"',
  "output-prefix": "wasm",
  "patch-from": "/results/outbox",
  "patch-mode": "patches",
  passes: 2,
  "random-seed": seed(16),
  type: "Process",
  device: "process@1.0",
  scheduler: hb.operator ?? hb.addr,
  nonce: seed(8),
}

// Sign the message
const signed = await hb.sign({
  path: "/~scheduler@1.0/schedule",
  ...fields,
})

console.log("Original ao-types:", signed.headers["ao-types"])
console.log("Body:", signed.body ? "has body" : "NO BODY")

// Inject device-stack="map" into ao-types
const originalAoTypes = signed.headers["ao-types"] || ""
const newAoTypes = originalAoTypes
  ? `device-stack="map", ${originalAoTypes}`
  : 'device-stack="map"'
signed.headers["ao-types"] = newAoTypes
console.log("Modified ao-types:", signed.headers["ao-types"])

// Verify ao-types is NOT in signature-input
const sigInput = signed.headers["signature-input"]
console.log("Signature covers ao-types?", sigInput?.includes('"ao-types"'))

// Send manually
console.log("\n--- Sending ---")
try {
  const response = await fetch(signed.url, {
    method: signed.method || "POST",
    headers: signed.headers,
    body: signed.body,
  })
  console.log("Status:", response.status)

  if (response.status >= 400) {
    const hdrs = Object.fromEntries(response.headers.entries())
    console.log("Details:", hdrs.details?.slice(0, 1000))
    console.log("Stack:", hdrs.stacktrace?.slice(0, 500))
  } else {
    const { result } = await import("hbsig")
    const res = await result(response)
    const pid = res.headers?.process || res.out?.process
    console.log("SPAWN PID:", pid)

    if (pid) {
      // Try compute
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
        } catch (e) {
          console.log("compute error:", e.message?.slice(0, 500))
        }
      } catch (e) {
        console.log("schedule error:", e.message?.slice(0, 500))
      }
    }
  }
} catch (e) {
  console.log("ERROR:", e.message?.slice(0, 500))
}
