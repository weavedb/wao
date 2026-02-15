import HB from "../../src/hb.js"
import { readFileSync } from "fs"
import { seed } from "../../src/utils.js"
import { commit, result, httpsig_to } from "hbsig"

const jwk = JSON.parse(readFileSync("/home/basque/Downloads/arweave-keyfile-UCzh5BWlNp1cT1eZQjNFBTk5QAjxtiMz4wiceSpKtus.json", "utf-8"))
const REMOTE = "https://push-10.forward.computer"

const hb = new HB({ url: REMOTE, jwk })
await hb.init(jwk)
console.log("addr:", hb.addr, "operator:", hb.operator)

// Test: What does hbsig's enc() produce for the device-stack array?
console.log("\n=== Encoding test ===")
const fields = {
  "data-protocol": "ao",
  variant: "ao.TN.1",
  image: "AZ6EXxyOaw3ZjK6htaKA6qZaIh_AbJoJnskGtj7FKZ0",
  "execution-device": "stack@1.0",
  "push-device": "push@1.0",
  "device-stack": ["wasi@1.0", "json-iface@1.0", "wasm-64@1.0", "patch@1.0", "multipass@1.0"],
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

const encoded = await httpsig_to(fields)
console.log("Has body:", encoded.body !== undefined)
console.log("Body type:", typeof encoded.body)
if (encoded.body) {
  const bodyText = typeof encoded.body === "string" ? encoded.body :
    encoded.body instanceof Blob ? await encoded.body.text() : String(encoded.body)
  console.log("Body:", bodyText.slice(0, 500))
}
console.log("Headers:")
for (const [k, v] of Object.entries(encoded.headers)) {
  console.log(`  ${k}: ${String(v).slice(0, 200)}`)
}

// Test: Send via hb.post() (httpsig) with array device-stack
console.log("\n=== POST via httpsig with array device-stack ===")
try {
  const res = await hb.post({
    path: "/~scheduler@1.0/schedule",
    ...fields,
  })
  const pid = res.headers?.process || res.out?.process
  console.log("SPAWN PID:", pid)

  if (pid) {
    console.log("\n--- Schedule ---")
    const sched = await hb.scheduleAOS({ pid, action: "Eval", data: "return 1+1" })
    console.log("slot:", sched.slot)

    console.log("\n--- Compute ---")
    try {
      const comp = await hb.computeAOS({ pid, slot: sched.slot })
      console.log("compute:", JSON.stringify(comp)?.slice(0, 500))
    } catch (e) {
      console.log("compute error:", e.message?.slice(0, 500))
    }
  }
} catch (e) {
  console.log("POST error:", e.message?.slice(0, 500))
}

// Test: Send via hb.post() with all strings (no arrays/numbers)
console.log("\n=== POST via httpsig with ALL STRINGS ===")
try {
  const allStringFields = {
    "data-protocol": "ao",
    variant: "ao.TN.1",
    image: "AZ6EXxyOaw3ZjK6htaKA6qZaIh_AbJoJnskGtj7FKZ0",
    "execution-device": "stack@1.0",
    "push-device": "push@1.0",
    "device-stack": "wasi@1.0, json-iface@1.0, wasm-64@1.0, patch@1.0, multipass@1.0",
    "output-prefix": "wasm",
    "patch-from": "/results/outbox",
    "patch-mode": "patches",
    passes: "2",
    "random-seed": seed(16),
    type: "Process",
    device: "process@1.0",
    scheduler: hb.operator ?? hb.addr,
    nonce: seed(8),
  }

  // Check encoding
  const encoded2 = await httpsig_to(allStringFields)
  console.log("Has body:", encoded2.body !== undefined)

  const res = await hb.post({
    path: "/~scheduler@1.0/schedule",
    ...allStringFields,
  })
  const pid = res.headers?.process || res.out?.process
  console.log("ALL-STRING PID:", pid)

  if (pid) {
    console.log("\n--- Compute ---")
    try {
      const comp = await hb.computeAOS({ pid, slot: 0 })
      console.log("compute:", JSON.stringify(comp)?.slice(0, 500))
    } catch (e) {
      console.log("compute error:", e.message?.slice(0, 500))
    }
  }
} catch (e) {
  console.log("ALL-STRING error:", e.message?.slice(0, 500))
}

// Test: hb.sign() to see actual HTTP request shape
console.log("\n=== Signed request inspection ===")
const signed = await hb.sign({
  path: "/~scheduler@1.0/schedule",
  ...fields,
})
console.log("Method:", signed.method)
console.log("URL:", signed.url)
console.log("Body:", signed.body ? `${signed.body.length || 'blob'} bytes` : "none")
console.log("Headers:")
for (const [k, v] of Object.entries(signed.headers || {})) {
  console.log(`  ${k}: ${String(v).slice(0, 200)}`)
}
