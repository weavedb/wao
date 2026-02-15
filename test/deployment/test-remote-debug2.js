import HB from "../../src/hb.js"
import { readFileSync } from "fs"
import { seed } from "../../src/utils.js"
import { commit, result } from "hbsig"

const REMOTE = "https://push-1.forward.computer"
const jwk = JSON.parse(readFileSync("/home/basque/Downloads/arweave-keyfile-UCzh5BWlNp1cT1eZQjNFBTk5QAjxtiMz4wiceSpKtus.json", "utf-8"))

const hb = new HB({ url: REMOTE, jwk })
await hb.init(jwk)
console.log("addr:", hb.addr)
console.log("operator:", hb.operator)

// Build AOS spawn message manually and capture full error
console.log("\n=== Manual spawnAOS with full error capture ===")
const obj = {
  path: "/~scheduler@1.0/schedule",
  "data-protocol": "ao",
  variant: "ao.TN.1",
  image: "AZ6EXxyOaw3ZjK6htaKA6qZaIh_AbJoJnskGtj7FKZ0",
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
}

// Sign with hb.commit
const committed = await hb.commit(obj, { path: false })
console.log("Committed keys:", Object.keys(committed))
console.log("Has device-stack:", "device-stack" in committed)
console.log("device-stack type:", typeof committed["device-stack"])
console.log("device-stack value:", committed["device-stack"])

// Skip encoding test

// Try via hb.post (the normal path)
console.log("\n--- Via hb.post() ---")
try {
  const res = await hb.post({
    path: "/~scheduler@1.0/schedule",
    ...obj,
  })
  console.log("post result:", JSON.stringify(res)?.slice(0, 300))
} catch (e) {
  console.log("post error:", e.message?.slice(0, 500))
}

// Skip raw fetch test

// Test: What if we flatten device-stack to a string?
console.log("\n--- Flatten device-stack to string ---")
try {
  const flatObj = {
    ...obj,
    "device-stack": "wasi@1.0,json-iface@1.0,wasm-64@1.0,patch@1.0,multipass@1.0",
    "random-seed": seed(16),
    nonce: seed(8),
  }
  const res = await hb.post({
    path: "/~scheduler@1.0/schedule",
    ...flatObj,
  })
  console.log("Flat device-stack result:", JSON.stringify(res)?.slice(0, 300))
  const pid = res.headers?.process || res.out?.process
  console.log("Flat device-stack pid:", pid)
} catch (e) {
  console.log("Flat error:", e.message?.slice(0, 500))
}

// Test: Legacy spawn (no device-stack) — confirms signing works
console.log("\n--- Legacy spawn (no device-stack, no array) ---")
try {
  const legObj = {
    path: "/~scheduler@1.0/schedule",
    "Data-Protocol": "ao",
    Variant: "ao.TN.1",
    Module: "ISShJH1ij-hPPt9St5UFFr_8Ys3Kj5cyg7zrMGt7H9s",
    "execution-device": "genesis-wasm@1.0",
    device: "process@1.0",
    "push-device": "push@1.0",
    Scheduler: hb.operator ?? hb.addr,
    "random-seed": seed(16),
    Type: "Process",
  }
  const res = await hb.post({
    path: "/~scheduler@1.0/schedule",
    ...legObj,
  })
  const pid = res.headers?.process || res.out?.process
  console.log("Legacy pid:", pid)
} catch (e) {
  console.log("Legacy error:", e.message?.slice(0, 500))
}
