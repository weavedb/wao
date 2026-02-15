import HB from "../../src/hb.js"
import { readFileSync } from "fs"

const REMOTE = "https://push-1.forward.computer"
const jwk = JSON.parse(readFileSync("/home/basque/Downloads/arweave-keyfile-UCzh5BWlNp1cT1eZQjNFBTk5QAjxtiMz4wiceSpKtus.json", "utf-8"))

const hb = new HB({ url: REMOTE, jwk })
await hb.init(jwk)
console.log("HB addr:", hb.addr)
console.log("HB operator:", hb.operator)

// Test 1: spawnLegacy (known working)
console.log("\n=== Test 1: spawnLegacy on remote ===")
try {
  const { pid } = await hb.spawnLegacy({})
  console.log("Legacy pid:", pid)
  console.log("TEST 1: PASS")
} catch (e) {
  console.log("TEST 1: FAIL -", e.message?.slice(0, 500))
}

// Test 2: spawnAOS on remote
console.log("\n=== Test 2: spawnAOS on remote ===")
try {
  const { pid } = await hb.spawnAOS()
  console.log("AOS pid:", pid)
  console.log("TEST 2: PASS")

  // If spawn succeeds, try schedule + compute
  console.log("\n--- Scheduling Eval ---")
  const sched = await hb.scheduleAOS({ pid, action: "Eval", data: "return 1+1" })
  console.log("Schedule slot:", sched.slot)

  console.log("\n--- Computing ---")
  try {
    const comp = await hb.computeAOS({ pid, slot: sched.slot })
    console.log("Compute result:", JSON.stringify(comp)?.slice(0, 500))
  } catch (e) {
    console.log("Compute error:", e.message?.slice(0, 300))
  }
} catch (e) {
  console.log("TEST 2: FAIL -", e.message?.slice(0, 500))
  if (e.cause) console.log("Cause:", e.cause)
}

// Test 3: spawnAOS on other nodes
for (const n of [2, 3, 5]) {
  const url = `https://push-${n}.forward.computer`
  console.log(`\n=== Test 3.${n}: spawnAOS on ${url} ===`)
  try {
    const hb2 = new HB({ url, jwk })
    await hb2.init(jwk)
    const { pid } = await hb2.spawnAOS()
    console.log(`push-${n} AOS pid:`, pid)
    console.log(`TEST 3.${n}: PASS`)
  } catch (e) {
    console.log(`TEST 3.${n}: FAIL -`, e.message?.slice(0, 300))
  }
}

// Test 4: Check remote node info
console.log("\n=== Test 4: Remote node info ===")
try {
  const infoRes = await fetch(`${REMOTE}/~meta@1.0/info`)
  const info = await infoRes.text()
  console.log("Node info:", info.slice(0, 1000))
} catch (e) {
  console.log("Info error:", e.message?.slice(0, 300))
}
