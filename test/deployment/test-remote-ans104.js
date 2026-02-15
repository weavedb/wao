import HB from "../../src/hb.js"
import { readFileSync } from "fs"

const jwk = JSON.parse(readFileSync("/home/basque/Downloads/arweave-keyfile-UCzh5BWlNp1cT1eZQjNFBTk5QAjxtiMz4wiceSpKtus.json", "utf-8"))

// Test 1: Remote push-10
console.log("=== Test 1: spawnAOS on push-10 (ANS-104) ===")
try {
  const hb = new HB({ url: "https://push-10.forward.computer", jwk })
  await hb.init(jwk)
  const { pid } = await hb.spawnAOS()
  console.log("pid:", pid)

  // Schedule + compute
  const sched = await hb.scheduleAOS({ pid, action: "Eval", data: "return 1+1" })
  console.log("slot:", sched.slot)
  try {
    const comp = await hb.computeAOS({ pid, slot: sched.slot })
    console.log("compute:", JSON.stringify(comp)?.slice(0, 500))
    console.log("TEST 1: PASS")
  } catch(e) {
    console.log("compute error:", e.message?.slice(0, 300))
    console.log("TEST 1: PARTIAL (spawn+schedule OK, compute failed)")
  }
} catch(e) {
  console.log("TEST 1: FAIL -", e.message?.slice(0, 500))
}

// Test 2: Remote push-1
console.log("\n=== Test 2: spawnAOS on push-1 (ANS-104) ===")
try {
  const hb = new HB({ url: "https://push-1.forward.computer", jwk })
  await hb.init(jwk)
  const { pid } = await hb.spawnAOS()
  console.log("pid:", pid)
  console.log("TEST 2: PASS (spawn)")
} catch(e) {
  console.log("TEST 2: FAIL -", e.message?.slice(0, 500))
}

// Test 3: Remote push (primary)
console.log("\n=== Test 3: spawnAOS on push (primary) ===")
try {
  const hb = new HB({ url: "https://push.forward.computer", jwk })
  await hb.init(jwk)
  const { pid } = await hb.spawnAOS()
  console.log("pid:", pid)
  console.log("TEST 3: PASS (spawn)")
} catch(e) {
  console.log("TEST 3: FAIL -", e.message?.slice(0, 500))
}
