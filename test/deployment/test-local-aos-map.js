import HB from "../../src/hb.js"
import HyperBEAM from "../../src/hyperbeam.js"
import { readFileSync } from "fs"

console.log("=== Starting local HyperBEAM ===")
const hbeam = await new HyperBEAM({ reset: true }).ready()
console.log("HyperBEAM started at", hbeam.url)

try {
  const hb = new HB({ url: hbeam.url, jwk: hbeam.jwk })
  await hb.init(hbeam.jwk)
  console.log("addr:", hb.addr)

  // Test 1: spawnAOS
  console.log("\n=== Test 1: spawnAOS ===")
  const { pid } = await hb.spawnAOS()
  console.log("PID:", pid)

  // Test 2: Schedule + Compute
  console.log("\n=== Test 2: Schedule Eval ===")
  const sched = await hb.scheduleAOS({ pid, action: "Eval", data: "return 1+1" })
  console.log("slot:", sched.slot)

  console.log("\n=== Test 3: Compute ===")
  try {
    const comp = await hb.computeAOS({ pid, slot: sched.slot })
    console.log("compute result:", JSON.stringify(comp)?.slice(0, 500))
    console.log("TEST: PASS")
  } catch (e) {
    console.log("compute error:", e.message?.slice(0, 500))
    console.log("TEST: FAIL")
  }

  // Test 4: msgAOS (full round-trip)
  console.log("\n=== Test 4: msgAOS ===")
  try {
    const msg = await hb.msgAOS({ pid, action: "Eval", data: "return 2+3" })
    console.log("msgAOS result:", JSON.stringify(msg)?.slice(0, 500))
    console.log("TEST: PASS")
  } catch (e) {
    console.log("msgAOS error:", e.message?.slice(0, 500))
    console.log("TEST: FAIL")
  }

} finally {
  console.log("\n=== Stopping HyperBEAM ===")
  hbeam.kill()
  console.log("Done")
}
