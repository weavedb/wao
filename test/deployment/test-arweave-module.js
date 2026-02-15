import { HyperBEAM } from "../../src/test.js"
import HB from "../../src/hb.js"

const hbeam = await new HyperBEAM({ reset: true, genesis_wasm: true }).ready()
console.log("HyperBEAM ready at", hbeam.url)

const hb = new HB({ url: hbeam.url, jwk: hbeam.jwk })
await hb.setInfo()

// WASI-compiled AOS WASM uploaded from bundled aos_wamr
const MODULE = "AZ6EXxyOaw3ZjK6htaKA6qZaIh_AbJoJnskGtj7FKZ0"
console.log("Spawning with Arweave module:", MODULE)

try {
  const { pid, slot } = await hb.spawnAOS({ module: MODULE })
  console.log("Spawn OK! pid:", pid, "slot:", slot)

  // Schedule + compute a simple eval
  console.log("Scheduling eval...")
  const sched = await hb.scheduleAOS({ pid, action: "Eval", data: "return 1+1" })
  console.log("Scheduled slot:", sched.slot)

  console.log("Computing...")
  const res = await hb.computeAOS({ pid, slot: sched.slot })
  console.log("Compute result:", JSON.stringify(res)?.slice(0, 300))
} catch (e) {
  console.log("Error:", e.message?.slice(0, 500))
}

hbeam.kill()
