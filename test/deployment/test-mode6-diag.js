/**
 * Diagnostic: Mode 6 deploy flow step-by-step
 */
import AO from "../../src/ao.js"
import HyperBEAM from "../../src/hyperbeam.js"

const hbeam = await new HyperBEAM({ reset: true }).ready()
console.log("HB started at", hbeam.url)

const ao = await new AO({ hb: hbeam.url, mode: "aos" }).init(hbeam.jwk)
console.log("AO initialized, mode:", ao.mode)

// Step 1: spawnAOS
console.log("\n=== Step 1: spawnAOS ===")
const t0 = Date.now()
try {
  const { pid } = await ao.spwn()
  console.log("Spawn OK:", pid, `(${Date.now() - t0}ms)`)

  // Step 2: scheduleAOS
  console.log("\n=== Step 2: scheduleAOS ===")
  const t1 = Date.now()
  const sched = await ao.hb.scheduleAOS({
    pid,
    action: "Eval",
    data: "return 1+1",
  })
  console.log("Schedule OK, slot:", sched.slot, `(${Date.now() - t1}ms)`)

  // Step 3: computeAOS
  console.log("\n=== Step 3: computeAOS ===")
  const t2 = Date.now()
  try {
    const comp = await ao.hb.computeAOS({ pid, slot: sched.slot })
    console.log("Compute OK:", typeof comp, `(${Date.now() - t2}ms)`)
    console.log("Compute result:", JSON.stringify(comp)?.slice(0, 200))
  } catch (e) {
    console.log("Compute FAILED:", e.message, `(${Date.now() - t2}ms)`)
  }

  // Step 4: eval (which uses load -> pipe)
  console.log("\n=== Step 4: ao.eval ===")
  const t3 = Date.now()
  try {
    const evalR = await ao.eval({ pid, data: "return 42" })
    console.log("Eval OK:", JSON.stringify(evalR)?.slice(0, 200), `(${Date.now() - t3}ms)`)
  } catch (e) {
    console.log("Eval FAILED:", e.message, `(${Date.now() - t3}ms)`)
  }

  // Step 5: deploy (full flow)
  console.log("\n=== Step 5: ao.deploy ===")
  const t4 = Date.now()
  try {
    const { p, pid: pid2, err } = await ao.deploy({
      src_data: `Handlers.add("Hi","Hi",function(msg) msg.reply({Data="hello"}) end)`,
    })
    console.log("Deploy:", err ? `FAILED: ${err}` : `OK pid=${pid2}`, `(${Date.now() - t4}ms)`)
    if (p) {
      const r = await ao.msg({ pid: pid2, act: "Hi" })
      console.log("Msg result:", r.out || JSON.stringify(r.res)?.slice(0, 200))
    }
  } catch (e) {
    console.log("Deploy FAILED:", e.message, `(${Date.now() - t4}ms)`)
  }
} catch (e) {
  console.log("FAILED:", e.message, `(${Date.now() - t0}ms)`)
}

hbeam.kill()
process.exit(0)
