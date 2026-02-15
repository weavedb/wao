import HyperBEAM from "../../src/hyperbeam.js"
import HB from "../../src/hb.js"
import { AO } from "../../src/index.js"

async function main() {
  console.log("Starting HyperBEAM...")
  const hbeam = await new HyperBEAM({ reset: true }).ready()
  console.log("HyperBEAM ready at", hbeam.url)

  const ao = await new AO({ hb: hbeam.url, mode: "aos" }).init(hbeam.jwk)
  console.log("AO initialized")

  // Step 1: Spawn
  console.log("\n=== Step 1: Spawn ===")
  const { pid, err: spawnErr } = await ao.spwn({})
  console.log("Spawn result:", { pid, spawnErr })
  if (!pid) { hbeam.kill(); process.exit(1) }

  // Step 2: Eval (loads code)
  console.log("\n=== Step 2: Eval ===")
  const luaCode = `
    local count = 0
    Handlers.add("Inc", "Inc", function(msg)
      count = count + 1
      msg.reply({ Data = tostring(count) })
    end)
    Handlers.add("Get", "Get", function(msg)
      msg.reply({ Data = tostring(count) })
    end)
  `
  const evalRes = await ao.eval({ pid, data: luaCode })
  console.log("Eval result:", evalRes?.err || "ok")

  // Step 3: Send Inc
  console.log("\n=== Step 3: Send Inc ===")
  const incRes = await ao.msg({ pid, act: "Inc" })
  console.log("Inc result:", { mid: incRes?.mid, err: incRes?.err })

  // Step 4: Send Get
  console.log("\n=== Step 4: Send Get ===")
  const getRes = await ao.msg({ pid, act: "Get", get: false })
  console.log("Get result:", { mid: getRes?.mid, out: getRes?.out, err: getRes?.err })

  // Check the cache for snapshots
  console.log("\n=== Checking computed slots ===")
  try {
    const slotsRes = await fetch(`${hbeam.url}/${pid}/compute/results/outbox?slot=4`)
    console.log("Compute slot 4:", slotsRes.status)
  } catch (e) {
    console.log("Compute error:", e.message)
  }

  // Cleanup
  setTimeout(() => { hbeam.kill(); process.exit(0) }, 2000)
}

main().catch(e => { console.error("FATAL:", e.message); process.exit(1) })
