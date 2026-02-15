import { HyperBEAM } from "../../src/test.js"
import AO from "../../src/ao.js"
import HB from "../../src/hb.js"

const hbeam = await new HyperBEAM({
  reset: true,
  genesis_wasm: true,
  force_signed: true,
}).ready()
console.log("HyperBEAM ready at", hbeam.url, "(force_signed: true)")

// Test 1: Legacy spawn (Mode 4) — should work since it uses httpsig flat tags
console.log("\n=== Test 1: Legacy spawn (genesis-wasm) with force_signed ===")
try {
  const ao = new AO({ hb: hbeam.url })
  await ao.init(hbeam.jwk)
  const { pid } = await ao.spwn({})
  console.log("Legacy spawn pid:", pid)
  const { res } = await ao.msg({ pid, act: "Eval", data: "return 1+1" })
  const data = res?.Messages?.[0]?.Data ?? res?.Output?.data
  console.log("Eval result:", data)
  console.log("TEST 1:", data ? "PASS" : "PARTIAL (spawn ok, compute may differ)")
} catch (e) {
  console.log("TEST 1: FAIL -", e.message?.slice(0, 500))
}

// Test 2: AOS spawn (Mode 6) — this is the one that fails on remote
console.log("\n=== Test 2: AOS spawn (wasm-64/stack@1.0) with force_signed ===")
try {
  const ao = new AO({ hb: hbeam.url, mode: "aos" })
  await ao.init(hbeam.jwk)
  const { pid } = await ao.spwn({})
  console.log("AOS spawn pid:", pid)
  const { res } = await ao.msg({ pid, act: "Eval", data: "return 1+1" })
  console.log("Eval result:", JSON.stringify(res)?.slice(0, 300))
  console.log("TEST 2: PASS")
} catch (e) {
  console.log("TEST 2: FAIL -", e.message?.slice(0, 500))
  console.log(e.stack?.slice(0, 500))
}

// Test 3: Direct HB spawnAOS — see raw error details
console.log("\n=== Test 3: Direct hb.spawnAOS() with force_signed ===")
try {
  const hb = new HB({ url: hbeam.url, jwk: hbeam.jwk })
  await hb.init(hbeam.jwk)
  const { pid } = await hb.spawnAOS()
  console.log("Direct spawnAOS pid:", pid)

  // Try scheduling
  const sched = await hb.scheduleAOS({ pid, action: "Eval", data: "return 2+2" })
  console.log("Schedule slot:", sched.slot)

  // Try compute
  const comp = await hb.computeAOS({ pid, slot: sched.slot })
  console.log("Compute:", JSON.stringify(comp)?.slice(0, 300))
  console.log("TEST 3: PASS")
} catch (e) {
  console.log("TEST 3: FAIL -", e.message?.slice(0, 500))
}

// Test 4: Direct fetch to see raw HTTP response for AOS spawn
console.log("\n=== Test 4: Raw fetch spawnAOS to see exact error ===")
try {
  const hb = new HB({ url: hbeam.url, jwk: hbeam.jwk })
  await hb.init(hbeam.jwk)

  // Build the same message spawnAOS would build, then POST manually
  const { seed: seedFn } = await import("../../src/utils.js")
  const { commit, result } = await import("hbsig")

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
    "random-seed": seedFn(16),
    type: "Process",
    device: "process@1.0",
    scheduler: hb.operator ?? hb.addr,
  }

  // Sign with httpsig
  const committed = await hb.commit(obj, { path: false })
  console.log("Committed keys:", Object.keys(committed))

  // Try multipart POST (this is what hb.post() does)
  const response = await fetch(`${hbeam.url}/~scheduler@1.0/schedule`, {
    method: "POST",
    headers: { "accept-bundle": "true" },
    body: committed._body ?? committed.body,
  })
  console.log("Status:", response.status)
  if (response.status >= 400) {
    const text = await response.text()
    console.log("Error body:", text.slice(0, 500))
    const hdrs = Object.fromEntries(response.headers.entries())
    console.log("Error headers:", JSON.stringify(hdrs).slice(0, 500))
  } else {
    const res = await result(response)
    console.log("Success! Process:", res.headers?.process || res.out?.process)
    console.log("TEST 4: PASS")
  }
} catch (e) {
  console.log("TEST 4: FAIL -", e.message?.slice(0, 500))
}

hbeam.kill()
