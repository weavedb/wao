import HB from "../../src/hb.js"
import { readFileSync } from "fs"
import { seed } from "../../src/utils.js"
import { commit, result } from "hbsig"

const REMOTE = "https://push-1.forward.computer"
const MODULE = "AZ6EXxyOaw3ZjK6htaKA6qZaIh_AbJoJnskGtj7FKZ0"

const jwk = JSON.parse(readFileSync("/home/basque/Downloads/arweave-keyfile-UCzh5BWlNp1cT1eZQjNFBTk5QAjxtiMz4wiceSpKtus.json", "utf-8"))

const hb = new HB({ url: REMOTE, jwk })
await hb.init(jwk)

// Test: Force JSON commit path with array device-stack
console.log("=== JSON commit path with array device-stack ===")
try {
  const obj = {
    path: "/~scheduler@1.0/schedule",
    "data-protocol": "ao",
    variant: "ao.TN.1",
    image: MODULE,
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

  // Use the commit path directly (normally only used for flat messages)
  const committed = await hb.commit(obj, { path: false })
  const jsonBody = JSON.stringify(committed, (key, value) => {
    if (value?.type === "Buffer" && Array.isArray(value?.data))
      return Buffer.from(value.data).toString("base64")
    if (Buffer.isBuffer(value)) return value.toString("base64")
    return value
  })
  console.log("JSON body (truncated):", jsonBody.slice(0, 500))

  const response = await fetch(`${REMOTE}${obj.path}`, {
    method: "POST",
    headers: { "content-type": "application/json", "accept-bundle": "true" },
    body: jsonBody,
  })
  console.log("Status:", response.status)
  if (response.status >= 400) {
    const hdrs = Object.fromEntries(response.headers.entries())
    console.log("Error details:", hdrs.details?.slice(0, 500))
    console.log("Stacktrace:", hdrs.stacktrace?.slice(0, 300))
  } else {
    const res = await result(response)
    const pid = res.headers?.process || res.out?.process
    console.log("Spawn OK! pid:", pid)

    if (pid) {
      // Schedule + compute
      const sched = await hb.scheduleAOS({ pid, action: "Eval", data: "return 1+1" })
      console.log("Schedule OK, slot:", sched.slot)

      try {
        const comp = await hb.computeAOS({ pid, slot: sched.slot })
        console.log("Compute:", JSON.stringify(comp)?.slice(0, 500))
      } catch (e) {
        console.log("Compute error:", e.message?.slice(0, 300))
      }

      // Counter test
      const load = await hb.messageAOS({ pid, action: "Eval", data: `local c=0 Handlers.add("Inc","Inc",function(m) c=c+1 m.reply({Data=tostring(c)}) end)` })
      console.log("Load slot:", load.slot)

      const inc = await hb.messageAOS({ pid, action: "Inc" })
      console.log("Inc outbox:", JSON.stringify(inc.outbox)?.slice(0, 300))

      const data = inc.outbox?.["1"]?.data ?? inc.outbox?.["1"]?.Data
      console.log("Counter:", data)
      if (data === "1") console.log("COUNTER: PASS")
    }
  }
} catch (e) {
  console.log("Error:", e.message?.slice(0, 500))
  console.log(e.stack?.slice(0, 500))
}
