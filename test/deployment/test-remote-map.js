import HB from "../../src/hb.js"
import { readFileSync } from "fs"
import { seed } from "../../src/utils.js"
import { result } from "hbsig"

const jwk = JSON.parse(readFileSync("/home/basque/Downloads/arweave-keyfile-UCzh5BWlNp1cT1eZQjNFBTk5QAjxtiMz4wiceSpKtus.json", "utf-8"))
const REMOTE = "https://push-10.forward.computer"
const hb = new HB({ url: REMOTE, jwk })
await hb.init(jwk)

// Attempt: device-stack as numbered map object (what dev_stack actually expects)
console.log("=== device-stack as numbered map object ===")
try {
  const obj = {
    path: "/~scheduler@1.0/schedule",
    "data-protocol": "ao",
    variant: "ao.TN.1",
    image: "AZ6EXxyOaw3ZjK6htaKA6qZaIh_AbJoJnskGtj7FKZ0",
    "execution-device": "stack@1.0",
    "push-device": "push@1.0",
    "device-stack": {
      "1": "wasi@1.0",
      "2": "json-iface@1.0",
      "3": "wasm-64@1.0",
      "4": "patch@1.0",
      "5": "multipass@1.0",
    },
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

  // Check what the signed message looks like
  const signed = await hb.sign(obj)
  console.log("Signed URL:", signed.url)
  console.log("Body length:", signed.body?.length)
  // List the header names
  const headerNames = Object.keys(signed.headers || {}).sort()
  console.log("Header names:", headerNames.join(", "))
  console.log("ao-types:", signed.headers?.["ao-types"])

  const response = await fetch(signed.url, {
    method: signed.method || "POST",
    headers: signed.headers,
    body: signed.body,
  })
  console.log("Status:", response.status)
  if (response.status >= 400) {
    const hdrs = Object.fromEntries(response.headers.entries())
    console.log("Details:", hdrs.details?.slice(0, 1000))
    console.log("Stack:", hdrs.stacktrace?.slice(0, 500))
    const text = await response.text()
    if (text) console.log("Body:", text.slice(0, 500))
  } else {
    const res = await result(response)
    const pid = res.headers?.process || res.out?.process
    console.log("SPAWN OK! pid:", pid)

    if (pid) {
      // Try schedule + compute
      console.log("\n--- Schedule ---")
      const sched = await hb.scheduleAOS({ pid, action: "Eval", data: "return 1+1" })
      console.log("Schedule slot:", sched.slot)

      console.log("\n--- Compute ---")
      try {
        const comp = await hb.computeAOS({ pid, slot: sched.slot })
        console.log("Compute:", JSON.stringify(comp)?.slice(0, 500))

        // Check if it's an actual result
        if (comp && typeof comp === "object") {
          const msgs = Object.keys(comp).filter(k => /^\d+$/.test(k))
          console.log("Outbox messages:", msgs.length)
          for (const k of msgs) {
            console.log(`  [${k}]:`, JSON.stringify(comp[k])?.slice(0, 200))
          }
        }
        console.log("TEST: PASS")
      } catch(e) {
        console.log("Compute error:", e.message?.slice(0, 500))
      }
    }
  }
} catch(e) {
  console.log("Error:", e.message?.slice(0, 500))
  console.log(e.stack?.slice(0, 500))
}
