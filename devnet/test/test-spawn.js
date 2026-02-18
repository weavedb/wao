/**
 * Test real MU spawn — verifies ao-loader WASM execution works in CF Workers.
 * Usage: node devnet/test/test-spawn.js
 */
import { createDataItemSigner } from "@permaweb/aoconnect-69"
import { ArweaveSigner, createData } from "arbundles"
import { acc } from "../../src/accounts.js"

const PORT = process.env.PORT || 8787
const BASE = `http://localhost:${PORT}`

// Known pre-registered module in armem-base.js
const MODULE = "Do_Uc2Sju_ffp6Ev0AnLVdPtot15rvMjP-a9VVaA5fM" // aos2_0_1

const info = await fetch(`${BASE}/su`).then(r => r.json())
const SCHEDULER = info.Address
console.log(`Scheduler: ${SCHEDULER}`)

// Create a signed DataItem for a Process spawn
const signer = new ArweaveSigner(acc[0].jwk)
const item = createData("", signer, {
  tags: [
    { name: "Data-Protocol", value: "ao" },
    { name: "Variant", value: "ao.TN.1" },
    { name: "Type", value: "Process" },
    { name: "Module", value: MODULE },
    { name: "Scheduler", value: SCHEDULER },
    { name: "Name", value: "TestProcess" },
    { name: "Authority", value: acc[0].addr },
    { name: "Content-Type", value: "text/plain" },
  ],
})
await item.sign(signer)

console.log(`Spawning process via MU with module ${MODULE}...`)
console.log(`DataItem ID: ${item.id}`)

const res = await fetch(`${BASE}/mu`, {
  method: "POST",
  headers: { "Content-Type": "application/octet-stream" },
  body: item.getRaw(),
})

const text = await res.text()
console.log(`Response: ${res.status} ${res.statusText}`)
try {
  console.log(JSON.parse(text))
} catch {
  console.log(text)
}

// Check wrangler logs for errors
if (res.status === 200) {
  console.log("\nSPAWN SUCCESS! ao-loader WASM execution works in CF Workers.")
} else {
  console.log("\nSpawn failed. Check wrangler logs for details.")
}
