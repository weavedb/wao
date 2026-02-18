/**
 * Test real MU message — verifies ao-loader handles messages on a spawned process.
 * Usage: node devnet/test/test-message.js
 */
import { ArweaveSigner, createData } from "arbundles"
import { acc } from "../../src/accounts.js"

const PORT = process.env.PORT || 8787
const BASE = `http://localhost:${PORT}`

// Known pre-registered module in armem-base.js
const MODULE = "Do_Uc2Sju_ffp6Ev0AnLVdPtot15rvMjP-a9VVaA5fM"

// Step 1: Spawn a process via MU
console.log("Step 1: Spawning process via MU...")
const signer = new ArweaveSigner(acc[0].jwk)

const info = await fetch(`${BASE}/su`).then(r => r.json())
const SCHEDULER = info.Address

const spawnItem = createData("", signer, {
  tags: [
    { name: "Data-Protocol", value: "ao" },
    { name: "Variant", value: "ao.TN.1" },
    { name: "Type", value: "Process" },
    { name: "Module", value: MODULE },
    { name: "Scheduler", value: SCHEDULER },
    { name: "Name", value: "TestMsgProcess" },
    { name: "Authority", value: acc[0].addr },
    { name: "Content-Type", value: "text/plain" },
  ],
})
await spawnItem.sign(signer)

const spawnRes = await fetch(`${BASE}/mu`, {
  method: "POST",
  headers: { "Content-Type": "application/octet-stream" },
  body: spawnItem.getRaw(),
})
const spawnJson = await spawnRes.json()
console.log(`  Spawn: ${spawnRes.status}`, spawnJson)
if (spawnRes.status !== 200) process.exit(1)
const processId = spawnJson.id

// Step 2: Send a message to the process
console.log(`\nStep 2: Sending Eval message to process ${processId}...`)
const msgItem = createData('return "Hello from AO devnet!"', signer, {
  target: processId,
  tags: [
    { name: "Data-Protocol", value: "ao" },
    { name: "Variant", value: "ao.TN.1" },
    { name: "Type", value: "Message" },
    { name: "Action", value: "Eval" },
    { name: "Content-Type", value: "text/plain" },
  ],
})
await msgItem.sign(signer)

const msgRes = await fetch(`${BASE}/mu`, {
  method: "POST",
  headers: { "Content-Type": "application/octet-stream" },
  body: msgItem.getRaw(),
})
const msgJson = await msgRes.text()
console.log(`  Message: ${msgRes.status}`, msgJson)

// Step 3: Check the result via CU
console.log(`\nStep 3: Getting result from CU...`)
const resultRes = await fetch(`${BASE}/cu/result/${msgItem.id}?process-id=${processId}`)
const result = await resultRes.json()
console.log(`  Result status: ${resultRes.status}`)
console.log(`  Output:`, JSON.stringify(result.Output?.data, null, 2)?.slice(0, 200))
console.log(`  Messages:`, result.Messages?.length ?? 0)
console.log(`  Spawns:`, result.Spawns?.length ?? 0)

if (resultRes.status === 200) {
  console.log("\nMESSAGE SUCCESS! Full ao-loader cycle works.")
} else {
  console.log("\nMessage execution issue. Check details above.")
}
