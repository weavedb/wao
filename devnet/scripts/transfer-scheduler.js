/**
 * Transfer scheduler ownership to a new address.
 *
 * Posts a Scheduler-Transfer transaction signed by the current scheduler
 * wallet, transferring scheduling authority to a new address.
 *
 * Usage:
 *   node devnet/scripts/transfer-scheduler.js --to <ADDRESS> [--port <PORT>]
 *
 * Options:
 *   --to    New scheduler owner address (required)
 *   --port  Devnet port (default: 8788)
 */
import _Arweave from "arweave"
const Arweave = _Arweave.default ?? _Arweave
import { createData, ArweaveSigner } from "arbundles"
import { su as suAcc } from "../../src/accounts.js"

const args = process.argv.slice(2)
function getArg(name, fallback) {
  const i = args.indexOf(`--${name}`)
  return i !== -1 && args[i + 1] ? args[i + 1] : fallback
}

const PORT = getArg("port", "8788")
const BASE = `http://localhost:${PORT}`
const MU_URL = `${BASE}/mu`
const newOwner = getArg("to", null)

if (!newOwner) {
  console.error("Usage: node transfer-scheduler.js --to <ADDRESS> [--port <PORT>]")
  process.exit(1)
}

const arweave = Arweave.init()

const signer = new ArweaveSigner(suAcc.jwk)

const tags = [
  { name: "Data-Protocol", value: "ao" },
  { name: "Variant", value: "ao.TN.1" },
  { name: "Type", value: "Scheduler-Transfer" },
  { name: "New-Owner", value: newOwner },
]

const item = createData("", signer, { tags })
await item.sign(signer)

const res = await fetch(MU_URL, {
  method: "POST",
  headers: { "Content-Type": "application/octet-stream" },
  body: item.getRaw(),
})

if (!res.ok) {
  console.error("Failed:", res.status, await res.text())
  process.exit(1)
}

console.log("Scheduler-Transfer published")
console.log(`  From: ${suAcc.addr}`)
console.log(`  To:   ${newOwner}`)
console.log(`  TX:   ${item.id}`)
