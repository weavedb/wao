/**
 * Publish a Scheduler-Location transaction to the devnet.
 *
 * This registers the SU wallet as a scheduler at the given URL so that
 * processes spawned with this scheduler address can locate the SU endpoint.
 *
 * Usage:
 *   node devnet/scripts/publish-scheduler.js [--url <SU_URL>] [--port <PORT>]
 *
 * Options:
 *   --url   Full SU URL (default: http://localhost:<PORT>/su)
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
const BASE = getArg("url", null) ? null : `http://localhost:${PORT}`
const SU_URL = getArg("url", `${BASE}/su`)
const MU_URL = `${BASE || new URL(SU_URL).origin.replace("/su", "")}/mu`

const arweave = Arweave.init()

const signer = new ArweaveSigner(suAcc.jwk)

const tags = [
  { name: "Data-Protocol", value: "ao" },
  { name: "Variant", value: "ao.TN.1" },
  { name: "Type", value: "Scheduler-Location" },
  { name: "Url", value: SU_URL },
  { name: "Time-To-Live", value: String(1000 * 60 * 60 * 24 * 365) },
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

console.log("Scheduler-Location published")
console.log(`  Address: ${suAcc.addr}`)
console.log(`  URL:     ${SU_URL}`)
console.log(`  TX:      ${item.id}`)
