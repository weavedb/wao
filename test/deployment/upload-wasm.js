import Arweave from "arweave"
import { readFileSync } from "fs"
import aos_wamr from "../../src/aos_wamr.js"

// Load wallet
const jwk = JSON.parse(readFileSync("/home/basque/Downloads/arweave-keyfile-UCzh5BWlNp1cT1eZQjNFBTk5QAjxtiMz4wiceSpKtus.json", "utf-8"))

// Decode the bundled WASM
const wasm = Buffer.from(aos_wamr, "base64")
console.log("WASM size:", wasm.length, "bytes")
console.log("WASM magic:", wasm.slice(0, 4).toString("hex"))

// Upload via native Arweave transaction
const arweave = Arweave.init({ host: "arweave.net", port: 443, protocol: "https" })
const addr = await arweave.wallets.jwkToAddress(jwk)
const balance = await arweave.wallets.getBalance(addr)
console.log("Wallet:", addr, "| Balance:", arweave.ar.winstonToAr(balance), "AR")

const tx = await arweave.createTransaction({ data: wasm })
tx.addTag("Content-Type", "application/wasm")
tx.addTag("Module-Format", "wasm64-wasi-preview1")
tx.addTag("Data-Protocol", "ao")
tx.addTag("Type", "Module")
tx.addTag("Variant", "ao.TN.1")
tx.addTag("Name", "aos-wamr")
tx.addTag("Description", "AOS WASM binary compiled for WAMR (WASI imports). Compatible with HyperBEAM wasm-64@1.0 device stack.")

console.log("TX reward:", arweave.ar.winstonToAr(tx.reward), "AR")
console.log("Signing...")
await arweave.transactions.sign(tx, jwk)

console.log("Uploading TX:", tx.id, "...")
const res = await arweave.transactions.post(tx)
console.log("Upload status:", res.status, res.statusText)

if (res.status === 200) {
  console.log("\nSUCCESS!")
  console.log("TX ID:", tx.id)
  console.log("URL: https://arweave.net/" + tx.id)
  console.log("Use with: hb.spawnAOS({ module: '" + tx.id + "' })")
} else {
  console.log("FAILED:", JSON.stringify(res.data))
}
