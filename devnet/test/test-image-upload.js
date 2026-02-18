/**
 * Test: upload an image via AR endpoint, then fetch it back and verify
 * Content-Type header is returned correctly.
 *
 * Usage: node devnet/test/test-image-upload.js
 */
import _Arweave from "arweave"
const Arweave = _Arweave.default ?? _Arweave
import { readFileSync } from "fs"
import { acc } from "../../src/accounts.js"

const PORT = process.env.PORT || 8787
const BASE = `http://localhost:${PORT}`

const arweave = Arweave.init()
arweave.transactions.getPrice = () => Promise.resolve("0")

async function getAnchor() {
  const info = await fetch(`${BASE}/ar`).then(r => r.json())
  return info.current || "0000000000000000000000000000000000000000000"
}
arweave.transactions.getTransactionAnchor = getAnchor

const owner = acc[0]

// Read the image file
const imgPath = new URL("../../docs/docs/public/images/cover.png", import.meta.url).pathname
const imgData = readFileSync(imgPath)
console.log(`Image size: ${imgData.length} bytes`)

// Create and post a transaction with the image data
const tx = await arweave.createTransaction({ data: imgData })
tx.addTag("Content-Type", "image/png")
tx.addTag("App-Name", "WAO-Explorer-Test")
tx.addTag("Name", "cover.png")
await arweave.transactions.sign(tx, owner.jwk)

const body = tx.toJSON ? tx.toJSON() : JSON.parse(JSON.stringify(tx))
const postRes = await fetch(`${BASE}/ar/tx`, {
  method: "POST",
  headers: { "Content-Type": "application/json" },
  body: JSON.stringify(body),
})
const postJson = await postRes.json()
console.log(`POST /ar/tx status: ${postRes.status}`, postJson)
console.log(`Transaction ID: ${tx.id}`)

// Now fetch it back via GET /ar/:id
const getRes = await fetch(`${BASE}/ar/${tx.id}`)
console.log(`\nGET /ar/${tx.id}`)
console.log(`  Status: ${getRes.status}`)
console.log(`  Content-Type: ${getRes.headers.get("content-type")}`)
const blob = await getRes.blob()
console.log(`  Body size: ${blob.size} bytes`)

// Verify
const ct = getRes.headers.get("content-type") || ""
if (ct.includes("image/png")) {
  console.log("\n  Content-Type header is correct!")
} else {
  console.log(`\n  UNEXPECTED Content-Type: "${ct}" (expected image/png)`)
}

// Also verify via GraphQL that data.type is returned
const gqlRes = await fetch(`${BASE}/ar/graphql`, {
  method: "POST",
  headers: { "Content-Type": "application/json" },
  body: JSON.stringify({
    query: `{ transactions(ids: ["${tx.id}"]) { edges { node { id data { size type } tags { name value } } } } }`,
  }),
})
const gqlJson = await gqlRes.json()
const node = gqlJson.data?.transactions?.edges?.[0]?.node
if (node) {
  console.log(`\nGQL data.type: "${node.data?.type}"`)
  console.log(`GQL data.size: "${node.data?.size}"`)
} else {
  console.log("\nGQL: transaction not found")
}

console.log("\nDone!")
