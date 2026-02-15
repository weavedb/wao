import HyperBEAM from "../../src/hyperbeam.js"
import HB from "../../src/hb.js"
import * as WarpArBundles from "warp-arbundles"
const pkg = WarpArBundles.default ?? WarpArBundles
const { createData, ArweaveSigner } = pkg
import { buildTags } from "../../src/utils.js"
import { mergeLeft } from "ramda"

async function main() {
  console.log("Starting HyperBEAM...")
  const hbeam = await new HyperBEAM({ reset: true }).ready()
  console.log("HyperBEAM ready at", hbeam.url)

  const hb = new HB({ url: hbeam.url })
  await hb.init(hbeam.jwk)
  console.log("HB initialized, addr:", hb.addr)
  console.log("Image:", hb.image)

  // Send raw fetch to see full response
  const tags = {
    "data-protocol": "ao",
    variant: "ao.TN.1",
    type: "Process",
    device: "process@1.0",
    "execution-device": "stack@1.0",
    image: hb.image,
    scheduler: hb.addr,
    "device-stack": '1="wasi@1.0", 2="json-iface@1.0", 3="wasm-64@1.0", 4="patch@1.0", 5="multipass@1.0"',
    "ao-types": 'device-stack="map", passes="integer"',
    passes: 2,
    "output-prefix": "wasm",
    "patch-from": "/results/outbox",
    "patch-mode": "patches",
    "push-device": "push@1.0",
    "random-seed": "abc123",
    signingFormat: "ANS-104",
  }

  const _tags = buildTags(tags)
  console.log("\nTags:", JSON.stringify(_tags, null, 2))

  const signer = new ArweaveSigner(hbeam.jwk)
  const item = createData("1984", signer, { tags: _tags })
  await item.sign(signer)

  const res = await fetch(`${hbeam.url}/~scheduler@1.0/schedule`, {
    method: "POST",
    headers: {
      "codec-device": "ans104@1.0",
      "Content-Type": "application/ans104",
    },
    body: item.binary,
  })

  console.log("\nResponse status:", res.status)
  console.log("Response headers:", Object.fromEntries(res.headers.entries()))
  const body = await res.text()
  console.log("Response body:", body.substring(0, 1000))

  // Cleanup
  setTimeout(() => { hbeam.kill(); process.exit(0) }, 2000)
}

main().catch(e => { console.error(e); process.exit(1) })
