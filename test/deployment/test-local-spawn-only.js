import HB from "../../src/hb.js"
import HyperBEAM from "../../src/hyperbeam.js"
import { seed, srcs, buildTags } from "../../src/utils.js"
import { ArweaveSigner } from "@ar.io/sdk"
import { createData } from "@dha-team/arbundles"
import { result } from "hbsig"

console.log("=== Starting local HyperBEAM ===")
const hbeam = await new HyperBEAM({ reset: true }).ready()
console.log("HyperBEAM started at", hbeam.url)

const hb = new HB({ url: hbeam.url, jwk: hbeam.jwk })
await hb.init(hbeam.jwk)
console.log("addr:", hb.addr)

async function sendANS104(path, tags, data = "1984") {
  const _tags = buildTags({ ...tags, signingFormat: "ANS-104" })
  const signer = new ArweaveSigner(hbeam.jwk)
  const item = createData(data, signer, { tags: _tags })
  await item.sign(signer)
  const res = await fetch(`${hbeam.url}${path}`, {
    method: "POST",
    headers: { "codec-device": "ans104@1.0", "Content-Type": "application/ans104" },
    body: item.binary,
  })
  console.log("Status:", res.status)
  if (res.status >= 400) {
    const hdrs = Object.fromEntries(res.headers.entries())
    console.log("Error details:", hdrs.details?.slice(0, 500))
    console.log("Stacktrace:", hdrs.stacktrace?.slice(0, 500))
    return null
  }
  return await result(res)
}

await hb.getImage()
const image = hb.image
console.log("image:", image?.slice(0, 20))

// Test 1: OLD format (device-stack/N flat tags)
console.log("\n=== Test 1: OLD format (device-stack/N flat tags) ===")
try {
  const res1 = await sendANS104("/~scheduler@1.0/schedule", {
    "data-protocol": "ao",
    variant: "ao.TN.1",
    image,
    "execution-device": "stack@1.0",
    "push-device": "push@1.0",
    "device-stack/1": "wasi@1.0",
    "device-stack/2": "json-iface@1.0",
    "device-stack/3": "wasm-64@1.0",
    "device-stack/4": "patch@1.0",
    "device-stack/5": "multipass@1.0",
    "output-prefix": "wasm",
    "patch-from": "/results/outbox",
    "patch-mode": "patches",
    passes: "2",
    "random-seed": seed(16),
    type: "Process",
    device: "process@1.0",
    scheduler: hb.operator ?? hb.addr,
    authority: hb.addr,
  })
  if (res1) {
    const pid = res1.headers?.process || res1.out?.process
    console.log("PID:", pid)
    console.log("TEST 1: SPAWN PASS")
  } else {
    console.log("TEST 1: SPAWN FAIL")
  }
} catch (e) {
  console.log("Error:", e.message?.slice(0, 300))
  console.log("TEST 1: SPAWN FAIL")
}

// Test 2: NEW format (ao-types device-stack=map)
console.log("\n=== Test 2: NEW format (ao-types device-stack=map) ===")
try {
  const res2 = await sendANS104("/~scheduler@1.0/schedule", {
    "data-protocol": "ao",
    variant: "ao.TN.1",
    image,
    "execution-device": "stack@1.0",
    "push-device": "push@1.0",
    "device-stack": '1="wasi@1.0", 2="json-iface@1.0", 3="wasm-64@1.0", 4="patch@1.0", 5="multipass@1.0"',
    "output-prefix": "wasm",
    "patch-from": "/results/outbox",
    "patch-mode": "patches",
    passes: "2",
    "random-seed": seed(16),
    type: "Process",
    device: "process@1.0",
    scheduler: hb.operator ?? hb.addr,
    authority: hb.addr,
    "ao-types": 'device-stack="map", passes="integer"',
  })
  if (res2) {
    const pid = res2.headers?.process || res2.out?.process
    console.log("PID:", pid)
    console.log("TEST 2: SPAWN PASS")
  } else {
    console.log("TEST 2: SPAWN FAIL")
  }
} catch (e) {
  console.log("Error:", e.message?.slice(0, 300))
  console.log("TEST 2: SPAWN FAIL")
}

// Test 3: hb.spawnAOS() (uses updated code)
console.log("\n=== Test 3: hb.spawnAOS() ===")
try {
  const { pid } = await hb.spawnAOS()
  console.log("PID:", pid)
  console.log("TEST 3: SPAWN PASS")
} catch (e) {
  console.log("Error:", e.message?.slice(0, 300))
  console.log("TEST 3: SPAWN FAIL")
}

console.log("\n=== Done ===")
hbeam.kill()
