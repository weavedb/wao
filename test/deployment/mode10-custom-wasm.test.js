import { describe, it, before, after } from "node:test"
import assert from "node:assert"
import { readFileSync } from "node:fs"
import { resolve } from "node:path"
import { createData, ArweaveSigner } from "arbundles"
import { HyperBEAM } from "../../src/test.js"
import HB from "../../src/hb.js"
import { REMOTE_NODES } from "./shared.js"

const wasmPathRust = resolve(
  import.meta.dirname,
  "../../custom-wasm/target/wasm64-unknown-unknown/release/custom_wasm.wasm",
)
const wasmPath = wasmPathRust

const jwk = JSON.parse(
  readFileSync(resolve(import.meta.dirname, "../../.wallet.json"), "utf8"),
)

// Upload WASM to Arweave via Turbo (free for < 100KB)
async function uploadToArweave(wasm) {
  const signer = new ArweaveSigner(jwk)
  const dataItem = createData(wasm, signer, {
    tags: [{ name: "Content-Type", value: "application/wasm" }],
  })
  await dataItem.sign(signer)
  const res = await fetch("https://upload.ardrive.io/v1/tx", {
    method: "POST",
    headers: { "content-type": "application/octet-stream" },
    body: dataItem.getRaw(),
  })
  if (res.status !== 200) throw new Error(`Turbo upload failed: ${res.status}`)
  const json = await res.json()
  return json.id
}

describe("Mode 10: Custom WASM64 on Local HB", function () {
  let hbeam, hb

  before(async () => {
    hbeam = await new HyperBEAM({ reset: true }).ready()
    hb = new HB({ url: hbeam.url })
    await hb.init(hbeam.jwk)
  })

  after(async () => {
    if (hbeam) hbeam.kill()
  })

  it("cache + spawn custom WASM", async () => {
    const wasm = readFileSync(wasmPath)
    console.log(`  WASM size: ${wasm.length} bytes`)
    const imageId = await hb.cacheBinary(wasm, "application/wasm")
    console.log(`  Cached image: ${imageId}`)
    assert.ok(imageId, "should get image ID from cache")

    const { pid, slot } = await hb.spawnAOS({ image: imageId })
    console.log(`  pid: ${pid}, spawn slot: ${slot}`)
    assert.ok(pid, "should get process ID")
  })

  it("eval: send Eval action", async () => {
    const wasm = readFileSync(wasmPath)
    const imageId = await hb.cacheBinary(wasm, "application/wasm")
    const { pid } = await hb.spawnAOS({ image: imageId })
    console.log(`  pid: ${pid}`)

    const { slot } = await hb.scheduleAOS({
      pid,
      action: "Eval",
      data: "hello from rust",
      tags: {},
    })
    console.log(`  Eval scheduled at slot ${slot}`)

    const result = await hb.computeAOS({ pid, slot })
    console.log(`  Result:`, JSON.stringify(result).slice(0, 200))
    assert.ok(result, "should get compute result")
  })

  it("counter: Inc + Get", async () => {
    const wasm = readFileSync(wasmPath)
    const imageId = await hb.cacheBinary(wasm, "application/wasm")
    const { pid } = await hb.spawnAOS({ image: imageId })
    console.log(`  pid: ${pid}`)

    const inc1 = await hb.scheduleAOS({ pid, action: "Inc", tags: {} })
    console.log(`  Inc scheduled at slot ${inc1.slot}`)
    const incResult = await hb.computeAOS({ pid, slot: inc1.slot })
    console.log(`  Inc result:`, JSON.stringify(incResult).slice(0, 200))

    const get1 = await hb.scheduleAOS({ pid, action: "Get", tags: {} })
    console.log(`  Get scheduled at slot ${get1.slot}`)
    const getResult = await hb.computeAOS({ pid, slot: get1.slot })
    console.log(`  Get result:`, JSON.stringify(getResult).slice(0, 200))

    const resultStr = JSON.stringify(getResult)
    assert.ok(
      resultStr.includes('"1"') || resultStr.includes('"data":"1"'),
      `counter should be 1, got: ${resultStr.slice(0, 300)}`,
    )
  })

  it("counter: multiple Inc → correct count", async () => {
    const wasm = readFileSync(wasmPath)
    const imageId = await hb.cacheBinary(wasm, "application/wasm")
    const { pid } = await hb.spawnAOS({ image: imageId })
    console.log(`  pid: ${pid}`)

    for (let i = 1; i <= 3; i++) {
      const { slot } = await hb.scheduleAOS({ pid, action: "Inc", tags: {} })
      await hb.computeAOS({ pid, slot })
      console.log(`  Inc #${i} at slot ${slot}`)
    }

    const { slot } = await hb.scheduleAOS({ pid, action: "Get", tags: {} })
    const result = await hb.computeAOS({ pid, slot })
    console.log(`  Get result:`, JSON.stringify(result).slice(0, 200))

    const resultStr = JSON.stringify(result)
    assert.ok(
      resultStr.includes('"3"') || resultStr.includes('"data":"3"'),
      `counter should be 3, got: ${resultStr.slice(0, 300)}`,
    )
  })
})

describe("Mode 10R: Custom WASM64 on Remote HB", function () {
  const REMOTE = REMOTE_NODES[1] // push-1 (supports compute for standard processes)
  let hb, arweaveImageId

  before(async () => {
    hb = new HB({ url: REMOTE, jwk })
    await hb.init(jwk)

    // Upload WASM to Arweave via Turbo (free for < 100KB)
    const wasm = readFileSync(wasmPath)
    console.log(`  Uploading ${wasm.length} bytes to Arweave via Turbo...`)
    arweaveImageId = await uploadToArweave(wasm)
    console.log(`  Arweave TX: ${arweaveImageId}`)
    console.log(`  Remote node: ${REMOTE}`)
  })

  it("spawn custom WASM from Arweave TX", async () => {
    const { pid, slot } = await hb.spawnAOS({
      image: arweaveImageId,
      sign: "ans104",
    })
    console.log(`  pid: ${pid}, spawn slot: ${slot}`)
    assert.ok(pid, "should get process ID")
  })

  it("schedule Inc + Get messages", async () => {
    // Note: remote compute fails with no_valid_device_stack (ANS-104 flat tags
    // get stripped after cache round-trip). This matches mode 7 behavior.
    // This test verifies spawn + schedule only.
    const { pid } = await hb.spawnAOS({
      image: arweaveImageId,
      sign: "ans104",
    })
    console.log(`  pid: ${pid}`)

    const inc1 = await hb.scheduleAOS({ pid, action: "Inc", tags: {} })
    console.log(`  Inc scheduled at slot ${inc1.slot}`)
    assert.ok(inc1.slot != null, "Inc slot assigned")

    const get1 = await hb.scheduleAOS({ pid, action: "Get", tags: {} })
    console.log(`  Get scheduled at slot ${get1.slot}`)
    assert.ok(get1.slot != null, "Get slot assigned")
  })

  it("multiple messages scheduled", async () => {
    const { pid } = await hb.spawnAOS({
      image: arweaveImageId,
      sign: "ans104",
    })
    console.log(`  pid: ${pid}`)

    const slots = []
    for (let i = 1; i <= 3; i++) {
      const { slot } = await hb.scheduleAOS({ pid, action: "Inc", tags: {} })
      slots.push(slot)
      console.log(`  Inc #${i} at slot ${slot}`)
    }
    assert.strictEqual(slots.length, 3, "3 slots assigned")

    const { slot } = await hb.scheduleAOS({ pid, action: "Get", tags: {} })
    console.log(`  Get at slot ${slot}`)
    assert.ok(slot != null, "Get slot assigned")
  })
})
