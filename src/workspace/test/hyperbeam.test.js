import assert from "assert"
import { describe, it, before, after } from "node:test"
import { readFileSync } from "fs"
import { resolve } from "path"

import { HyperBEAM } from "wao/test"
import { AO } from "wao"

const src_data = readFileSync(
  resolve(import.meta.dirname, "../src/counter.lua"),
  "utf8"
)

describe("HyperBEAM AOS", function () {
  let hbeam, ao

  before(async () => {
    // genesis_wasm: true — runs AOS processes on the HyperBEAM stack
    hbeam = await new HyperBEAM({ reset: true, genesis_wasm: true }).ready()
    ao = await new AO({ hb: hbeam.url }).init(hbeam.jwk)
  })

  after(async () => hbeam.kill())

  it("should deploy and send messages via HyperBEAM", async () => {
    const { p } = await ao.deploy({ src_data })

    // message through HTTP → HyperBEAM → genesis-wasm
    const { out } = await p.msg("Hello")
    assert.equal(out, "Hello, World!")

    await p.msg("Inc")
    await p.msg("Inc")
    const { out: count } = await p.msg("Get")
    assert.equal(count, "2")
  })

  it("should spawn multiple processes on same HyperBEAM", async () => {
    const { p: p1 } = await ao.deploy({ src_data })
    const { p: p2 } = await ao.deploy({ src_data })

    // Increment p1 twice, p2 once
    await p1.msg("Inc")
    await p1.msg("Inc")
    await p2.msg("Inc")

    const { out: count1 } = await p1.msg("Get")
    const { out: count2 } = await p2.msg("Get")

    assert.equal(count1, "2")
    assert.equal(count2, "1")
  })
})

describe("HyperBEAM Raw", function () {
  let hbeam, hb

  before(async () => {
    hbeam = await new HyperBEAM({ reset: true }).ready()
    hb = hbeam.hb
  })

  after(async () => hbeam.kill())

  it("should read and write node config", async () => {
    await hb.post({ path: "/~meta@1.0/info", test_key: "test_value" })
    const { out } = await hb.get({ path: "/~meta@1.0/info" })
    assert.equal(out.test_key, "test_value")
  })

  it("should spawn and compute with device stack", async () => {
    const { pid } = await hb.spawn({
      "execution-device": "stack@1.0",
      "device-stack": ["wao@1.0", "patch@1.0"],
      "patch-from": "/results",
      "patch-to": "/cache",
    })
    assert.ok(pid, "spawn should return a process ID")
  })
})
