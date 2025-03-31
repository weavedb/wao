import assert from "assert"
import { after, describe, it, before, beforeEach } from "node:test"
import { acc } from "../src/test.js"
import HB from "../src/hb.js"
const [{ signer, jwk }] = acc
import { connect, createSigner } from "aoconnect-wao"
import { randomBytes } from "node:crypto"

const data = `
local count = 0
Handlers.add("Hello", "Hello", function (msg)
  count = count + 1
  msg.reply({ Data = "Count: "..tostring(count) })
end)
`
const URL = "http://localhost:10000"

describe("Hyperbeam", function () {
  after(() => setTimeout(() => process.exit(), 100))
  it("should get metrics", async () => {
    const hb = new HB()
    const met = await hb.metrics()
    assert.ok(met.process_uptime_seconds)
  })
  it.only("should get info", async () => {
    const hb = new HB()
    const info = await hb.info()
    assert.equal(info.port, 10000)
  })

  it("should deploy a process", async () => {
    const signer = createSigner(jwk)
    const { request } = connect({
      MODE: "mainnet",
      URL,
      device: "process@1.0",
      signer,
    })
    const { request: req } = connect({
      MODE: "mainnet",
      URL,
      device: "",
      signer,
    })
    const res = await req({ path: "~meta@1.0/info/address" })
    const address = res.body
    const p = await request({
      path: "/schedule",
      method: "POST",
      type: "Process",
      scheduler: address,
      module: "JArYBF-D8q2OmZ4Mok00sD2Y_6SYEQ7Hjx-6VZ_jl3g",
      device: "process@1.0",
      "scheduler-device": "scheduler@1.0",
      "execution-device": "genesis-wasm@1.0",
      authority: address,
      "scheduler-location": address,
      "random-seed": randomBytes(16).toString("hex"),
      data: 'print("Hello World")',
      "Data-Protocol": "ao",
      Variant: "ao.N.1",
    })
    const process = await p.process.text()
    console.log(process)
    const m = await request({
      path: `${process}/schedule`,
      type: "Message",
      method: "POST",
      action: "Eval",
      data,
      "Data-Protocol": "ao",
      Variant: "ao.TN.1",
    })
    const slot = await m.slot.text()
    console.log("slot", slot)

    const r2 = await request({
      path: `/${process}/push&slot+integer=${slot}`,
      method: "POST",
      target: process,
      "slot+integer": slot,
    }).catch(e => console.log(e))
    console.log("mid", r2)
    let i = 0
    const m2 = await request({
      path: `${process}/schedule`,
      type: "Message",
      method: "POST",
      action: "Hello",
      data: "1984",
      "Data-Protocol": "ao",
      Variant: "ao.TN.1",
    })
    console.log(m2)
    const slot2 = await m2.slot.text()
    console.log("this is the slot", slot2)
    const r3 = await request({
      path: `/${process}/compute&slot+integer=${slot2}/results/json`,
      method: "POST",
      target: process,
      "slot+integer": slot2,
    })
    assert.equal(r3.Messages[0].Data, "Count: 1")
  })
})
