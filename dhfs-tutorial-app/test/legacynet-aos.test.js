import assert from "assert"
import { describe, it, before, after } from "node:test"
import { HyperBEAM } from "wao/test"
import { AO } from "wao"

const seed = num => {
  const array = new Array(num)
  for (let i = 0; i < num; i++) array[i] = Math.floor(Math.random() * 256)
  return Buffer.from(array).toString("base64")
}

const data = `
local count = 0
Handlers.add("Inc", "Inc", function (msg)
  count = count + 1
  msg.reply({ Data = "Count: "..tostring(count) })
end)

Handlers.add("Get", "Get", function (msg)
  msg.reply({ Data = "Count: "..tostring(count) })
end)`

describe("Legacynet AOS", function () {
  let hbeam, hb
  before(async () => {
    hbeam = await new HyperBEAM({ reset: true, as: ["genesis_wasm"] }).ready()
    hb = hbeam.hb
  })
  after(async () => hbeam.kill())

  it("should spawn a legacynet AOS process", async () => {
    // Use spawnLegacy/scheduleLegacy for spawn (authority header conflicts with httpsig)
    const { pid } = await hb.spawnLegacy()
    const { slot: slot0 } = await hb.scheduleLegacy({ pid, data })
    await hb.computeLegacy({ pid, slot: slot0 })
    const { slot: slot1 } = await hb.scheduleLegacy({ pid, action: "Inc" })
    await hb.computeLegacy({ pid, slot: slot1 })
    const { slot: slot2 } = await hb.scheduleLegacy({ pid, action: "Inc" })
    const r = await hb.computeLegacy({ pid, slot: slot2 })
    assert.equal(r.Messages[0].Data, "Count: 2")

    const { body } = await hb.post({
      path: "/~relay@1.0/call",
      method: "POST",
      "relay-path": `http://localhost:6363/dry-run?process-id=${pid}`,
      "content-type": "application/json",
      "relay-body": JSON.stringify({
        Tags: [{ name: "Action", value: "Get" }],
        Owner: hb.addr,
      }),
    })
    assert.equal(JSON.parse(body).Messages[0].Data, "Count: 2")
  })

  it("should spawn a legacynet AOS process #2", async () => {
    const { pid } = await hb.spawnLegacy()
    const { slot } = await hb.scheduleLegacy({ pid, data })
    const r = await hb.computeLegacy({ pid, slot })
    const { slot: slot2 } = await hb.scheduleLegacy({ pid, action: "Inc" })
    const r2 = await hb.computeLegacy({ pid, slot: slot2 })
    assert.equal(r2.Messages[0].Data, "Count: 1")
    const { slot: slot3 } = await hb.scheduleLegacy({ pid, action: "Inc" })
    const r4 = await hb.computeLegacy({ pid, slot: slot3 })
    const r3 = await hb.dryrun({ pid, action: "Get" })
    assert.equal(r3.Messages[0].Data, "Count: 2")
  })

  it("should spawn a legacynet AOS process #3", async () => {
    const ao = await new AO({ module_type: "mainnet", hb: hbeam.url }).init(
      hbeam.jwk
    )
    const { p } = await ao.deploy({ src_data: data })
    await p.m("Inc")
    assert.equal(await p.d("Get"), "Count: 1")
    await p.m("Inc")
    assert.equal(await p.d("Get"), "Count: 2")
  })
})
