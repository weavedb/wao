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
    const { process: pid } = await hb.p(
      "/schedule",
      {
        device: "process@1.0",
        type: "Process",
        "data-protocol": "ao",
        variant: "ao.TN.1",
        scheduler: hb.addr,
        "scheduler-location": hb.addr,
        authority: hb.addr,
        "random-seed": seed(16),
        module: "ISShJH1ij-hPPt9St5UFFr_8Ys3Kj5cyg7zrMGt7H9s",
        "scheduler-device": "scheduler@1.0",
        "execution-device": "stack@1.0",
        "device-stack": ["genesis-wasm@1.0", "patch@1.0"],
        "push-device": "push@1.0",
        "patch-from": "/results/outbox",
      },
      { path: false }
    )

    await hb.p(
      `/${pid}/schedule`,
      { type: "Message", target: pid, action: "Eval", data },
      { path: false }
    )

    await hb.p(
      `/${pid}/schedule`,
      { type: "Message", target: pid, action: "Inc" },
      { path: false }
    )
    const { slot } = await hb.p(
      `/${pid}/schedule`,
      { type: "Message", target: pid, action: "Inc" },
      { path: false }
    )
    const { results } = await hb.g(`/${pid}/compute`, { slot })
    assert.equal("Count: 2", results.outbox["1"].data)

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
