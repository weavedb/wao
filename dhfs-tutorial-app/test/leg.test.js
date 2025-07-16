import assert from "assert"
import { describe, it, before, after, beforeEach } from "node:test"
import { HyperBEAM } from "wao/test"

const cwd = "../HyperBEAM"

const seed = num => {
  const array = new Array(num)
  for (let i = 0; i < num; i++) array[i] = Math.floor(Math.random() * 256)
  return Buffer.from(array).toString("base64")
}

const src_data = `
local count = 0
Handlers.add("Add", "Add", function (msg)
  count = count + tonumber(msg.Plus)
end)

Handlers.add("Get", "Get", function (msg)
  msg.reply({ Data = tostring(count) })
end)
`

const data = `
local count = 0
Handlers.add("Inc", "Inc", function (msg)
  count = count + 1
  msg.reply({ Data = "Count: "..tostring(count) })
end)

Handlers.add("Get", "Get", function (msg)
  msg.reply({ Data = "Count: "..tostring(count) })
end)`

describe("Hyperbeam Legacynet", function () {
  let hbeam, hb
  before(async () => {
    hbeam = await new HyperBEAM({
      cwd,
      reset: true,
      as: ["genesis_wasm"],
    }).ready()
  })
  beforeEach(async () => (hb = hbeam.hb))
  after(async () => hbeam.kill())

  it("should interact with a hyperbeam node", async () => {
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
})
