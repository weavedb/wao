import assert from "assert"
import { after, describe, it, before, beforeEach } from "node:test"
import HyperBEAM from "../../src/hyperbeam.js"

const src_data = `
local count = 0
Handlers.add("Add", "Add", function (msg)
  count = count + tonumber(msg.Plus)
end)

Handlers.add("Get", "Get", function (msg)
  msg.reply({ Data = tostring(count) })
end)
`

describe("HyperBEAM", function () {
  let hb, hbeam
  before(async () => (hbeam = await new HyperBEAM({ reset: true }).ready()))

  beforeEach(async () => (hb = hbeam.hb))
  after(async () => hbeam.kill())

  it("should handle counter with Add and Get handlers", async () => {
    const { pid } = await hb.spawnAOS()
    await hb.messageAOS({ pid, action: "Eval", tags: {}, data: src_data })
    await hb.messageAOS({ pid, action: "Add", tags: { Plus: "3" } })
    assert.equal(
      (await hb.messageAOS({ pid, action: "Get" })).outbox["1"].data,
      "3"
    )
  })

  it("should execute AOS with WAMR", async () => {
    const pid = await hb.spawnAOS()
    await hb.messageAOS({ action: "Eval", tags: {}, data: src_data })
    await hb.messageAOS({ action: "Add", tags: { Plus: "3" } })
    assert.equal((await hb.messageAOS({ action: "Get" })).outbox["1"].data, "3")
    await hb.messageAOS({ action: "Add", tags: { Plus: "3" } })
    assert.equal((await hb.messageAOS({ action: "Get" })).outbox["1"].data, "6")
  })
})
