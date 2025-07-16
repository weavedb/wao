import assert from "assert"
import { after, describe, it, before, beforeEach } from "node:test"
import HyperBEAM from "../../src/hyperbeam.js"

const data = `
local count = 0
Handlers.add("Inc", "Inc", function (msg)
  count = count + 1
  msg.reply({ Data = "Count: "..tostring(count) })
end)

Handlers.add("Get", "Get", function (msg)
  msg.reply({ Data = "Count: "..tostring(count) })
end)`

const src_data = `
local count = 0
Handlers.add("Add", "Add", function (msg)
  count = count + tonumber(msg.Plus)
end)

Handlers.add("Get", "Get", function (msg)
  msg.reply({ Data = tostring(count) })
end)
`

describe("Hyperbeam Device", function () {
  let hb, hbeam
  before(async () => (hbeam = await new HyperBEAM({ reset: true }).ready()))
  beforeEach(async () => (hb = hbeam.hb))
  after(async () => hbeam.kill())

  it("should upload module", async () => {
    const { process: pid } = await hb.p("/~process@1.0/schedule", {
      device: "process@1.0",
      scheduler: hb.addr,
      "execution-device": "wao@1.0",
    })

    // todo: message invalid when signed body is empty
    await hb.p(`/${pid}/schedule`, { target: pid })
    await hb.p(`/${pid}/schedule`, { target: pid })
    await hb.p(`/${pid}/schedule`, { target: pid })
    const { count } = await hb.g(`/${pid}/compute`, { slot: 3 })
    assert.equal(count, 4)
    await hb.p(`/${pid}/schedule`, { target: pid })
    const now = await hb.getJSON({ path: `/${pid}/now` })
    assert.equal(now.count, 5)
  })

  it("should upload module", async () => {
    const { pid } = await hb.spawn({ "execution-device": "wao@1.0" })
    await hb.schedule({ pid })
    await hb.schedule({ pid })
    await hb.schedule({ pid })
    const { count } = await hb.compute({ pid, slot: 2 })
    assert.equal(count, 3)
    await hb.schedule({ pid })
    assert.equal((await hb.now({ pid })).count, 5)
  })
})
