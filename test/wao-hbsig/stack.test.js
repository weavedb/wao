import assert from "assert"
import { after, describe, it, before, beforeEach } from "node:test"
import HyperBEAM from "../../src/hyperbeam.js"

describe("Stack Device", function () {
  let hb, hbeam
  before(async () => (hbeam = await new HyperBEAM({ reset: true }).ready()))
  beforeEach(async () => (hb = hbeam.hb))
  after(async () => hbeam.kill())

  it("should compute with device-stack array", async () => {
    const { pid } = await hb.spawn({
      "execution-device": "stack@1.0",
      "device-stack": ["inc@1.0", "double@1.0"],
    })
    // [0] 1 * 2 = 2, [1] (2 + 1) * 2 = 6
    await hb.schedule({ pid })
    assert.equal((await hb.compute({ pid, slot: 1 })).num, 6)
  })

  it("should compute with triple device-stack array", async () => {
    const { pid } = await hb.spawn({
      "execution-device": "stack@1.0",
      "device-stack": ["inc@1.0", "double@1.0", "double@1.0"],
    })
    // [0] 1 * 2 * 2 = 4, [1] (4 + 1) * 2 * 2 = 20, [2] (20 + 1) * 2 * 2 = 84
    await hb.schedule({ pid })
    await hb.schedule({ pid })
    assert.equal((await hb.compute({ pid, slot: 0 })).num, 4)
    assert.equal((await hb.compute({ pid, slot: 1 })).num, 20)
    assert.equal((await hb.compute({ pid, slot: 2 })).num, 84)
  })

  it("should compute with device-stack object", async () => {
    const { pid } = await hb.spawn({
      "execution-device": "stack@1.0",
      "device-stack": { 1: "inc@1.0", 2: "double@1.0" },
    })
    await hb.schedule({ pid })
    const result = await hb.compute({ pid, slot: 0 })
    assert.equal(result.num, 2)
  })
})
