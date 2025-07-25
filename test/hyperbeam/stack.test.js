import assert from "assert"
import { after, describe, it, before, beforeEach } from "node:test"
import HyperBEAM from "../../src/hyperbeam.js"

describe("Hyperbeam Device", function () {
  let hb, hbeam
  before(async () => (hbeam = await new HyperBEAM({ reset: true }).ready()))
  beforeEach(async () => (hb = hbeam.hb))
  after(async () => hbeam.kill())
  it("should test stack@1.0", async () => {
    const { pid } = await hb.spawn({
      "execution-device": "stack@1.0",
      "device-stack": ["inc@1.0", "double@1.0"],
    })
    // [0] 1 * 2 = 2, [1] (2 + 1) * 2 = 6, 2 (3 + 1) * 2 = 14
    await hb.schedule({ pid })
    assert.equal((await hb.compute({ pid, slot: 1 })).num, 6)
    return
    await hb.schedule({ pid })
    assert.equal((await hb.compute({ pid, slot: 0 })).num, 2)
    assert.equal((await hb.compute({ pid, slot: 1 })).num, 6)
    assert.equal((await hb.compute({ pid, slot: 2 })).num, 14)
  })

  it("should test stack@1.0 #2", async () => {
    const { pid } = await hb.spawn({
      "execution-device": "stack@1.0",
      "device-stack": ["inc@1.0", "double@1.0", "double@1.0"],
    })
    // [0] 1 * 2 * 2 = 4, [1] (4 + 1) * 2 * 2 = 20, 2 (20 + 1) * 2 * 2= 84
    await hb.schedule({ pid })
    await hb.schedule({ pid })
    assert.equal((await hb.compute({ pid, slot: 0 })).num, 4)
    assert.equal((await hb.compute({ pid, slot: 1 })).num, 20)
    assert.equal((await hb.compute({ pid, slot: 2 })).num, 84)
  })
})
