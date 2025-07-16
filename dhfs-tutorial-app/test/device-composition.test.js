import assert from "assert"
import { describe, it, before, after, beforeEach } from "node:test"
import { HyperBEAM } from "wao/test"

const cwd = "../HyperBEAM"
const devices = [
  "json",
  "structured",
  "httpsig",
  "flat",
  "meta",
  "stack",
  { name: "mydev@1.0", module: "dev_mydev" },
]

describe("Device Composition", function () {
  let hbeam, hb
  before(async () => {
    hbeam = await new HyperBEAM({ cwd, devices, reset: true }).ready()
  })
  beforeEach(async () => (hb = hbeam.hb))
  after(async () => hbeam.kill())

  it("should chain methods from an existing message", async () => {
    const out = await hb.p("/~mydev@1.0/resolve3")
    const { num } = await hb.g(
      `/${out.hashpath_7}/~mydev@1.0/inc/~mydev@1.0/double/~mydev@1.0/square`
    )
    assert.equal(num, 196)
  })

  it("should chain methods from an initial value", async () => {
    const { num } = await hb.p(
      "/~mydev@1.0/calc/~mydev@1.0/inc/~mydev@1.0/double/~mydev@1.0/square",
      { init_num: 1 }
    )
    assert.equal(num, 16)
  })

  it("should stack devices", async () => {
    const msg_base = {
      device: "stack@1.0",
      "device-stack": { 1: "mydev@1.0", 2: "mydev@1.0", 3: "mydev@1.0" },
      mode: "Fold",
      num: 3,
    }

    const out = await hb.p("inc2", msg_base)
    assert.equal(out.num, 6) // 3 + 1 + 1 + 1

    const out2 = await hb.p("double2", msg_base)
    assert.equal(out2.num, 24) // 3 * 2 * 2 * 2

    const out3 = await hb.p("square2", msg_base)
    assert.equal(out3.num, 6561) // 3 * 3 * 9 * 81
  })
})
