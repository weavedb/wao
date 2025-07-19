import assert from "assert"
import { describe, it, before, after, beforeEach } from "node:test"
import { HyperBEAM } from "wao/test"

const cwd = "../HyperBEAM"

const seed = num => {
  const array = new Array(num)
  for (let i = 0; i < num; i++) array[i] = Math.floor(Math.random() * 256)
  return Buffer.from(array).toString("base64")
}

describe("Processes and Scheduler", function () {
  let hbeam, hb
  before(async () => {
    hbeam = await new HyperBEAM({ cwd, reset: true }).ready()
  })
  beforeEach(async () => (hb = hbeam.hb))
  after(async () => hbeam.kill())

  it("should spawn a process", async () => {
    const { process: pid } = await hb.p("/~process@1.0/schedule", {
      scheduler: hb.addr,
      body: {
        device: "process@1.0",
        type: "Process",
        scheduler: hb.addr,
        "random-seed": seed(16),
        "execution-device": "inc@1.0",
      },
    })
    console.log(`Process ID: ${pid}`)
    const { slot } = await hb.p(`/${pid}/schedule`, {
      body: { type: "Message" },
    })
    console.log(`Allocated Slot: ${slot}`)

    const out = await hb.g(`/${pid}/compute`, { slot })
    assert.equal(out.num, 2)

    const { slot: slot2 } = await hb.p(`/${pid}/schedule`, {
      body: { type: "Message" },
    })
    console.log(`Allocated Slot: ${slot2}`)

    const out2 = await hb.g(`/${pid}/compute`, { slot: slot2 })
    assert.equal(out2.num, 3)

    const { slot: slot3 } = await hb.p(`/${pid}/schedule`, {
      body: { type: "Message" },
    })
    console.log(`Allocated Slot: ${slot3}`)

    const out3 = await hb.g(`/${pid}/now`)
    assert.equal(out3.num, 4)
  })

  it("should spawn a process with better API", async () => {
    const { pid } = await hb.spawn({ "execution-device": "inc@1.0" })
    const { slot } = await hb.schedule({ pid })
    const { num } = await hb.compute({ pid, slot })
    assert.equal(num, 2)

    const {
      res: { num: num2 },
    } = await hb.message({ pid }) // schedule + compute
    assert.equal(num2, 3)

    const { num: num3 } = await hb.now({ pid })
    assert.equal(num3, 3)
  })

  it("should spawn a process with stack@1.0", async () => {
    const { pid } = await hb.spawn({
      "execution-device": "stack@1.0",
      "device-stack": ["inc@1.0", "double@1.0", "square@1.0"],
    })

    const { num } = await hb.now({ pid })
    assert.equal(num, 4) // ((0 + 1) * 2) * ((0 + 1) * 2)

    const {
      res: { num: num2 },
    } = await hb.message({ pid })
    assert.equal(num2, 100) // ((4 + 1) * 2) * ((4 + 1) * 2)
  })

  it("should patch", async () => {
    const { pid } = await hb.spawn({
      "execution-device": "stack@1.0",
      "device-stack": ["inc2@1.0", "patch@1.0"],
      "patch-from": "/results",
      "patch-to": "/cache",
    })
    await hb.schedule({ pid })
    await hb.schedule({ pid })
    const square = (await hb.now({ pid, path: "/cache/square" })).body
    const double = (await hb.now({ pid, path: "/cache/double" })).body
    assert.equal(square, 9)
    assert.equal(double, 6)
  })
})
