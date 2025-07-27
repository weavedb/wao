import assert from "assert"
import { describe, it, before, after } from "node:test"
import { HyperBEAM } from "wao/test"
import { id } from "hbsig"

const mydev = { name: "mydev@1.0", module: "dev_mydev" }
const devices = ["json", "structured", "httpsig", "flat", "meta", mydev]

describe("Hashpaths", function () {
  let hbeam, hb
  before(async () => {
    hbeam = await new HyperBEAM({ devices, reset: true }).ready()
    hb = hbeam.hb
  })
  after(async () => hbeam.kill())

  it("should resolve", async () => {
    await hb.p("/~mydev@1.0/resolve")
  })

  it("should resolve #2", async () => {
    await hb.p("/~mydev@1.0/resolve2")
  })

  it("should resolve #3", async () => {
    const out = await hb.p("/~mydev@1.0/resolve3")
    const msg3 = await hb.g(`/${out.hashpath_3}`)
    const msg5 = await hb.g(`/${out.hashpath_5}`)
    const msg7 = await hb.g(`/${out.hashpath_7}`)

    assert.deepEqual({ device: "mydev@1.0", num: 1 }, msg3)
    assert.deepEqual({ device: "mydev@1.0", num: 3 }, msg5)
    assert.deepEqual({ device: "mydev@1.0", num: 6 }, msg7)
  })

  it("should extract hashpath", async () => {
    const { out, hashpath } = await hb.post({ path: "/~mydev@1.0/forward" })
    const { msg1, msg2 } = JSON.parse(out)
    assert.equal(`${id(msg1)}/${id(msg2)}`, hashpath)
  })
})
