import assert from "assert"
import { after, describe, it, before, beforeEach } from "node:test"
import { pick } from "ramda"
import HyperBEAM from "../../src/hyperbeam.js"
import { HB } from "../../src/index.js"
import { wait } from "../../src/test.js"

describe("Hyperbeam Device", function () {
  let hb, hbeam
  before(async () => (hbeam = await new HyperBEAM({ reset: true }).ready()))
  beforeEach(async () => (hb = hbeam.hb))

  after(async () => hbeam.kill())

  // data is not persistent at 10000, but persistent at 10001
  it("should be persistent", async () => {
    //const hb2 = new HB({ jwk: hbeam.jwk, url: "http://localhost:10000" })
    const { pid } = await hb.spawn({})
    const { edges } = await hb.messages({ pid })
    assert.equal(edges.length, 1)
    hbeam.kill()
    hbeam = await new HyperBEAM({ reset: false }).ready()
    const { edges: edges2 } = await hbeam.hb.messages({ pid })
    assert.equal(edges2.length, 1)
  })
})

describe("Hyperbeam Device", function () {
  let hb, hbeam
  before(async () => (hbeam = await new HyperBEAM({ reset: true }).ready()))
  beforeEach(async () => (hb = hbeam.hb))

  after(async () => hbeam.kill())
  // data is not persistent at 10000, but persistent at 10001
  it("should fail", async () => {
    const hb2 = new HB({ jwk: hbeam.jwk, url: "http://localhost:10000" })
    const { pid } = await hb2.spawn({})
    const { edges } = await hb2.messages({ pid })
    assert.equal(edges.length, 1)
    hbeam.kill()
    hbeam = await new HyperBEAM({ reset: false }).ready()

    // this fails
    await assert.rejects(hb2.messages({ pid }))
  })
})
