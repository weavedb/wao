import assert from "assert"
import { describe, it, before, after, beforeEach } from "node:test"
import { HyperBEAM, wait, toAddr } from "wao/test"
import { HB } from "wao"
import { resolve } from "path"
import { readFileSync } from "fs"

const cwd = "../../HyperBEAM"
const wallet = ".wallet.json"

describe("HyperBEAM", function () {
  let hbeam, hb, jwk

  before(async () => {
    hbeam = await new HyperBEAM({ c: "12", cmake: "3.5", cwd, wallet }).ready()
    jwk = JSON.parse(readFileSync(resolve(process.cwd(), cwd, wallet), "utf8"))
  })

  beforeEach(async () => (hb = await new HB({}).init(jwk)))

  after(async () => hbeam.kill())

  it("should run a HyperBEAM node", async () => {
    const info = await hb.getJSON({ path: "/~meta@1.0/info" })
    assert.equal(info.address, toAddr(jwk.n))
  })
})
