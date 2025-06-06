import assert from "assert"
import { describe, it, before, after, beforeEach } from "node:test"
import { wait, toAddr } from "wao/test"
import { HyperBEAM, HB } from "wao"
import { resolve } from "path"
import { readFileSync } from "fs"

const cwd = "../../HyperBEAM"
const wallet = ".wallet.json"

describe("HyperBEAM", function () {
  let hbeam, hb, jwk

  before(async () => {
    hbeam = new HyperBEAM({ cwd, wallet })
    jwk = JSON.parse(
      readFileSync(resolve(import.meta.dirname, cwd, wallet), "utf8")
    )
    await wait(5000)
  })

  beforeEach(async () => (hb = await new HB({}).init(jwk)))

  after(async () => hbeam.kill())

  it("should run a HyperBEAM node", async () => {
    const info = await hb.meta.info()
    assert.equal(info.address, toAddr(jwk.n))
  })
})
