import assert from "assert"
import { describe, it, before, after, beforeEach } from "node:test"
import { HyperBEAM, toAddr } from "wao/test"
import { HB } from "wao"
import { resolve } from "path"
import { readFileSync } from "fs"

const cwd = "../../HyperBEAM"
const hb_dir = resolve(process.cwd(), cwd)
const wallet = resolve(hb_dir, ".wallet.json")

const jwk = JSON.parse(readFileSync(wallet, "utf8"))
const addr = toAddr(jwk.n)

describe("HyperBEAM", function () {
  let hbeam, hb, jwk

  before(async () => (hbeam = await new HyperBEAM({ cwd }).ready()))
  beforeEach(async () => (hb = await new HB({}).init(jwk)))
  after(async () => hbeam.kill())

  it("should run a HyperBEAM node", async () => {
    const { out } = await hb.getJSON({ path: "/~meta@1.0/build" })
    assert.equal(out.node, "HyperBEAM")
  })
})
