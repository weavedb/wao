import assert from "assert"
import { after, describe, it, before, beforeEach } from "node:test"
import { acc, mu, AO, toAddr } from "../src/test.js"
import { getJWK } from "./lib/test-utils.js"
import HB from "../src/hb.js"
import { isNotNil, filter, isNil, range } from "ramda"
import { randomBytes } from "node:crypto"
import { wait } from "../src/utils.js"
import HyperBEAM from "../src/hyperbeam.js"
import { readFileSync } from "fs"
import { resolve } from "path"

describe("Hyperbeam Signer", function () {
  let hb, hb2, hbeam, jwk, addr
  before(async () => {
    jwk = getJWK("../../HyperBEAM/.wallet.json")
    addr = toAddr(jwk.n)
    hbeam = new HyperBEAM({ shell: false })
  })

  beforeEach(async () => {
    hb = await new HB({}).init(jwk)
  })

  after(async () => {})

  it.only("should run eunit", async () => {
    //await hbeam.eunit("find_failed")
    await hbeam.eunit("wao_test4")
  })
})
