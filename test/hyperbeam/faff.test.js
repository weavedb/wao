import assert from "assert"
import { after, describe, it, before, beforeEach } from "node:test"
import { acc } from "../../src/test.js"
import HB from "../../src/hb.js"
import HyperBEAM from "../../src/hyperbeam.js"
let allowed_user = acc[0]
let disallowed_user = acc[1]
let operator = null

describe("Hyperbeam Legacynet", function () {
  let hbeam
  before(async () => {
    hbeam = await new HyperBEAM({
      faff: [HyperBEAM.OPERATOR, allowed_user.addr],
      reset: true,
    }).ready()
  })
  beforeEach(async () => {
    operator = hbeam
    allowed_user.hb = new HB({ jwk: allowed_user.jwk })
    disallowed_user.hb = new HB({ jwk: disallowed_user.jwk })
  })
  after(async () => hbeam.kill())

  it("should test faff@1.0", async () => {
    const msg = ["/~message@1.0/set/hello", { hello: "world" }]

    // GET
    assert(await operator.hb.g(...msg))
    assert(await allowed_user.hb.g(...msg))
    assert(await disallowed_user.hb.g(...msg))

    // POST
    assert(await operator.hb.p(...msg))
    assert(await allowed_user.hb.p(...msg))
    await assert.rejects(disallowed_user.hb.p(...msg))

    const info = await operator.hb.g("/~meta@1.0/info")
    assert.deepEqual(info.faff_allow_list, [operator.addr, allowed_user.addr])

    // remove allowed_user
    await operator.hb.p("/~meta@1.0/info", { faff_allow_list: [operator.addr] })
    const info2 = await operator.hb.g("/~meta@1.0/info")
    assert.deepEqual(info2.faff_allow_list, [operator.addr])

    // now previously allowed_user fails too
    await assert.rejects(allowed_user.hb.p(...msg))
  })
})
