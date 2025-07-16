import assert from "assert"
import { after, describe, it, before, beforeEach } from "node:test"
import { acc, toAddr } from "../src/test.js"
import HB from "../src/hb.js"
import HyperBEAM from "../src/hyperbeam.js"
import { readFileSync } from "fs"
import { resolve } from "path"

const cwd = "./HyperBEAM"
const wallet = resolve(process.cwd(), cwd, ".wallet.json")
const jwk = JSON.parse(readFileSync(wallet, "utf8"))

let operator = { jwk, addr: toAddr(jwk.n) }
let allowed_user = acc[0]
let disallowed_user = acc[1]

describe("Hyperbeam Legacynet", function () {
  let hb, hb2, hb3, hbeam

  before(async () => {
    hbeam = await new HyperBEAM({
      faff: [operator.addr, allowed_user.addr],
    }).ready()
  })

  beforeEach(async () => {
    operator.hb = await new HB({}).init(operator.jwk)
    allowed_user.hb = await new HB({}).init(allowed_user.jwk)
    disallowed_user.hb = await new HB({}).init(disallowed_user.jwk)
  })
  after(async () => hbeam.kill())

  it("should test faff@1.0", async () => {
    const msg = { path: "/~message@1.0/set/hello", hello: "world" }
    const info = { path: "/~meta@1.0/info" }

    // GET
    assert(await operator.hb.get(msg))
    assert(await allowed_user.hb.get(msg))
    assert(await disallowed_user.hb.get(msg))

    // POST
    assert(await operator.hb.post(msg))
    assert(await allowed_user.hb.post(msg))
    await assert.rejects(disallowed_user.hb.post(msg))

    const { out: info1 } = await operator.hb.get(info)
    assert.deepEqual(info1.faff_allow_list, [operator.addr, allowed_user.addr])

    // remove allowed_user
    await operator.hb.post({ ...info, faff_allow_list: [operator.addr] })
    const { out: info2 } = await operator.hb.get({
      path: "/~meta@1.0/info",
    })
    assert.deepEqual(info2.faff_allow_list, [operator.addr])

    // now previously allowed_user fails too
    await assert.rejects(allowed_user.hb.post(msg))
  })
})
