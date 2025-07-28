import assert from "assert"
import { after, describe, it, before, beforeEach } from "node:test"
import { mod } from "./lib/test-utils.js"
import { HyperBEAM } from "wao/test"
import cases, { errors } from "./lib/cases.js"
import { normalize } from "../src/erl_json.js"
import { createSigner } from "../src/signer.js"
import { send } from "../src/send.js"
import { commit } from "../src/commit.js"

describe("Hyperbeam commit", function () {
  let hbeam, sign, hb
  before(async () => {
    hbeam = await new HyperBEAM({ reset: true }).ready()
    sign = createSigner(hbeam.jwk, hbeam.url)
    hb = hbeam.hb
  })
  after(async () => hbeam.kill())

  it("should test commit", async () => {
    const msg = await commit(
      { key: "value", data: Buffer.from([1, 2, 3]) },
      { signer: sign }
    )
    for (const k in msg.commitments) {
      if (msg.commitments[k].committer) {
        assert.equal(hbeam.addr, msg.commitments[k].committer)
      }
    }
  })
  it("should schedule a nested message", async () => {
    const { pid } = await hb.spawn()
    const { slot } = await hb.schedule({
      pid,
      tags: { str: "value", num: 123 },
      data: "abc",
    })
    assert.equal(1, slot)
  })
})
