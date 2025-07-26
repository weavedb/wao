import assert from "assert"
import { after, describe, it, before, beforeEach } from "node:test"
import { mod } from "./lib/test-utils.js"
import { HyperBEAM } from "wao/test"
import cases, { errors } from "./lib/cases.js"
import { normalize } from "../src/erl_json.js"
import { createSigner } from "../src/signer.js"
import { send } from "../src/send.js"
import { commit } from "../src/commit.js"

describe("Hyperbeam Signer", function () {
  let hbeam, sign
  before(async () => {
    hbeam = await new HyperBEAM({ reset: true }).ready()
    sign = createSigner(hbeam.jwk, hbeam.url)
  })
  after(async () => hbeam.kill())

  it("should test signer", async () => {
    const msg = await commit({ key: "value" }, { signer: sign })
    for (const k in msg.commitments) {
      if (msg.commitments[k].committer) {
        assert.equal(hbeam.addr, msg.commitments[k].committer)
      }
    }
  })
})
