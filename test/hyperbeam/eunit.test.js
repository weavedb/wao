import assert from "assert"
import { after, describe, it, before, beforeEach } from "node:test"
import HyperBEAM from "../../src/hyperbeam.js"

describe("Hyperbeam Signer", function () {
  let hb, hbeam
  before(async () => (hbeam = new HyperBEAM({ reset: true, shell: false })))
  beforeEach(async () => (hb = hbeam.hb))
  it.only("should run eunit", async () => await hbeam.eunit("dev_message"))
})
