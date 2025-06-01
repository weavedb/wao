import assert from "assert"
import { after, describe, it, before, beforeEach } from "node:test"
import { wait } from "../src/utils.js"
import { getJWK } from "./test-utils.js"
import HB from "../src/hb.js"
import HyperBEAM from "../src/hyperbeam.js"

const src_data = `
local count = 0
Handlers.add("Add", "Add", function (msg)
  count = count + tonumber(msg.Plus)
end)

Handlers.add("Get", "Get", function (msg)
  msg.reply({ Data = tostring(count) })
end)
`

describe("HyperBEAM Template", function () {
  let hb, hbeam
  before(async () => {
    hbeam = new HyperBEAM({ c: "12", cmake: "3.5", gateway: 4000 })
    await wait(5000)
    const jwk = getJWK("../HyperBEAM/hyperbeam-key.json")
    hb = new HB({ jwk })
  })
  after(async () => {
    hbeam.kill()
  })
  it.only("should handle counter with Add and Get handlers", async () => {
    const pid = await hb.spawnAOS()
    await hb.messageAOS("Eval", {}, src_data)
    await hb.messageAOS("Add", { Plus: "3" })
    assert.equal((await hb.messageAOS("Get")).outbox["1"].data, "3")
  })
})
