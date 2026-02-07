import assert from "assert"
import base64url from "base64url"
import { after, describe, it, before, beforeEach } from "node:test"
import { acc, mu, AO, toAddr } from "../../src/test.js"
import HB from "../../src/hb.js"
import AOHB from "../../src/ao.js"
import { isNotNil, filter, isNil } from "ramda"
import { randomBytes } from "node:crypto"
import { wait } from "../../src/utils.js"
import Server from "../../src/server.js"
import HyperBEAM from "../../src/hyperbeam.js"
import { readFileSync } from "fs"
import { resolve } from "path"

import AO2 from "../../src/ao.js"

// Use a separate test account JWK (not HyperBEAM's JWK) to avoid multiple_matches
// when both user and scheduler sign with the same key
const testJwk = acc[0].jwk

const src_data = `
local count = 0
Handlers.add("Add", "Add", function (msg)
  count = count + tonumber(msg.Plus)
end)

Handlers.add("Get", "Get", function (msg)
  msg.reply({ Data = tostring(count) })
end)
`

const data = `
local count = 0
Handlers.add("Inc", "Inc", function (msg)
  count = count + 1
  msg.reply({ Data = "Count: "..tostring(count) })
end)

Handlers.add("Get", "Get", function (msg)
  msg.reply({ Data = "Count: "..tostring(count) })
end)`

const URL = "http://localhost:10001"

describe("Hyperbeam Legacynet (FAIL)", function () {
  let hb, hbeam
  before(async () => (hbeam = await new HyperBEAM({ reset: true, genesis_wasm: true }).ready()))
  // Use separate test JWK to avoid multiple_matches when scheduler signs with same key
  beforeEach(async () => (hb = await new HB({ url: hbeam.url }).init(testJwk)))
  after(async () => hbeam.kill())

  it("should run hyper Lua", async () => {
    const { pid } = await hb.spawnLua()
    await hb.scheduleLua({ pid, action: "Eval", data })
    await hb.scheduleLua({ pid, action: "Inc" })
    const { slot } = await hb.scheduleLua({ pid, action: "Get" })
    const { outbox } = await hb.computeLua({ pid, slot })
    assert.equal(outbox[0].data, "Count: 1")
    await hb.scheduleLua({ pid, action: "Inc" })
    const { slot: slot2 } = await hb.scheduleLua({ pid, action: "Get" })
    const { outbox: outbox2 } = await hb.computeLua({ pid, slot: slot2 })
    assert.equal(outbox2[0].data, "Count: 2")
  })

  it("should handle counter with Add and Get handlers", async () => {
    const { pid } = await hb.spawnAOS()
    await hb.messageAOS({ pid, action: "Eval", tags: {}, data: src_data })
    await hb.messageAOS({ pid, action: "Add", tags: { Plus: "3" } })
    assert.equal(
      (await hb.messageAOS({ pid, action: "Get" })).outbox["1"].data,
      "3"
    )
  })

  it("should execute AOS with WAMR", async () => {
    const { pid } = await hb.spawnAOS()
    await hb.messageAOS({ pid, action: "Eval", tags: {}, data: src_data })
    await hb.messageAOS({ pid, action: "Add", tags: { Plus: "3" } })
    assert.equal(
      (await hb.messageAOS({ pid, action: "Get" })).outbox["1"].data,
      "3"
    )
    await hb.messageAOS({ pid, action: "Add", tags: { Plus: "3" } })
    assert.equal(
      (await hb.messageAOS({ pid, action: "Get" })).outbox["1"].data,
      "6"
    )
  })

  it("should test WAMR", async () => {
    const { pid } = await hb.spawnAOS()
    await hb.scheduleAOS({ pid, action: "Eval", data: src_data })
    await hb.scheduleAOS({ pid, action: "Add", tags: { Plus: "3" } })
    await hb.scheduleAOS({ pid, action: "Get" })
    console.log("compute: 0", await hb.computeAOS({ pid, slot: 0 }))
    console.log("compute: 1", await hb.computeAOS({ pid, slot: 1 }))
    console.log("compute: 3", await hb.computeAOS({ pid, slot: 3 }))
    console.log("compute: 3", await hb.computeAOS({ pid, slot: 3 }))
    console.log("compute: 2", await hb.computeAOS({ pid, slot: 2 }))
  })
})
