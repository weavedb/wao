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

const src_data = `
local count = 2
Handlers.add("Inc2", "Inc2", function (msg)
  msg.reply({ Data = tostring(count) })
end)

Handlers.add("Inc", "Inc", function (msg)
  local data = Send({ Target = msg.To, Action = "Inc2" }).receive().Data
  count = count + tonumber(data)
  msg.reply({ Data = "Count: "..tostring(count) })
end)

Handlers.add("Get", "Get", function (msg)
  msg.reply({ Data = "Count: "..tostring(count) })
end)`

const src_data2 = `
local count = 10
Handlers.add("Inc2", "Inc2", function (msg)
  msg.reply({ Data = tostring(count) })
end)

Handlers.add("Inc", "Inc", function (msg)
  local data = Send({ Target = msg.To, Action = "Inc2" }).receive().Data
  count = count + tonumber(data)
  msg.reply({ Data = "Count: "..tostring(count) })
end)

Handlers.add("Get", "Get", function (msg)
  msg.reply({ Data = "Count: "..tostring(count) })
end)`

const URL = "http://localhost:10001"

describe("Hyperbeam Legacynet", function () {
  let hb, hbeam
  before(async () => (hbeam = await new HyperBEAM({ reset: true }).ready()))
  beforeEach(async () => (hb = hbeam.hb))
  after(async () => hbeam.kill())

  it.only("should deploy a process", async () => {
    const ao = await new AO2({ hb: "ans104" }).init(hbeam.jwk)
    const { p, pid } = await ao.deploy({ src_data })
    console.log((await p.msg("Inc", { To: pid })).res.Messages[0])
    console.log(await p.msg("Get"))
  })

  it("should deploy a process", async () => {
    const { out: address } = await hb.get({ path: "/~meta@1.0/info/address" })
    const ao = await new AO2({ hb: "httpsig" }).init(hbeam.jwk)
    const ao2 = await new AO2({ hb: "httpsig" }).init(hbeam.jwk)
    const { p, pid } = await ao.deploy({ src_data })
    const { p: p2, pid: pid2 } = await ao2.deploy({ src_data: src_data2 })
    console.log("pid", pid)
    console.log((await p.msg("Inc", { To: pid2 })).res.Messages[0])
    console.log(await p.msg("Get"))
    console.log(pid, pid2)
  })
})
