import { describe, it, before, after } from "node:test"
import { HyperBEAM } from "../../src/test.js"
import AO from "../../src/ao.js"
import {
  runCounterTest,
  runTokenMintTest,
  runTokenTransferTest,
  runCrossProcessTest,
} from "./shared.js"

describe("Mode 8: Local HB — HyperAOS Lua", function () {
  let hbeam, ao

  before(async () => {
    hbeam = await new HyperBEAM({ reset: true }).ready()
    ao = await new AO({ hb: hbeam.url, mode: "lua" }).init(hbeam.jwk)
  })

  after(async () => {
    if (hbeam) hbeam.kill()
  })

  it("counter: deploy, Inc, Get → 1", async () => {
    await runCounterTest(ao, "Mode 8")
  })

  it("token: deploy, Mint 100, Balance → 100", async () => {
    await runTokenMintTest(ao, "Mode 8")
  })

  it("token transfer: Mint 100, Transfer 30, Balance → 70", async () => {
    await runTokenTransferTest(ao, "Mode 8")
  })

  it("cross-process: token Transfer → receiver Credit-Notice", async () => {
    await runCrossProcessTest(ao, ao, "Mode 8", true)
  })

  it("note: receive() requires multipass (not in lua@5.3a stack)", () => {
    console.log("  lua@5.3a lacks multipass@1.0 in device stack")
    console.log("  Send().receive() returns nil — coroutine cannot be resumed")
    console.log("  push@1.0 delivers messages but cannot resolve .receive()")
    console.log("  Use ao.send() + msg.reply() pattern instead")
  })
})
