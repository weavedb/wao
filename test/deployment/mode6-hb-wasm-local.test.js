import { describe, it, before, after } from "node:test"
import { HyperBEAM } from "../../src/test.js"
import AO from "../../src/ao.js"
import {
  runCounterTest,
  runSelfReceiveTest,
  runTokenMintTest,
  runTokenTransferTest,
  runCrossProcessTest,
  runCrossProcessReceiveHBTest,
} from "./shared.js"

describe("Mode 6: Local HB — wasm-64 (AOS)", function () {
  let hbeam, ao

  before(async () => {
    hbeam = await new HyperBEAM({ reset: true }).ready()
    ao = await new AO({ hb: hbeam.url, mode: "aos" }).init(hbeam.jwk)
  })

  after(async () => {
    if (hbeam) hbeam.kill()
  })

  it("counter: deploy, Inc, Get → 1", async () => {
    await runCounterTest(ao, "Mode 6")
  })

  it("token: deploy, Mint 100, Balance → 100", async () => {
    await runTokenMintTest(ao, "Mode 6")
  })

  it("token transfer: Mint 100, Transfer 30, Balance → 70", async () => {
    await runTokenTransferTest(ao, "Mode 6")
  })

  it("cross-process: token Transfer → receiver Credit-Notice", async () => {
    await runCrossProcessTest(ao, ao, "Mode 6", true)
  })

  it("self-receive: Send().receive() coroutine via push", async () => {
    await runSelfReceiveTest(ao, "Mode 6")
  })

  it("cross-process receive: PingChain → Pong via push", async () => {
    await runCrossProcessReceiveHBTest(ao, "Mode 6")
  })
})
