import { describe, it, before } from "node:test"
import { AO, acc } from "../../src/test.js"
import {
  runCounterTest,
  runSelfReceiveTest,
  runTokenMintTest,
  runTokenTransferTest,
  runCrossProcessTest,
  runCrossProcessReceiveTest,
} from "./shared.js"

describe("Mode 1: Legacynet Emulator (wao/test)", function () {
  let ao

  before(async () => {
    ao = await new AO().init(acc[0])
  })

  it("counter: deploy, Inc, Get → 1", async () => {
    await runCounterTest(ao, "Mode 1")
  })

  it("self-message receive(): Send({Target=ao.id}).receive()", async () => {
    await runSelfReceiveTest(ao, "Mode 1")
  })

  it("token: deploy, Mint 100, Balance → 100", async () => {
    await runTokenMintTest(ao, "Mode 1")
  })

  it("token transfer: Mint 100, Transfer 30, Balance → 70", async () => {
    await runTokenTransferTest(ao, "Mode 1")
  })

  it("cross-process: token Transfer → receiver Credit-Notice", async () => {
    const ao2 = await new AO({ mem: ao.mem }).init(acc[1])
    await runCrossProcessTest(ao, ao2, "Mode 1", false)
  })

  it("cross-process receive(): Receive() chain across processes", async () => {
    await runCrossProcessReceiveTest(ao, "Mode 1")
  })
})
