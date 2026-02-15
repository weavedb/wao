import assert from "assert"
import { describe, it, before, after } from "node:test"
import { acc, Server } from "../../src/test.js"
import AO from "../../src/ao.js"
import {
  runCounterTest,
  runSelfReceiveTest,
  runTokenMintTest,
  runTokenTransferTest,
  runCrossProcessTest,
  chainSenderSrc,
  chainResponderSrc,
} from "./shared.js"

describe("Mode 2: Standalone Local AO Server", function () {
  let server, ao

  before(async () => {
    server = new Server({ port: 4100 })
    ao = await new AO({ port: 4100 }).init(acc[0])
  })

  after(async () => {
    if (server) await server.end()
  })

  it("counter: deploy, Inc, Get → 1", async () => {
    await runCounterTest(ao, "Mode 2")
  })

  it("self-message receive(): Send({Target=ao.id}).receive()", async () => {
    await runSelfReceiveTest(ao, "Mode 2")
  })

  it("token: deploy, Mint 100, Balance → 100", async () => {
    await runTokenMintTest(ao, "Mode 2")
  })

  it("token transfer: Mint 100, Transfer 30, Balance → 70", async () => {
    await runTokenTransferTest(ao, "Mode 2")
  })

  it("cross-process: token Transfer → receiver Credit-Notice", async () => {
    await runCrossProcessTest(ao, ao, "Mode 2", false)
  })

  it("cross-process receive(): Receive() chain across processes", async () => {
    const { p: senderP, err: err1 } = await ao.deploy({
      boot: true,
      src_data: chainSenderSrc,
    })
    assert.ok(!err1, `Mode 2: sender deploy failed: ${err1}`)

    const { pid: responderPid, err: err2 } = await ao.deploy({
      src_data: chainResponderSrc,
    })
    assert.ok(!err2, `Mode 2: responder deploy failed: ${err2}`)

    const { out } = await senderP.msg(
      "PingChain",
      { Recipient: responderPid },
      {
        get: false,
        mode: "gql",
        timeout: 5000,
        check: [{ from: responderPid }],
      }
    )
    assert.equal(
      out,
      "chain:pong-hello",
      `Mode 2: expected "chain:pong-hello", got "${out}"`
    )
  })
})
