import assert from "assert"
import { after, describe, it, before, beforeEach } from "node:test"
import { wait } from "../src/utils.js"
import { prepare } from "./test-utils.js"

describe("HyperBEAM Template", function () {
  let hbeam, server, send
  before(async () => {
    ;({ hbeam, server, send } = await prepare())
  })
  after(async () => {
    hbeam.kill("SIGKILL")
    server.close()
  })
  it.only("should use meta device", async () => {
    const res = await send({ path: "/~meta@1.0/info", method: "GET" })
    console.log(res)
  })
})
