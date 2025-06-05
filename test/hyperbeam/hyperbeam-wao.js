import assert from "assert"
import { after, describe, it, before, beforeEach } from "node:test"
import { acc } from "../src/test.js"
import { wait, toAddr } from "../src/utils.js"
import { prepare } from "./test-utils.js"
import { connect, createSigner } from "@permaweb/aoconnect"
import { resolve } from "path"
import { readFileSync } from "fs"
import { exec } from "child_process"
import { send, verify } from "../src/signer.js"

describe("HyperBEAM", function () {
  it("should sign http message", async () => {
    const { hbeam, server, request } = await prepare()
    const msg = await request({
      path: "/~wao@1.0/info",
      method: "POST",
      data: "test",
    })
    console.log(msg)
    assert((await verify(msg)).verified)
    const version = (await send(msg)).version
    hbeam.kill("SIGKILL")
  })

  it("should query wao device", async () => {
    const { hbeam, server, request } = await prepare()
    const version = (await request({ method: "POST", path: "/~wao@1.0/info" }))
      .version
    assert.equal(version, "1.0")
    const addr = (
      await request({
        method: "GET",
        path: "/~meta@1.0/info/address",
      })
    ).body
    const addr2 = toAddr(
      JSON.parse(
        readFileSync(
          resolve(import.meta.dirname, "../HyperBEAM/.wallet.json"),
          "utf8"
        )
      ).n
    )
    assert.equal(addr, addr2)
    hbeam.kill("SIGKILL")
  })
  it("should use relay device", async () => {
    const { hbeam, server, request } = await prepare()
    const msg = await request({
      path: "/~relay@1.0/call",
      method: "POST",
      "relay-path": "http://localhost:4000/relay",
      "relay-method": "POST",
      "relay-body": "test",
    })
    assert.deepEqual(JSON.parse((await send(msg)).body), {
      success: true,
      body: "test",
    })
    hbeam.kill("SIGKILL")
    server.close()
  })

  it.only("should use wao device", async () => {
    const { hbeam, server, request } = await prepare()
    const msg = await request({
      path: "/~wao@1.0/relay",
      method: "POST",
      "forward-to": "http://localhost:4000/relay",
      "forward-method": "POST",
      "forward-body": "test",
    })
    const res = await send(msg)
    assert.deepEqual(JSON.parse(res.body), { success: true, body: "test" })
    hbeam.kill("SIGKILL")
    server.close()
  })
})
