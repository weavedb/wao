import assert from "assert"
import { after, describe, beforeEach, it, before } from "node:test"
import HyperBEAM from "../../src/hyperbeam.js"
import { run } from "../../src/hyperbeam-server.js"
import HB from "../../src/hb.js"
import { getJWK } from "../lib/test-utils.js"
import { wait, toAddr } from "../../src/utils.js"
import { acc } from "../../src/test.js"
import { connect, createSigner } from "@permaweb/aoconnect"
import { resolve } from "path"
import { readFileSync } from "fs"
import { exec } from "child_process"
import { send, verify } from "../../src/signer.js"

describe("HyperBEAM", function () {
  let hb, hbeam, hb2, addr, jwk, jwk2
  before(async () => {
    hbeam = new HyperBEAM({ c: "12", cmake: "3.5", gateway: 4000 })
    await wait(5000)
    jwk = getJWK("../../HyperBEAM/.wallet.json")
    addr = toAddr(jwk.n)
    jwk2 = getJWK("../../HyperBEAM/hyperbeam-key.json")
    run()
  })
  beforeEach(async () => {
    hb = await new HB({}).init(jwk)
    hb2 = await new HB({}).init(jwk2)
  })

  after(async () => {
    hbeam.kill("SIGKILL")
  })

  it("should sign http message", async () => {
    const msg = await hb._request2({
      path: "/~wao@1.0/info",
      method: "POST",
      data: "test",
    })
    console.log(msg)
    assert((await verify(msg)).verified)
    const version = (await send(msg)).version
  })

  it("should query wao device", async () => {
    console.log(await hb.send({ method: "POST", path: "/~wao@1.0/info" }))
    const version = (
      await hb.send({ method: "POST", path: "/~wao@1.0/info" })
    ).headers.get("version")
    assert.equal(version, "1.0")
    const addr = (
      await hb.send({
        method: "GET",
        path: "/~meta@1.0/info/address",
      })
    ).body
    const addr2 = toAddr(
      JSON.parse(
        readFileSync(
          resolve(import.meta.dirname, "../../HyperBEAM/.wallet.json"),
          "utf8"
        )
      ).n
    )
    assert.equal(addr, addr2)
  })

  it("should use relay device", async () => {
    const msg = await hb.send({
      path: "/~relay@1.0/call",
      method: "POST",
      "relay-path": "http://localhost:4000/relay",
      "relay-method": "POST",
      "relay-body": "test",
    })
    assert.deepEqual(JSON.parse(msg.body), {
      success: true,
      body: "test",
    })
  })

  it("should use wao device", async () => {
    const msg = await hb.send({
      path: "/~wao@1.0/relay",
      method: "POST",
      "forward-to": "http://localhost:4000/relay",
      "forward-method": "POST",
      "forward-body": "test",
    })
    assert.deepEqual(JSON.parse(msg.body), { success: true, body: "test" })
  })
})
