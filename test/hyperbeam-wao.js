import assert from "assert"
import { after, describe, it, before, beforeEach } from "node:test"
import { acc } from "../src/test.js"
import { wait, toAddr } from "../src/utils.js"
import { connect, createSigner } from "@permaweb/aoconnect"
import { spawn } from "child_process"
import { resolve } from "path"
import { readFileSync } from "fs"
import { exec } from "child_process"
import { send, verify, createRequest } from "../src/signer.js"
import { run } from "../src/hyperbeam-server.js"

const _env = {
  //DIAGNOSTIC: "1",
  CMAKE_POLICY_VERSION_MINIMUM: "3.5",
  CC: "gcc-12",
  CXX: "g++-12",
}

const genEval = (port = 10001, sport = 4000, wallet = ".wallet.json") => {
  return `hb:start_mainnet(#{ port => ${port}, gateway => <<"http://localhost:${sport}">>, priv_key_location => <<"${wallet}">>}).`
}

const deploy = (_eval, env = {}, cwd = "../HyperBEAM") => {
  const hbeam = spawn("rebar3", ["shell", "--eval", _eval], {
    env: {
      ...process.env,
      ...env,
    },
    cwd: resolve(import.meta.dirname, cwd),
  })
  hbeam.stdout.on("data", chunk => console.log(`stdout: ${chunk}`))
  hbeam.stderr.on("data", err => console.error(`stderr: ${err}`))
  hbeam.on("error", err => console.error(`failed to start process: ${err}`))
  hbeam.on("close", code =>
    console.log(`child process exited with code ${code}`)
  )
  return hbeam
}

describe("HyperBEAM", function () {
  it("should sign http message", async () => {
    const port = 10001
    const hbeam = deploy(genEval(port), _env)
    await wait(5000)
    const signer = createSigner(acc[0].jwk, "http://localhost:10001")
    const request = createRequest({ signer })
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
    const port = 10001
    const hbeam = deploy(genEval(port), _env)
    await wait(5000)
    const signer = createSigner(acc[0].jwk)
    const { request } = connect({
      MODE: "mainnet",
      URL: `http://localhost:${port}`,
      device: "",
      signer,
    })
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
  it.only("should use relay device", async () => {
    const port = 10001
    const hbeam = deploy(genEval(port), _env)
    await wait(5000)
    const server = run()
    const signer = createSigner(acc[0].jwk, "http://localhost:10001")
    const request = createRequest({ signer })

    // In your test, pass the URL in the body
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
})
