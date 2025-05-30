import assert from "assert"
import { after, describe, it, before, beforeEach } from "node:test"
import { acc } from "../src/test.js"
import { connect, createSigner } from "@permaweb/aoconnect"
import { spawn } from "child_process"
import { resolve } from "path"
import { readFileSync } from "fs"
import { exec } from "child_process"
import sha256 from "fast-sha256"
function base64urlDecode(str) {
  str = str.replace(/-/g, "+").replace(/_/g, "/")
  const pad = str.length % 4
  if (pad === 2) str += "=="
  else if (pad === 3) str += "="
  else if (pad !== 0) throw new Error("Invalid base64url string")
  const bin = atob(str)
  const bytes = new Uint8Array(bin.length)
  for (let i = 0; i < bin.length; i++) bytes[i] = bin.charCodeAt(i)
  return bytes
}

function base64urlEncode(bytes) {
  let bin = ""
  for (const b of bytes) bin += String.fromCharCode(b)
  let b64 = btoa(bin)
  return b64.replace(/\+/g, "-").replace(/\//g, "_").replace(/=+$/g, "")
}

function toAddr(n) {
  const pubBytes = base64urlDecode(n)
  const hash = sha256(pubBytes)
  return base64urlEncode(hash)
}

const wait = ms => new Promise(res => setTimeout(() => res(), ms))
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
  it.only("should query wao device", async () => {
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
})
