import { connect, createSigner } from "@permaweb/aoconnect"
import { send as _send, verify, createRequest } from "../src/signer.js"
import { wait, toAddr } from "../src/utils.js"
import { acc } from "../src/test.js"
import { run } from "../src/hyperbeam-server.js"
import { spawn } from "child_process"
import { resolve } from "path"
import { readFileSync } from "fs"

const prepare = async (port = 10001, port2 = 4000, jwk) => {
  const _eval = genEval(port)
  console.log(_eval)
  const hbeam = deploy(_eval, _env)
  await wait(5000)
  const server = run(port2)
  jwk ??= acc[0].jwk
  const signer = createSigner(jwk, `http://localhost:${port}`)
  const request = createRequest({ signer })
  const send = async args => {
    const msg = await request(args)
    return await _send(msg)
  }
  return { server, hbeam, request, send, addr: toAddr(jwk.n) }
}

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
const getJWK = file => {
  return JSON.parse(readFileSync(resolve(import.meta.dirname, file), "utf8"))
}

export { prepare, getJWK }
