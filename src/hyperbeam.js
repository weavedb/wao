import { spawn } from "child_process"
import { resolve } from "path"

export default class HyperBEAM {
  constructor({
    port = 10001,
    gateway,
    wallet,
    cwd = "../HyperBEAM",
    c,
    cmake,
  } = {}) {
    this.c = c
    this.cmake = cmake
    this.port = port
    this.hbeam = spawn(
      "rebar3",
      ["shell", "--eval", this.genEval({ gateway, wallet })],
      {
        env: { ...process.env, ...this.genEnv() },
        cwd: resolve(import.meta.dirname, cwd),
      }
    )
    this.hbeam.stdout.on("data", chunk => console.log(`stdout: ${chunk}`))
    this.hbeam.stderr.on("data", err => console.error(`stderr: ${err}`))
    this.hbeam.on("error", err =>
      console.error(`failed to start process: ${err}`)
    )
    this.hbeam.on("close", code =>
      console.log(`child process exited with code ${code}`)
    )
    return this.hbeam
  }
  genEnv() {
    let _env = {}
    if (this.c) {
      _env.CC = `gcc-${this.c}`
      _env.CXX = `g++-${this.c}`
    }
    if (this.cmake) _env.CMAKE_POLICY_VERSION_MINIMUM = this.cmake
    return _env
  }
  genEval({ gateway = 4000, wallet = ".wallet.json" }) {
    const _wallet = `, priv_key_location => <<"${wallet}">>`
    const _gateway = gateway
      ? `, gateway => <<"http://localhost:${gateway}">>`
      : ""
    return `hb:start_mainnet(#{ port => ${this.port}${_gateway}${_wallet} }).`
  }
  kill() {
    this.hbeam.kill("SIGKILL")
  }
}
