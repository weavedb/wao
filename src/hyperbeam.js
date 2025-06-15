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
    legacy = false,
  } = {}) {
    this.c = c
    this.cmake = cmake
    this.port = port
    this.hbeam = spawn(
      "rebar3",
      ["shell", "--eval", this.genEval({ gateway, wallet, legacy })],
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
  genEval({ gateway, wallet = ".wallet.json", legacy = false }) {
    const _wallet = `, priv_key_location => <<"${wallet}">>`
    const _gateway = gateway
      ? `, gateway => <<"http://localhost:${gateway}">>`
      : ""
    const _store = `, store => [#{ <<"store-module">> => hb_store_fs, <<"prefix">> => <<"cache-mainnet">> }, #{ <<"store-module">> => hb_store_gateway, <<"subindex">> => [#{ <<"name">> => <<"Data-Protocol">>, <<"value">> => <<"ao">> }], <<"store">> => [#{ <<"store-module">> => hb_store_fs, <<"prefix">> => <<"cache-mainnet">> }] }, #{ <<"store-module">> => hb_store_gateway, <<"store">> => [#{ <<"store-module">> => hb_store_fs, <<"prefix">> => <<"cache-mainnet">> }] }]`
    const _bundler = `, bundler_httpsig => <<"http://localhost:4001">>`
    const _routes = `, routes => [#{ <<"template">> => <<"/result/.*">>, <<"node">> => #{ <<"prefix">> => <<"http://localhost:6363">> } }, #{ <<"template">> => <<"/graphql">>, <<"nodes">> => [#{ <<"prefix">> => <<"http://localhost:${gateway}">>, <<"opts">> => #{ http_client => httpc, protocol => http2 } }, #{ <<"prefix">> => <<"http://localhost:${gateway}">>, <<"opts">> => #{ http_client => gun, protocol => http2 } }] }, #{ <<"template">> => <<"/raw">>, <<"node">> => #{ <<"prefix">> => <<"http://localhost:${gateway}">>, <<"opts">> => #{ http_client => gun, protocol => http2 } } }]`
    const _port = `port => ${this.port}`
    return !legacy
      ? `hb:start_mainnet(#{ ${_port}${_gateway}${_wallet}${_store} }).`
      : `hb:start_mainnet(#{ ${_port}${_gateway}${_wallet}${_store}${_bundler}${_routes} }).`
  }

  kill() {
    this.hbeam.kill("SIGKILL")
  }
}
