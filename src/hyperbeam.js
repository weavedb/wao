import { spawn } from "child_process"
import { resolve } from "path"
import { isNil, map } from "ramda"
import { rmSync, readFileSync, readdirSync } from "fs"

export default class HyperBEAM {
  constructor({
    port = 10001,
    cu = 6363,
    bundler,
    gateway,
    wallet,
    clearCache,
    cwd = "./HyperBEAM",
    c,
    cmake,
    legacy = false,
    faff,
    simplePay = false,
    simplePayPrice,
    p4_lua,
    store_prefix,
    operator,
  } = {}) {
    if (clearCache) {
      const dirname = resolve(process.cwd(), cwd)
      for (let v of readdirSync(dirname)) {
        if (/^cache-/.test(v)) {
          try {
            rmSync(resolve(dirname, v), { recursive: true, force: true })
          } catch (e) {
            console.log(e)
          }
        }
      }
    }
    this.cu = cu
    this.store_prefix = store_prefix
      ? "cache-mainnet-" + Math.floor(Math.random() * 10000000)
      : "cache-mainnet"
    this.p4_lua = p4_lua
    this.simplePay = simplePay
    this.spp = simplePayPrice
    this.operator = operator
    this.faff = faff
    this.c = c
    this.cmake = cmake
    this.port = port
    this.bundler = bundler
    this.hbeam = spawn(
      "rebar3",
      ["shell", "--eval", this.genEval({ gateway, wallet })],
      {
        env: { ...process.env, ...this.genEnv() },
        cwd: resolve(process.cwd(), cwd),
      }
    )
    this.hbeam.stdout.on("data", chunk => console.log(chunk.toString()))
    this.hbeam.stderr.on("data", err => console.error(err.toString()))
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

  genEval({ gateway, wallet = ".wallet.json" }) {
    const _wallet = `, priv_key_location => <<"${wallet}">>`
    const _gateway = gateway
      ? `, gateway => <<"http://localhost:${gateway}">>`
      : ""

    // store option will be overwritten by hb.erl
    const _store = this.store_prefix
      ? `, store => [#{ <<"store-module">> => hb_store_fs, <<"prefix">> => <<"${this.store_prefix}">> }, #{ <<"store-module">> => hb_store_gateway, <<"subindex">> => [#{ <<"name">> => <<"Data-Protocol">>, <<"value">> => <<"ao">> }], <<"store">> => [#{ <<"store-module">> => hb_store_fs, <<"prefix">> => <<"${this.store_prefix}">> }] }, #{ <<"store-module">> => hb_store_gateway, <<"store">> => [#{ <<"store-module">> => hb_store_fs, <<"prefix">> => <<"${this.store_prefix}">> }] }]`
      : ""
    const _bundler = this.bundler
      ? `, bundler_httpsig => <<"http://localhost:${this.bundler}">>`
      : ""
    const _routes = `, routes => [#{ <<"template">> => <<"/result/.*">>, <<"node">> => #{ <<"prefix">> => <<"http://localhost:${this.cu}">> } }, #{ <<\"template\">> => <<\"/dry-run\">>, <<\"node\">> => #{ <<\"prefix\">> => <<\"http://localhost:${this.cu}\">> } }, #{ <<"template">> => <<"/graphql">>, <<"nodes">> => [#{ <<"prefix">> => <<"http://localhost:${gateway}">>, <<"opts">> => #{ http_client => httpc, protocol => http2 } }, #{ <<"prefix">> => <<"http://localhost:${gateway}">>, <<"opts">> => #{ http_client => gun, protocol => http2 } }] }, #{ <<"template">> => <<"/raw">>, <<"node">> => #{ <<"prefix">> => <<"http://localhost:${gateway}">>, <<"opts">> => #{ http_client => gun, protocol => http2 } } }]`

    const _p4_non_chargable = this.p4_lua
      ? `, p4_non_chargable_routes => [#{ <<"template">> => <<"/*~node-process@1.0/*">> }, #{ <<"template">> => <<"/~wao@1.0/*">> }, #{ <<"template">> => <<"/~p4@1.0/balance">> }, #{ <<"template">> => <<"/~meta@1.0/*">> }]`
      : !this.simplePay
        ? ""
        : `, p4_non_chargable_routes => [#{ <<"template">> => <<"/~simple-pay@1.0/topup">> }, #{ <<"template">> => <<"/~p4@1.0/balance">> }, #{ <<"template">> => <<"/~meta@1.0/*">> }, #{ <<"template">> => <<"/~p4@1.0/balance">> }, #{ <<"template">> => <<"/~meta@1.0/*">> }]`
    const _operator = this.operator
      ? `, operator => <<"${this.operator}">>`
      : ""
    const _spp = this.spp ? `, simple_pay_price => ${this.spp}` : ""

    const _node_processes = this.p4_lua
      ? `, node_processes => #{ <<"ledger">> => #{ <<"device">> => <<"process@1.0">>, <<"execution-device">> => <<"lua@5.3a">>, <<"scheduler-device">> => <<"scheduler@1.0">>, <<"module">> => <<"${this.p4_lua.processor}">>, <<"operator">> => <<"${this.operator}">> } }`
      : ""
    const processor = this.p4_lua
      ? `#{ <<"device">> => <<"p4@1.0">>, <<"pricing-device">> => <<"simple-pay@1.0">>, <<"ledger-device">> => <<"lua@5.3a">>, <<"module">> => <<"${this.p4_lua.client}">>, <<"ledger-path">> => <<"/ledger~node-process@1.0">> }`
      : ""
    const _port = `port => ${this.port}`
    const _faff = isNil(this.faff)
      ? ""
      : `, faff_allow_list => [ ${map(addr => `<<"${addr}">>`)(this.faff).join(", ")} ]`

    const _on = this.p4_lua
      ? `, on => #{ <<"request">> => ${processor}, <<"response">> => ${processor} }`
      : this.simplePay
        ? `, on => #{ <<"request">> => #{ <<"device">> => <<"p4@1.0">>, <<"pricing-device">> => <<"simple-pay@1.0">>, <<"ledger-device">> => <<"simple-pay@1.0">> }, <<"response">> => #{ <<"device">> => <<"p4@1.0">>, <<"pricing-device">> => <<"simple-pay@1.0">>, <<"ledger-device">> => <<"simple-pay@1.0">> } }`
        : !isNil(this.faff)
          ? `, on => #{ <<"request">> => #{ <<"device">> => <<"p4@1.0">>, <<"pricing-device">> => <<"faff@1.0">>, <<"ledger-device">> => <<"faff@1.0">> }, <<"response">> => #{ <<"device">> => <<"p4@1.0">>, <<"pricing-device">> => <<"faff@1.0">>, <<"ledger-device">> => <<"faff@1.0">> } }`
          : ""
    const start = `hb:start_mainnet(#{ ${_port}${_gateway}${_wallet}${_faff}${_bundler}${_routes}${_on}${_p4_non_chargable}${_operator}${_spp}${_node_processes}}).`
    return start
  }

  kill() {
    this.hbeam.kill("SIGKILL")
  }
}
