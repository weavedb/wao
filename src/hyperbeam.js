import { spawn, spawnSync } from "child_process"
import { resolve } from "path"
import { isNil, map } from "ramda"
import { toAddr } from "./test.js"
import HB from "./hb.js"
import { rmSync, readFileSync, readdirSync, writeFileSync, existsSync } from "fs"
import devs from "./devs.js"
import dotenv from "dotenv"
dotenv.config({ path: ".env.hyperbeam" })

export default class HyperBEAM {
  static OPERATOR = Symbol("operator")
  constructor({
    port = 10001,
    cu_port = 6363,
    as = [],
    bundler,
    gateway,
    wallet = ".wallet.json",
    reset,
    cwd = process.env.CWD ?? "./HyperBEAM",
    c,
    cmake,
    faff,
    simple_pay = false,
    simple_pay_price,
    bundler_ans104,
    bundler_httpsig,
    p4_non_chargable_routes,
    p4_lua,
    store_prefix,
    operator,
    logs = true,
    shell = true,
    devices,
    genesis_wasm = false,
    arweave_gateway,
    force_signed = false,
    linkify_mode, // v0.9-FINAL: HB linkify mode. undefined => HB default; pass "false" for hbsig-style inline-only responses
    rebar3, // Use rebar3 shell (true) or direct erl (false). Defaults to HB_REBAR3 env or true
  } = {}) {
    // Determine rebar3 mode: option > env var > default (true)
    const envRebar3 = process.env.HB_REBAR3
    if (rebar3 !== undefined) {
      this.rebar3 = rebar3
    } else if (envRebar3 !== undefined) {
      this.rebar3 = envRebar3.toLowerCase() !== "false"
    } else {
      this.rebar3 = true // default to rebar3 mode
    }
    this.genesis_wasm = genesis_wasm
    this.force_signed = force_signed
    this.linkify_mode = linkify_mode
    this.cu_port = cu_port
    this.arweave_gateway = arweave_gateway || process.env.ARWEAVE_GATEWAY
    this.devices = devices
    this.p4_non_chargable_routes = p4_non_chargable_routes
    this.logs = logs
    this.cwd = cwd
    this.dirname = resolve(process.cwd(), this.cwd)
    this.wallet = wallet
    this.wallet_location = resolve(this.dirname, this.wallet)
    this.jwk = JSON.parse(this.file(this.wallet_location))
    this.addr = toAddr(this.jwk.n)
    if (reset) {
      for (let v of readdirSync(this.dirname)) {
        if (/^cache-/.test(v)) {
          try {
            rmSync(resolve(this.dirname, v), { recursive: true, force: true })
          } catch (e) {
            console.log(e)
          }
        }
      }
    }
    //this.cu = cu
    this.store_prefix = store_prefix
      ? "cache-mainnet-" + Math.floor(Math.random() * 10000000)
      : "cache-mainnet"
    this.p4_lua = p4_lua
    this.simple_pay = simple_pay
    this.spp = simple_pay_price
    this.operator = operator
    if (this.operator === HyperBEAM.OPERATOR) this.operator = this.addr
    this.faff = faff
    this.c = c
    this.cmake = cmake
    this.port = port
    this.url = `http://127.0.0.1:${this.port}`
    if (bundler) this.bundler = `http://localhost::${bundler}`
    this.bundler_ans104 = bundler_ans104
    if (bundler_httpsig) this.bundler = bundler_httpsig
    this.as = as
    this.gateway = gateway
    if (Array.isArray(this.faff)) {
      let i = 0
      for (let v of this.faff) {
        if (typeof v === "symbol" && v === HyperBEAM.OPERATOR) {
          this.faff[i] = this.addr
        }
        i++
      }
    }
    if (shell) this.shell()
  }
  shell() {
    const evalCmd = this.genEval({ gateway: this.gateway, wallet: this.wallet })
    const cwd = resolve(process.cwd(), this.cwd)
    const env = this.genEnv() // genEnv() returns filtered process.env without proxy vars

    if (this.rebar3) {
      // rebar3 shell mode
      const _as = this.as.length === 0 ? [] : ["as", this.as.join(",")]
      this._shell = spawn(
        "rebar3",
        [
          ..._as,
          "shell",
          "--eval",
          evalCmd,
        ],
        { env, cwd }
      )
    } else {
      // Direct erl mode - compile first if needed, then start
      // This mode is better for proxy environments as it gives more control
      // Manually expand glob pattern to avoid shell interpretation issues
      const buildDir = resolve(cwd, "_build/default/lib")
      let ebinDirs = []
      try {
        const libs = readdirSync(buildDir)
        for (const lib of libs) {
          const ebinPath = resolve(buildDir, lib, "ebin")
          if (existsSync(ebinPath)) {
            ebinDirs.push(ebinPath)
          }
        }
      } catch (e) {
        console.error("Failed to enumerate ebin directories:", e.message)
      }

      // Build -pa arguments for each ebin directory
      const paArgs = ebinDirs.flatMap(dir => ["-pa", dir])

      this._shell = spawn(
        "erl",
        [
          "+A", "4",  // Async threads for WAMR linked-in driver (see hb_beamr.erl)
          ...paArgs,
          "-sname", `hb_${this.port}`,  // Unique node name to allow multiple instances
          "-eval", evalCmd,
        ],
        { env, cwd }
      )
    }

    if (this.logs) {
      this._shell.stdout.on("data", chunk => console.log(chunk.toString()))
      this._shell.stderr.on("data", err => console.error(err.toString()))
      this._shell.on("error", err =>
        console.error(`failed to start process: ${err}`)
      )
      this._shell.on("close", code => {
        console.log(`child process exited with code ${code}`)
        delete this._shell
      })
    }
  }
  file(path, type = "utf8") {
    return readFileSync(resolve(this.dirname, path), type)
  }
  eunit(module, test) {
    return new Promise(res => {
      let isTest = !isNil(test)
      if (Array.isArray(module)) {
        for (const v of module) {
          if (Array.isArray(v) || /:/.test(v)) {
            isTest = true
            break
          }
        }
      }
      const _as = this.as.length === 0 ? [] : ["as", this.as.join(",")]
      const _test = Array.isArray(test) ? test.join("+") : test
      let _module = ""

      if (Array.isArray(module)) {
        for (const v of module) {
          _module += _module === "" ? "" : ","
          if (Array.isArray(v)) _module += `${v[0]}:${v[1].join("+")}`
          else _module += v
        }
      } else {
        _module = test ? `${module}:${_test}` : module
      }

      const _arg = isTest ? "--test" : "--module"
      let params = [..._as, "eunit", _arg, _module]
      const _eunit = spawn("rebar3", params, {
        env: this.genEnv(),
        cwd: resolve(process.cwd(), this.cwd),
      })
      if (this.logs) {
        _eunit.stdout.on("data", chunk => console.log(chunk.toString()))
        _eunit.stderr.on("data", err => console.error(err.toString()))
        _eunit.on("error", err =>
          console.error(`failed to start process: ${err}`)
        )
        _eunit.on("close", code => {
          console.log(`child process exited with code ${code}`)
          res()
        })
      }
    })
  }
  async ok() {
    try {
      const address = await fetch(`${this.url}/~meta@1.0/info/address`).then(
        r => r.text()
      )
      if (address) {
        if (this.logs) console.log("HyperBEAM ok(): initializing HB...")
        this.hb = await new HB({ url: this.url }).init(this.jwk)
        this._info = { address }
        if (this.logs) console.log("HyperBEAM ok(): SUCCESS!")
        return true
      } else return false
    } catch (e) {
      if (this.logs) console.error("HyperBEAM ok() error:", e.message)
      return false
    }
  }
  async ready(timeout = 60000) {
    // Start CU server if genesis_wasm is enabled
    if (this.genesis_wasm) {
      await this.startCU()
    }

    const start = Date.now()
    while (Date.now() - start < timeout) {
      try {
        if (await this.ok()) {
          return this
        }
      } catch (e) {
        // Ignore errors, will retry
      }
      // Wait 1 second before next attempt
      await new Promise(r => setTimeout(r, 1000))
    }
    return false
  }

  // Start the genesis-wasm CU server
  async startCU() {
    const cuDir = resolve(this.dirname, "genesis-wasm-server")
    const dbDir = resolve(this.dirname, "cache-mainnet/genesis-wasm")

    // Ensure DB directory exists
    spawnSync("mkdir", ["-p", dbDir])

    // Kill any stale CU process listening on cu_port before spawning a new
    // one. Sequential test runs in the same OS share port 6363; if a prior
    // run's CU lingered (e.g. detached but its parent died before SIGKILL
    // could propagate), the new CU's bind silently fails and HB ends up
    // talking to the stale CU, which has a different process registry and
    // throws confusing 500/400s on the next spawn/schedule.
    try {
      spawnSync("bash", ["-c", `lsof -ti:${this.cu_port} | xargs -r kill -9 2>/dev/null`], { stdio: "ignore" })
    } catch (_e) {}
    // Brief settle so the OS can release the port.
    await new Promise(r => setTimeout(r, 200))

    // Use arweave_gateway option or ARWEAVE_GATEWAY env var for proxy environments
    const gatewayUrl = this.arweave_gateway || process.env.GATEWAY_URL || "https://arweave.net"
    const graphqlUrl = process.env.GRAPHQL_URL || `${gatewayUrl}/graphql`

    // CU needs proxy for external services (arweave.net) but not for localhost
    // Keep all env vars but ensure NO_PROXY is set for localhost connections
    const noProxy = 'localhost,127.0.0.1,::1'
    const env = {
      ...process.env,
      // Ensure NO_PROXY includes localhost for Node.js fetch and global-agent
      NO_PROXY: noProxy,
      no_proxy: noProxy,
      GLOBAL_AGENT_NO_PROXY: noProxy,
      UNIT_MODE: "hbu",
      HB_URL: `http://localhost:${this.port}`,
      NODE_CONFIG_ENV: "development",
      DB_URL: resolve(dbDir, "genesis-wasm-db"),
      PORT: String(this.cu_port),
      WALLET_FILE: this.wallet_location,
      DISABLE_PROCESS_FILE_CHECKPOINT_CREATION: "false",
      PROCESS_MEMORY_FILE_CHECKPOINTS_DIR: resolve(dbDir, "checkpoints"),
      GATEWAY_URL: gatewayUrl,
      ARWEAVE_URL: gatewayUrl,
      GRAPHQL_URL: graphqlUrl,
      GRAPHQL_URLS: graphqlUrl,
      CHECKPOINT_GRAPHQL_URL: graphqlUrl,
    }

    // Node 26 enables wasm-memory64 by default and rejects the experimental
    // flag; older Node versions still need it. Detect from process.versions.
    const nodeMajor = parseInt((process.versions.node || "0").split(".")[0], 10)
    const memory64Flag = nodeMajor >= 24 ? [] : ["--experimental-wasm-memory64"]
    this.cuProc = spawn("node", [...memory64Flag, "-r", "dotenv/config", "src/app.js"], {
      cwd: cuDir,
      env,
      detached: true,
      stdio: this.logs ? ["ignore", "pipe", "pipe"] : "ignore"
    })

    this.cuProc.unref()

    if (this.logs) {
      console.log(`CU server starting on port ${this.cu_port}...`)
      if (this.cuProc.stdout) {
        this.cuProc.stdout.on("data", chunk => console.log(`[CU] ${chunk.toString().trim()}`))
      }
      if (this.cuProc.stderr) {
        this.cuProc.stderr.on("data", chunk => console.error(`[CU] ${chunk.toString().trim()}`))
      }
    }

    // Wait for CU to be ready - check / endpoint instead of /status
    const start = Date.now()
    while (Date.now() - start < 30000) {
      try {
        const res = await fetch(`http://localhost:${this.cu_port}/`)
        if (res.ok || res.status === 404) {
          // Any response (including 404) means server is up
          if (this.logs) console.log("CU server ready")
          return true
        }
      } catch (e) {
        // Not ready yet
      }
      await new Promise(r => setTimeout(r, 500))
    }
    if (this.logs) console.log("CU server startup timeout, continuing anyway...")
    return true // Continue anyway, the CU process is running
  }
  genEnv() {
    // Start with process.env but filter out proxy settings
    // HyperBEAM uses arweave_gateway config for external access, not proxy
    // This avoids httpc proxy issues with localhost CU connections
    const proxyKeys = ['HTTPS_PROXY', 'HTTP_PROXY', 'https_proxy', 'http_proxy', 'ALL_PROXY', 'all_proxy']
    let _env = Object.fromEntries(
      Object.entries(process.env).filter(([key]) => !proxyKeys.includes(key))
    )
    if (this.diagnostic) _env.DIAGNOSTIC = this.diagnostic
    if (this.c) {
      _env.CC = `gcc-${this.c}`
      _env.CXX = `g++-${this.c}`
    }
    if (this.cmake) _env.CMAKE_POLICY_VERSION_MINIMUM = this.cmake
    // Ensure enough async threads for WAMR linked-in driver + HTTP client
    // See hb_beamr.erl: "configure BEAM to have enough async worker threads (see erl +A N)"
    const asyncThreads = "+A 4"
    if (!_env.ERL_ZFLAGS) _env.ERL_ZFLAGS = asyncThreads
    else if (!_env.ERL_ZFLAGS.includes("+A")) _env.ERL_ZFLAGS += ` ${asyncThreads}`
    // Skip the hb application's default-port HTTP server start (8734). We call
    // hb_http_server:start_node/1 explicitly with the actual port via genEval,
    // so the default binding is redundant; skipping it avoids eaddrinuse and
    // the downstream case_clause crashes when multiple HyperBEAM instances
    // run side-by-side (p4.test.js, p4-lua.test.js).
    _env.WAO_NO_DEFAULT_HTTP_SERVER = "1"
    return _env
  }

  genEval({ gateway, wallet = ".wallet.json" }) {
    let _devices = ""
    let _devs = []
    if (this.devices) {
      for (const v of this.devices) {
        if (typeof v === "object") {
          _devs.push(
            `#{<<"name">> => <<"${v.name}">>, <<"module">> => ${v.module}}`
          )
        } else if (devs[v])
          _devs.push(
            `#{<<"name">> => <<"${devs[v].name}">>, <<"module">> => ${devs[v].module}}`
          )
      }
    }
    if (_devs.length > 0) {
      _devices = `, <<"preloaded-devices">> => [${_devs.join(", ")}]`
    }
    const _wallet = `, <<"priv-key-location">> => <<"${wallet}">>`
    // Use arweave_gateway (Cloudflare proxy) if set, otherwise local gateway port, otherwise default
    const _gateway = this.arweave_gateway
      ? `, <<"gateway">> => <<"${this.arweave_gateway}">>`
      : gateway
        ? `, <<"gateway">> => <<"http://localhost:${gateway}">>`
        : ""

    // Store config: use single hb_store_fs matching HyperBEAM eunit test pattern.
    // Multi-store with hb_store_gateway wrappers caused snapshot discovery failures
    // because list_numbered/resolve interactions across stores break symlink following.
    // The wao@1.0 device handles Arweave TX resolution independently via HTTP.
    const _store = this.store_prefix
      ? `, <<"store">> => #{ <<"store-module">> => hb_store_fs, <<"name">> => <<"${this.store_prefix}">> }`
      : ""
    let _bundler = this.bundler
      ? `, <<"bundler-httpsig">> => <<"${this.bundler}">>`
      : ""
    // Only include bundler_ans104 if it's a truthy value (port number or URL)
    // When false or omitted, don't include it - Erlang code expects either no option or a valid URL
    let _bundler_ans104 = this.bundler_ans104 && this.bundler_ans104 !== false
      ? `, <<"bundler-ans104">> => <<"http://localhost:${this.bundler_ans104}">>`
      : ""
    /*
    const _routes = `, routes => [#{ <<"template">> => <<"/result/.*">>, <<"node">> => #{ <<"prefix">> => <<"http://localhost:${this.cu}">> } }, #{ <<\"template\">> => <<\"/dry-run\">>, <<\"node\">> => #{ <<\"prefix\">> => <<\"http://localhost:${this.cu}\">> } }, #{ <<"template">> => <<"/graphql">>, <<"nodes">> => [#{ <<"prefix">> => <<"http://localhost:${gateway}">>, <<"opts">> => #{ http_client => httpc, protocol => http2 } }, #{ <<"prefix">> => <<"http://localhost:${gateway}">>, <<"opts">> => #{ http_client => gun, protocol => http2 } }] }, #{ <<"template">> => <<"/raw">>, <<"node">> => #{ <<"prefix">> => <<"http://localhost:${gateway}">>, <<"opts">> => #{ http_client => gun, protocol => http2 } } }]`
    */
    const _p4_non_chargable = this.p4_non_chargable
      ? `, <<"p4-non-chargable-routes">> => [${this.p4_non_chargable_routes
          .map(() => `#{ <<"template">> => <<"/*~node-process@1.0/*">> }`)
          .join(", ")}]`
      : this.p4_lua
        ? `, <<"p4-non-chargable-routes">> => [#{ <<"template">> => <<"/*~node-process@1.0/*">> }, #{ <<"template">> => <<"/~wao@1.0/*">> }, #{ <<"template">> => <<"/~p4@1.0/balance">> }, #{ <<"template">> => <<"/~meta@1.0/*">> }]`
        : !this.simple_pay
          ? ""
          : `, <<"p4-non-chargable-routes">> => [#{ <<"template">> => <<"/~simple-pay@1.0/topup">> }, #{ <<"template">> => <<"/~meta@1.0/*">> }, #{ <<"template">> => <<"/~simple-pay@1.0/balance">> }]`

    const _operator = this.operator
      ? `, <<"operator">> => <<"${this.operator}">>`
      : ""
    const _spp = this.spp ? `, <<"simple-pay-price">> => ${this.spp}` : ""
    const _genesis_wasm_port = this.genesis_wasm ? `, <<"genesis-wasm-port">> => ${this.cu_port}` : ""
    const _force_signed = this.force_signed ? `, <<"force-signed-requests">> => true, <<"force-signed">> => true` : ""

    // Helper to format module(s) for Erlang - supports ID string, inline object, or array
    const formatModule = (mod) => {
      if (typeof mod === "string") {
        // ID string
        return `<<"${mod}">>`
      } else if (Array.isArray(mod)) {
        // Array of inline modules
        return `[${mod.map(m => `#{ <<"content-type">> => <<"text/x-lua">>, <<"body">> => <<"${escapeErlangString(m.body)}">>${m.name ? `, <<"name">> => <<"${m.name}">>` : ""} }`).join(", ")}]`
      } else if (mod && mod.body) {
        // Single inline module object
        return `#{ <<"content-type">> => <<"text/x-lua">>, <<"body">> => <<"${escapeErlangString(mod.body)}">>${mod.name ? `, <<"name">> => <<"${mod.name}">>` : ""} }`
      }
      return `<<"${mod}">>`
    }

    // Helper to escape special characters for Erlang binary strings
    const escapeErlangString = (str) => {
      if (!str) return str
      return str.replace(/\\/g, "\\\\").replace(/"/g, '\\"').replace(/\n/g, "\\n").replace(/\r/g, "\\r").replace(/\t/g, "\\t")
    }

    const _node_processes = this.p4_lua
      ? `, <<"node-processes">> => #{ <<"ledger">> => #{ <<"device">> => <<"process@1.0">>, <<"execution-device">> => <<"lua@5.3a">>, <<"scheduler-device">> => <<"scheduler@1.0">>, <<"module">> => ${formatModule(this.p4_lua.processor)}, <<"operator">> => <<"${this.operator}">>${this.p4_lua.admin ? `, <<"admin">> => <<"${this.p4_lua.admin}">>` : ""}${this.p4_lua.balance ? `, <<"balance">> => #{ ${Object.entries(this.p4_lua.balance).map(([k, v]) => `<<"${k}">> => ${v}`).join(", ")} }` : ""} } }`
      : ""
    const processor = this.p4_lua
      ? `#{ <<"device">> => <<"p4@1.0">>, <<"pricing-device">> => <<"simple-pay@1.0">>, <<"ledger-device">> => <<"lua@5.3a">>, <<"module">> => ${formatModule(this.p4_lua.client)}, <<"ledger-path">> => <<"/ledger~node-process@1.0">> }`
      : ""
    const _port = `<<"port">> => ${this.port}`
    const _faff = isNil(this.faff)
      ? ""
      : `, <<"faff-allow-list">> => [ ${map(addr => `<<"${addr}">>`)(this.faff).join(", ")} ]`

    const _on = this.p4_lua
      ? `, <<"on">> => #{ <<"request">> => ${processor}, <<"response">> => ${processor} }`
      : this.simple_pay
        ? `, <<"on">> => #{ <<"request">> => #{ <<"device">> => <<"p4@1.0">>, <<"pricing-device">> => <<"simple-pay@1.0">>, <<"ledger-device">> => <<"simple-pay@1.0">> }, <<"response">> => #{ <<"device">> => <<"p4@1.0">>, <<"pricing-device">> => <<"simple-pay@1.0">>, <<"ledger-device">> => <<"simple-pay@1.0">> } }`
        : !isNil(this.faff)
          ? `, <<"on">> => #{ <<"request">> => #{ <<"device">> => <<"p4@1.0">>, <<"pricing-device">> => <<"faff@1.0">>, <<"ledger-device">> => <<"faff@1.0">> }, <<"response">> => #{ <<"device">> => <<"p4@1.0">>, <<"pricing-device">> => <<"faff@1.0">>, <<"ledger-device">> => <<"faff@1.0">> } }`
          : ""
    // Add cache_writers to allow the wallet to write to cache (needed for WASM module uploads)
    // Use the wallet address (this.addr) which is always available from the wallet file
    const _cache_writers = `, <<"cache-writers">> => [<<"${this.addr}">>]`

    // Use gun HTTP client for relay calls instead of httpc
    // gun doesn't use system proxy settings, avoiding the proxy issue with localhost CU
    const _relay_http_client = `, <<"relay-http-client">> => gun, <<"http-client">> => gun`

    // Custom routes using Cloudflare proxy instead of arweave.net
    // Also add CU routes for genesis_wasm when enabled
    const cuRoutes = this.genesis_wasm
      ? `#{ <<"template">> => <<"/result/*">>, <<"node">> => #{ <<"prefix">> => <<"http://localhost:${this.cu_port}">> } },
          #{ <<"template">> => <<"/snapshot/*">>, <<"node">> => #{ <<"prefix">> => <<"http://localhost:${this.cu_port}">> } },
          #{ <<"template">> => <<"/dry-run">>, <<"node">> => #{ <<"prefix">> => <<"http://localhost:${this.cu_port}">> } },`
      : ""
    const _routes = this.arweave_gateway || this.genesis_wasm
      ? `, <<"routes">> => [
          ${cuRoutes}
          #{ <<"template">> => <<"/graphql">>, <<"nodes">> => [
              #{ <<"prefix">> => <<"${this.arweave_gateway || 'https://arweave.net'}">>, <<"opts">> => #{ http_client => gun, protocol => http2 } }
          ]},
          #{ <<"template">> => <<"/arweave">>, <<"node">> => #{
              <<"match">> => <<"^/arweave">>,
              <<"with">> => <<"${this.arweave_gateway || 'https://arweave.net'}">>,
              <<"opts">> => #{ http_client => gun, protocol => http2 }
          }},
          #{ <<"template">> => <<"/raw">>, <<"node">> => #{
              <<"prefix">> => <<"${this.arweave_gateway || 'https://arweave.net'}">>,
              <<"opts">> => #{ http_client => gun, protocol => http2 }
          }}
        ]`
      : ""

    // Explicitly clear httpc proxy settings at Erlang level before starting HyperBEAM
    // This ensures no proxy is used regardless of any OS-level or cached settings
    const clearProxy = `application:ensure_all_started(inets), httpc:set_options([{proxy, {undefined, []}}, {ipfamily, inet}]), `

    // Force-load dev_hbsig early so its codec functions are available
    // before any device-stack processing occurs
    const loadHbsig = `code:ensure_loaded(dev_hbsig), `

    // Pre-register device name atoms so hb_util:atom/1 (which uses list_to_existing_atom)
    // doesn't crash with badarg when resolving device names from HTTP headers/binaries
    const preRegisterAtoms = `lists:foreach(fun list_to_atom/1, ["wao@1.0", "hbsig@1.0", "stack@1.0", "patch@1.0", "inc@1.0", "double@1.0", "add@1.0", "mul@1.0", "inc2@1.0", "square@1.0", "mydev@1.0", "lua@5.3a", "process@1.0", "scheduler@1.0", "message@1.0", "meta@1.0", "cache@1.0", "json@1.0", "structured@1.0", "httpsig@1.0", "flat@1.0", "genesis-wasm@1.0", "compute@1.0", "delegated-compute@1.0", "relay@1.0", "router@1.0", "cron@1.0", "node-process@1.0", "p4@1.0", "simple-pay@1.0", "faff@1.0", "ans104@1.0", "test-device@1.0", "lookup@1.0", "local-name@1.0", "upload@1.0", "hook@1.0", "auth-hook@1.0", "http-auth@1.0", "greenzone@1.0", "apply@1.0", "dedup@1.0", "cookie@1.0", "push@1.0", "query@1.0", "manifest@1.0", "name@1.0", "profile@1.0", "monitor@1.0", "multipass@1.0", "poda@1.0", "snp@1.0", "trie@1.0", "volume@1.0", "secret@1.0", "wasi@1.0", "wasm-64@1.0", "whois@1.0", "cacheviz@1.0", "hyperbuddy@1.0", "copycat@1.0", "json-iface@1.0", "arweave@2.9", "b32-name@1.0", "blacklist@1.0", "bundler@1.0", "gzip@1.0", "location@1.0", "metering@1.0", "rate-limit@1.0", "tx@1.0"]), `

    // Pre-create prometheus ETS tables owned by the shell process.
    // dev_hbsig on_load also does this, but the module loads lazily so this
    // covers the gap between rebar3 boot and module loading.
    const initPrometheus = `lists:foreach(fun({N,{T,C}}) -> case ets:info(N) of undefined -> ets:new(N,[T,named_table,public,{C,true}]); _ -> ok end; ({N,C}) -> case ets:info(N) of undefined -> ets:new(N,[set,named_table,public,{C,true}]); _ -> ok end end, [{prometheus_registry_table,{bag,read_concurrency}},{prometheus_counter_table,write_concurrency},{prometheus_gauge_table,write_concurrency},{prometheus_summary_table,write_concurrency},{prometheus_quantile_summary_table,write_concurrency},{prometheus_histogram_table,write_concurrency},{prometheus_boolean_table,write_concurrency}]), `

    // When running multiple HyperBEAM instances, rebar3 shell auto-starts the
    // hb app which binds port 8734 (default). The second instance fails to start
    // the hb app because port 8734 is already in use. This leaves hb_sup,
    // dev_scheduler_registry, and ar_timestamp uninitialized. We idempotently
    // ensure they are running before calling start_mainnet.
    const ensureInit = `(fun() -> try hb:init() catch _:_ -> ok end, case whereis(hb_sup) of undefined -> catch hb_sup:start_link(); _ -> ok end, catch dev_scheduler_registry:start(), catch ar_timestamp:start() end)(), `

    // The HTTPSig codec transforms 'authority' to '@authority' in signature params but
    // not in component lines, causing internal message verification to fail (RFC 9421).
    // This affects both node_processes definitions (which include 'authority' via
    // augment_definition) and push device re-scheduling (which signs outbox messages
    // with httpsig). Disable verification until upstream fixes the codec.
    const _verify_assignments = (this.p4_lua || this.genesis_wasm) ? `, <<"verify-assignments">> => false` : ""

    // Use hb_http_server:start_node directly instead of hb:start_mainnet.
    // start_mainnet always overwrites the store config with a single hb_store_fs,
    // which prevents hb_store_gateway from resolving Arweave TX IDs.
    // start_node preserves user-provided store via set_default_opts.
    const _priv_wallet = `, <<"priv-wallet">> => hb:wallet(<<"${wallet}">>)`
    // cache_control => <<"always">> ensures compute results/snapshots are cached.
    // process_snapshot_slots => 1 takes a snapshot every slot (not just every 60s).
    // process_async_cache => false writes snapshots synchronously before returning
    // the HTTP response, preventing race conditions where the next compute call
    // arrives before the snapshot is written.
    // Without these, hb_cache:write strips uncommitted keys (like device-stack)
    // from the cached process state, and subsequent computes fail with
    // {error, no_valid_device_stack} when loading from the corrupted cache.
    const _cache_control = `, <<"cache-control">> => <<"always">>, <<"process-snapshot-slots">> => 1, <<"process-async-cache">> => false`

    const _linkify =
      this.linkify_mode === undefined
        ? ""
        : `, <<"linkify-mode">> => ${this.linkify_mode === false ? "false" : (this.linkify_mode === true ? "true" : this.linkify_mode)}`
    const start = `${clearProxy}${initPrometheus}${preRegisterAtoms}${loadHbsig}${ensureInit}hb_http_server:start_node(#{ ${_port}${_gateway}${_priv_wallet}${_faff}${_bundler}${_bundler_ans104}${_on}${_p4_non_chargable}${_operator}${_spp}${_genesis_wasm_port}${_force_signed}${_devices}${_node_processes}${_cache_writers}${_relay_http_client}${_routes}${_store}${_verify_assignments}${_cache_control}, <<"prometheus">> => false${_linkify}}).`

    return start
  }

  kill() {
    // Kill CU server if we started it
    if (this.cuProc && this.cuProc.pid) {
      try {
        process.kill(-this.cuProc.pid, "SIGKILL")
      } catch (e) {
        // Process may already be dead
      }
    }
    // Kill main HyperBEAM shell process
    if (this._shell) {
      this._shell.kill("SIGKILL")
    }
    // Also kill any remaining beam.smp processes on our port
    spawnSync("pkill", ["-9", "-f", `beam.smp.*${this.port}`], { stdio: "ignore" })
  }
}
