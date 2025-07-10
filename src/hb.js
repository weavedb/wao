import { connect, createSigner } from "@permaweb/aoconnect"
import { isEmpty, last, isNotNil, mergeLeft } from "ramda"
import { toAddr, buildTags } from "./utils.js"
import { signer } from "./signer.js"
import { send as _send } from "./send.js"
import hyper_aos from "./lua/hyper-aos.js"
import aos_wamr from "./lua/aos_wamr.js"
import { from } from "./httpsig.js"

const seed = num => {
  const array = new Uint8Array(num)
  return crypto.getRandomValues(array).toString()
}

class HB {
  constructor({
    url = "http://localhost:10001",
    cu = "http://localhost:6363",
    jwk,
  } = {}) {
    this.cu = cu
    this.url = url
    this.dev = {}
    this.dev.hyperbuddy = {
      metrics: async (args = {}) => {
        return this.parseMetrics(
          await this.fetch(
            this.path({ dev: "hyperbuddy", path: "metrics", json: false }),
            false
          )
        )
      },
    }
    this.dev.json = {
      commit: async args => {
        return await this.post({ path: "/~json@1.0/commit", ...args })
      },
      verify: async args => {
        return await this.post({ path: "/~json@1.0/verify", ...args })
      },
      deserialize: async args => {
        return await this.post({ path: "/~json@1.0/deserialize", ...args })
      },
      serialize: async args => {
        return await this.post({ path: "/~json@1.0/serialize", ...args })
      },
    }
    this.dev.meta = {
      info: async (args = {}) => {
        let { method = "GET", json = true, key } = args
        if (method.toLowerCase() === "post") {
          return await this.post({ path: "/~meta@1.0/info", ...args })
        } else {
          return key
            ? await this.fetch(
                this.path({
                  dev: "meta",
                  path: `info/${key}`,
                  json: args.json ?? false,
                }),
                args.json ?? false
              )
            : await this.fetch(
                this.path({ dev: "meta", path: "info", json: json })
              )
        }
      },
      build: async () => {
        return await this.fetch(this.path({ dev: "meta", path: "build" }))
      },
    }
    if (jwk) this._init(jwk)
  }

  _init(jwk) {
    this.signer = createSigner(jwk, this.url)
    this.addr = toAddr(jwk.n)
    this.sign = signer({ signer: this.signer, url: this.url })

    const { request } = connect({
      MODE: "mainnet",
      URL: this.url,
      device: "",
      signer: this.signer,
    })

    this._request = request
  }

  async init(jwk) {
    this._init(jwk)
    try {
      this._info = await this.dev.meta.info({})
    } catch (e) {}
    return this
  }

  async send(msg) {
    return await _send(msg)
  }

  async getImage() {
    const wasm = Buffer.from(aos_wamr, "base64")
    const id = await this.cacheBinary(wasm, "application/wasm")
    this.image ??= id
    return id
  }

  async getLua() {
    const lua = Buffer.from(hyper_aos, "base64")
    const id = await this.cacheModule(lua, "application/lua")
    this.lua ??= id
    return id
  }
  async scheduleAOS({ pid, action = "Eval", tags = {}, data }) {
    pid ??= this.pid
    let _tags = mergeLeft(tags, {
      device: "process@1.0",
      method: "POST",
      path: `/${pid}~process@1.0/schedule`,
      scheduler: this.scheduler,
      Type: "Message",
      Action: action,
      Target: pid,
    })
    if (data) _tags.data = data
    let res = await this.post(_tags)
    const slot = res.headers.slot
    return { slot, res, pid }
  }
  async messageAOS(args) {
    const { slot, pid } = await this.scheduleAOS(args)
    return { slot, outbox: await this.computeAOS({ pid, slot }) }
  }

  path({
    dev = "message",
    path,
    json = true,
    params = {},
    pid = "",
    tail = "",
  }) {
    if (!/@/.test(dev)) dev += "@1.0"
    let _params = ""
    if (!isEmpty(params)) {
      let i = 0
      for (const k in params) {
        _params += `${i === 0 ? "?" : "&"}${k}=${params[k]}`
        i++
      }
    }
    if (path && !/^\//.test(path)) path = "/" + path
    return `/${pid}~${dev}${path ?? ""}${tail}${json ? "/~json@1.0/serialize" : ""}${_params}`
  }

  async text(dev, path, params = {}, tail) {
    let pid = ""
    if (/^[a-zA-Z0-9_-]{43}$/.test(dev)) {
      pid = dev
      dev = "process"
    }
    return await this.fetch(
      this.path({ dev, path, json: false, params, pid, tail }),
      false
    )
  }

  async json(dev, path, params = {}, tail) {
    let pid = ""
    if (/^[a-zA-Z0-9_-]{43}$/.test(dev)) {
      pid = dev
      dev = "process"
    }
    return await this.fetch(
      this.path({ dev, path, json: true, params, pid, tail })
    )
  }

  async fetch(path, json = true) {
    return await fetch(this.url + path).then(r => (json ? r.json() : r.text()))
  }

  async computeAOS({ pid, slot }) {
    return await this.getJSON({ path: `/${pid}/compute/results/outbox`, slot })
  }

  async computeLua({ pid, slot }) {
    return await this.getJSON({ path: `/${pid}/compute/results`, slot })
  }

  async compute({ pid, slot, path = "" }) {
    if (path && !/^\//.test(path)) path = "/" + path
    return await this.getJSON({ path: `/${pid}/compute${path}`, slot })
  }

  async computeLegacy({ pid, slot }) {
    const json = await this.compute({ pid, slot })
    return JSON.parse(json.results.json.body)
  }

  async spawn(tags = {}) {
    const addr = await this.dev.meta.info({ key: "address" })
    this.scheduler ??= addr
    const _tags = mergeLeft(tags, {
      device: "process@1.0",
      path: "/schedule",
      scheduler: this.scheduler,
      "random-seed": seed(16),
      Type: "Process",
      "execution-device": "test-device@1.0",
    })
    const res = await this.post(_tags)
    return { res, pid: res.headers.process }
  }

  async cacheModule(data, type) {
    if (!this.cache) {
      const { pid } = await this.spawn({})
      this.cache = pid
    }
    const { slot } = await this.schedule({
      data,
      pid: this.cache,
      "content-type": type,
    })
    const msgs = await this.messages({ pid: this.cache, from: slot, limit: 1 })
    return msgs.edges[0].node.message.Id
  }

  async cacheBinary(data, type) {
    const res = await this.post({ path: "/~wao@1.0/cache_module", data, type })
    return res.headers.id
  }
  async message(args) {
    const pid = args.pid
    const { slot } = await this.schedule(args)
    const res = await this.compute({ pid, slot })
    return { slot, pid, res }
  }
  async scheduleLegacy({
    pid,
    action = "Eval",
    tags = {},
    data,
    scheduler,
  } = {}) {
    if (action) tags.Action = action
    return await this.schedule({ pid, tags, data, scheduler })
  }
  async scheduleLua(...args) {
    return await this.scheduleLegacy(...args)
  }
  async schedule({ pid, tags = {}, data } = {}) {
    pid ??= this.pid
    let _tags = mergeLeft(tags, {
      method: "POST",
      path: `/${pid}/schedule`,
      Type: "Message",
      Target: pid,
    })
    if (data) _tags.data = data
    let res = await this.post(_tags)
    return { slot: res.headers.slot, res }
  }

  async spawnAOS(image) {
    const addr = await this.dev.meta.info({ key: "address" })
    this.scheduler ??= addr
    image ??= this.image ?? (await this.getImage())
    const res = await this.post({
      device: "process@1.0",
      path: "/schedule",
      scheduler: this.scheduler,
      "Data-Protocol": "ao",
      Variant: "ao.N.1",
      "scheduler-location": this.scheduler,
      Authority: this.scheduler,
      "random-seed": seed(16),
      Type: "Process",
      image,
      "execution-device": "stack@1.0",
      "push-device": "push@1.0",
      "device-stack": [
        "wasi@1.0",
        "json-iface@1.0",
        "wasm-64@1.0",
        "patch@1.0",
        "multipass@1.0",
      ],
      "output-prefix": "wasm",
      "patch-from": "/results/outbox",
      "patch-mode": "patches",
      "stack-keys": ["init", "compute", "snapshot", "normalize"],
      passes: 2,
    })
    const pid = res.headers.process
    this.pid ??= pid
    return { pid, res }
  }

  async spawnLua(lua) {
    const addr = await this.dev.meta.info({ key: "address" })
    this.scheduler ??= addr
    lua ??= this.lua ?? (await this.getLua())
    const res = await this.post({
      device: "process@1.0",
      path: "/schedule",
      scheduler: this.scheduler,
      "Data-Protocol": "ao",
      Variant: "ao.N.1",
      "scheduler-location": this.scheduler,
      Authority: this.scheduler,
      "random-seed": seed(16),
      Type: "Process",
      module: lua,
      "execution-device": "lua@5.3a",
      "push-device": "push@1.0",
      "patch-from": "/results/outbox",
    })
    const pid = res.headers.process
    this.pid ??= pid
    return { pid, res }
  }

  parseMetrics(txt) {
    const parts = txt.split(/\r?\n/)
    let index = 0
    let _metrics = {}
    let vals = []
    let desc = []
    for (const v of parts) {
      if (/^# /.test(v)) {
        const [_, type, name, ...rest] = v.split(" ")
        if (!_metrics[name]) _metrics[name] = { index: ++index, values: [] }
        _metrics[name][type] = rest.join(" ")
      } else if (v !== "") {
        if (/{/.test(v)) {
          const [name, rest] = v.split("{")
          if (!_metrics[name]) _metrics[name] = { index: ++index, values: [] }
          const [params, val] = rest.split("}")
          let _params = {}
          for (const v of params.split(",")) {
            const [key, val] = v.trim().split("=")
            _params[key] = val.replace(/"/g, "")
          }
          _metrics[name].values.push({ value: val.trim(), params: _params })
        } else {
          const [name, val] = v.split(" ")
          if (!_metrics[name]) _metrics[name] = { index: ++index, values: [] }
          _metrics[name].values.push({ value: val })
        }
      }
    }
    return _metrics
  }
  async now({ pid, path = "" }) {
    if (path && !/^\//.test(path)) path = "/" + path
    return await this.getJSON({ path: `/${pid}/now${path}` })
  }
  async slot({ pid, path = "" }) {
    if (path && !/^\//.test(path)) path = "/" + path
    return await this.getJSON({ path: `/${pid}/slot${path}` })
  }

  async messages({ pid, from, to, limit } = {}) {
    let params = `target=${pid}`
    if (isNotNil(from)) params += `&from=${from}`
    if (isNotNil(to)) params += `&to=${to}`
    params += `&accept=application/aos-2`
    let res = await fetch(`${this.url}/~scheduler@1.0/schedule?${params}`).then(
      r => r.json()
    )
    if (res.page_info.has_next_page) {
      res.next = async () => {
        const from2 = last(res.edges).cursor + 1
        return await this.message({ pid, from: from2, to, limit })
      }
    }
    return res
  }

  async spawnLegacy({ module, tags = {}, data } = {}) {
    let t = {
      Type: "Process",
      "Data-Protocol": "ao",
      Variant: "ao.TN.1",
      scheduler: this._info.address,
      Authority: this._info.address,
      "scheduler-location": this._info.address,
      "random-seed": seed(16),
      module: module ?? "ISShJH1ij-hPPt9St5UFFr_8Ys3Kj5cyg7zrMGt7H9s",
      device: "process@1.0",
      "scheduler-device": "scheduler@1.0",
      "execution-device": "stack@1.0",
      "push-device": "push@1.0",
      "device-stack": ["genesis-wasm@1.0", "patch@1.0"],
      "patch-from": "/results/outbox",
      "stack-keys": ["init", "compute", "snapshot", "normalize"],
    }
    if (data) t.data = data
    tags = mergeLeft(tags, t)
    return await this.spawn(tags)
  }
  async results({ process, limit, sort = "DESC", from, to } = {}) {
    let params = ""
    const addParam = (key, val) => {
      params += params === "" ? "?" : "&"
      params += `${key}=${val}`
    }
    if (limit) addParam("limit", limit)
    if (sort) addParam("sort", sort)
    if (from) addParam("from", from)
    if (to) addParam("to", to)
    const res = await this.post({
      path: "/~relay@1.0/call",
      method: "GET",
      "relay-path": `${this.cu}/results/${process}${params}`,
      "content-type": "application/json",
    })
    return JSON.parse(res.body)
  }
  async dryrun({ tags = {}, pid, action, data } = {}) {
    if (typeof action === "string") tags.Action = action
    let json = { Tags: buildTags({ ...tags }), Owner: this.addr }
    if (data) json.Data = data
    const res = await this.post({
      path: "/~relay@1.0/call",
      method: "POST",
      "relay-path": `${this.cu}/dry-run?process-id=${pid}`,
      "content-type": "application/json",
      "relay-body": JSON.stringify(json),
    })
    return JSON.parse(res.body)
  }

  async post(obj, json) {
    const _json = json ? "/~json@1.0/serialize" : ""
    obj.path += _json
    return await this.send(await this.sign(obj))
  }
  async get({ path, ...params }, json = false) {
    const _json = json ? "/~json@1.0/serialize" : ""
    path ??= "/~message@1.0"
    if (!/^\//.test(path)) path = "/" + path
    let _params = ""
    if (!isEmpty(params)) {
      let i = 0
      for (const k in params) {
        _params += `${i === 0 ? "?" : "&"}${k}=${params[k]}`
        i++
      }
    }
    const response = await fetch(`${this.url}${path}${_json}${_params}`)
    let headers = {}
    response.headers.forEach((v, k) => (headers[k] = v))
    const http = {
      headers,
      body: await response.text(),
      status: response.status,
    }
    return {
      out: from(http),
      ...http,
    }
  }
  async postJSON(args) {
    const res = await this.post(args, true)
    return JSON.parse(res.body)
  }
  async getJSON(args) {
    const res = await this.get(args, true)
    return JSON.parse(res.body)
  }
}

export default HB
