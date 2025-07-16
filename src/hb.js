import { connect, createSigner } from "@permaweb/aoconnect"
import { isEmpty, last, isNotNil, mergeLeft } from "ramda"
import { rsaid, hmacid, toAddr, buildTags } from "./utils.js"
import { sign, signer } from "./signer.js"
import { send as _send } from "./send.js"
import hyper_aos from "./lua/hyper-aos.js"
import aos_wamr from "./lua/aos_wamr.js"
import { from } from "./httpsig.js"

const seed = num => {
  const array = new Array(num)
  for (let i = 0; i < num; i++) array[i] = Math.floor(Math.random() * 256)
  return Buffer.from(array).toString("base64")
}

class HB {
  constructor({
    url = "http://localhost:10001",
    cu = "http://localhost:6363",
    jwk,
  } = {}) {
    this.cu = cu
    this.url = url
    if (jwk) this._init(jwk)
  }
  async signEncoded(encoded) {
    const { path, ...msg } = encoded
    return await sign({
      jwk: this.jwk,
      msg,
      path,
      url: this.url,
    })
  }

  _init(jwk) {
    this.jwk = jwk
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

  async setInfo() {
    if (!this._info) {
      try {
        this._info = await this.g("/~meta@1.0/info")
      } catch (e) {}
    }
  }

  async init(jwk) {
    this._init(jwk)
    await this.setInfo()
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
    const id = await this.cacheScript(lua, "application/lua")
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
      type: "Message",
      Action: action,
      target: pid,
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

  async messageLegacy(args) {
    const { slot, pid } = await this.scheduleLegacy(args)
    console.log(slot, pid, args)
    return { slot, res: await this.computeLegacy({ pid, slot }) }
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
    const addr = await this.g("/~meta@1.0/info/address")
    this.scheduler ??= addr
    const _tags = mergeLeft(tags, {
      device: "process@1.0",
      path: "/schedule",
      scheduler: this.scheduler,
      "random-seed": seed(16),
      type: "Process",
      "execution-device": "test-device@1.0",
    })
    const res = await this.post(_tags)
    return { res, pid: res.headers.process }
  }

  async cacheScript(data, type = "application/lua") {
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
      type: "Message",
      target: pid,
    })
    if (data) _tags.data = data
    let res = await this.post(_tags)
    return { slot: res.headers.slot, res, pid }
  }

  async spawnAOS(image) {
    const addr = await this.g("/~meta@1.0/info/address")
    this.scheduler ??= addr
    image ??= this.image ?? (await this.getImage())
    const res = await this.post({
      device: "process@1.0",
      path: "/schedule",
      scheduler: this.scheduler,
      "data-protocol": "ao",
      variant: "ao.N.1",
      "scheduler-location": this.scheduler,
      authority: this.scheduler,
      "random-seed": seed(16),
      type: "Process",
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
    const addr = await this.g("/~meta@1.0/info/address")
    this.scheduler ??= addr
    lua ??= this.lua ?? (await this.getLua())
    const res = await this.post({
      device: "process@1.0",
      path: "/schedule",
      scheduler: this.scheduler,
      "data-protocol": "ao",
      variant: "ao.N.1",
      "scheduler-location": this.scheduler,
      authority: this.scheduler,
      "random-seed": seed(16),
      type: "Process",
      module: lua,
      "execution-device": "lua@5.3a",
      "push-device": "push@1.0",
      "patch-from": "/results/outbox",
    })
    const pid = res.headers.process
    this.pid ??= pid
    return { pid, res }
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
        return await this.messages({ pid, from: from2, to, limit })
      }
    }
    return res
  }

  async spawnLegacy({ module, tags = {}, data } = {}) {
    await this.setInfo()
    let t = {
      type: "Process",
      "data-protocol": "ao",
      variant: "ao.TN.1",
      scheduler: this._info.address,
      authority: this._info.address,
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

  async commit(obj, opts) {
    const msg = await this.sign(obj, opts)
    const hmacId = hmacid(msg.headers)
    const rsaId = rsaid(msg.headers)
    const committer = this.addr
    const meta = { alg: "rsa-pss-sha512", "commitment-device": "httpsig@1.0" }
    const meta2 = { alg: "hmac-sha256", "commitment-device": "httpsig@1.0" }
    const sigs = {
      signature: msg.headers.signature,
      "signature-input": msg.headers["signature-input"],
    }
    return {
      commitments: {
        [rsaId]: { ...meta, committer, ...sigs },
        [hmacId]: { ...meta2, ...sigs },
      },
      ...obj,
    }
  }

  async p(path, ...args) {
    args[0] ??= {}
    args[0].path ??= path
    return (await this.post(...args))?.out ?? null
  }

  async post(obj, json) {
    const _json = json ? "/~json@1.0/serialize" : ""
    obj.path += _json
    return await this.send(await this.sign(obj))
  }

  async g(path, ...args) {
    args[0] ??= {}
    args[0].path ??= path
    return (await this.get(...args))?.out ?? null
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
      ...from(http),
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
