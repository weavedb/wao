import { createSigner } from "@permaweb/aoconnect"
import { isEmpty, last, isNotNil, mergeLeft, clone } from "ramda"
import { toAddr, buildTags, seed } from "./utils.js"
import { rsaid, hmacid, sign, signer, send as _send, commit } from "hbsig"
import hyper_aos from "./lua/hyper-aos.js"
import aos_wamr from "./lua/aos_wamr.js"
import { from } from "./httpsig-utils.js"

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
  }

  async setInfo() {
    if (!this.operator) {
      try {
        this.operator = await this.g("/~meta@1.0/info/address")
      } catch (e) {
        console.log(e)
      }
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

  async messageAOS(args) {
    const { slot, pid } = await this.scheduleAOS(args)
    return { slot, outbox: await this.computeAOS({ pid, slot }) }
  }

  async messageLegacy(args) {
    const { slot, pid } = await this.scheduleLegacy(args)
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

  async cacheScript(data, type = "application/lua") {
    if (!this.cache) {
      const { pid } = await this.spawn({})
      this.cache = pid
    }
    const { slot } = await this.scheduleFlat({
      data,
      pid: this.cache,
      tags: { "content-type": type },
    })
    const msgs = await this.messages({ pid: this.cache, from: slot, limit: 1 })
    return msgs.edges[0].node.message.Id
  }

  async cacheBinary(data, type) {
    const res = await this.post({ path: "/~wao@1.0/cache_module", data, type })
    return res.out.id
  }

  async message(args) {
    const pid = args.pid
    const { slot } = await this.schedule(args)
    const res = await this.compute({ pid, slot })
    return { slot, pid, res }
  }

  async scheduleFlat({ pid, tags = {}, data } = {}) {
    let _tags = mergeLeft(tags, { type: "Message", target: pid })
    if (data) _tags.data = data
    let res = await this.post({ path: "/~process@1.0/schedule", body: _tags })
    return { slot: res.out.slot, res, pid }
  }

  async scheduleNP({ pid, tags = {}, data } = {}) {
    if (data) tags.data = data
    let res = await this.post({
      path: `/${pid}~node-process@1.0/schedule`,
      body: await this.commit(tags),
    })
    return { slot: res.out.slot, res, pid }
  }
  async schedule({ pid, tags = {}, data } = {}) {
    let _tags = mergeLeft(tags, { type: "Message", target: pid })
    if (data) _tags.data = data
    let body = await this.commit(_tags, { path: false })
    let signed = await this.sign({ path: `/${pid}/schedule`, body })
    let res = await this.send(signed)
    return { slot: res.out.slot, res, pid }
  }

  async scheduleLua({ action = "Eval", tags = {}, ...rest }) {
    if (action) tags.Action = action
    return await this.schedule({ tags, ...rest })
  }

  async spawnLua(lua) {
    await this.setInfo()
    const tags = {
      "data-protocol": "ao",
      variant: "ao.N.1",
      authority: this.operator,
      module: this.lua ?? (await this.getLua()),
      "execution-device": "lua@5.3a",
      "push-device": "push@1.0",
      "patch-from": "/results/outbox",
    }
    return this.spawn(tags)
  }

  async now({ pid, path = "" }) {
    if (path && !/^\//.test(path)) path = "/" + path
    return await this.getJSON({ path: `/${pid}/now${path}` })
  }

  async slot({ pid, path = "" }) {
    if (path && !/^\//.test(path)) path = "/" + path
    return await this.getJSON({ path: `/${pid}/slot${path}` })
  }

  async messages({ pid, from, to } = {}) {
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
        return await this.messages({ pid, from: from2, to })
      }
    }
    return res
  }
  async spawn(tags = {}) {
    await this.setInfo()
    const res = await this.post({
      path: "/~process@1.0/schedule",
      body: await this.commit(
        mergeLeft(tags, {
          "random-seed": seed(16),
          type: "Process",
          "execution-device": "test-device@1.0",
          device: "process@1.0",
          scheduler: this.operator,
        }),
        { path: false }
      ),
      scheduler: this.operator,
    })
    return { res, pid: res.out.process }
  }
  async spawnLegacy({ module, tags = {}, data } = {}) {
    await this.setInfo()
    let t = mergeLeft(tags, {
      "data-protocol": "ao",
      variant: "ao.TN.1",
      authority: this.operator,
      module: module ?? "ISShJH1ij-hPPt9St5UFFr_8Ys3Kj5cyg7zrMGt7H9s",
      device: "process@1.0",
      "execution-device": "stack@1.0",
      "push-device": "push@1.0",
      "device-stack": ["genesis-wasm@1.0", "patch@1.0"],
      "patch-from": "/results/outbox",
    })
    if (data) t.data = data
    return await this.spawn(t)
  }
  async scheduleLegacy({ action = "Eval", tags = {}, ...rest } = {}) {
    if (action) tags.Action = action
    return await this.schedule({ tags, ...rest })
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
    return await commit(obj, { ...opts, signer: this.sign })
  }

  async p(path, ...args) {
    let _args = clone(args)
    _args[0] ??= {}
    _args[0].path ??= path
    return (await this.post(..._args))?.out ?? null
  }

  async post(obj, opt = {}) {
    const _json = opt.json ? "/~json@1.0/serialize" : ""
    obj.path += _json
    const signed = await this.sign(obj, opt)
    return await this.send(signed)
  }

  async g(path, ...args) {
    let _args = clone(args)
    _args[0] ??= {}
    _args[0].path ??= path
    return (await this.get(..._args))?.out ?? null
  }

  async get({ path, ...params }, opt = {}) {
    const _json = opt.json ? "/~json@1.0/serialize" : ""
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
    console.log(`${this.url}${path}${_json}${_params}`)
    const response = await fetch(`${this.url}${path}${_json}${_params}`)
    console.log(response)
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

  async postJSON(args, opt = {}) {
    const res = await this.post(args, { ...opt, json: true })
    return JSON.parse(res.body)
  }

  async getJSON(args, opt = {}) {
    const res = await this.get(args, { ...opt, json: true })
    return JSON.parse(res.body)
  }
  async spawnAOS(image) {
    await this.setInfo()
    image ??= this.image ?? (await this.getImage())
    const tags = {
      "data-protocol": "ao",
      variant: "ao.N.1",
      authority: this.operator,
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
      passes: 2,
    }
    return await this.spawn(tags)
  }

  async scheduleAOS({ action = "Eval", tags = {}, ...rest }) {
    if (action) tags.Action = action
    return await this.schedule({ tags, ...rest })
  }
}

export default HB
