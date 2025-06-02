import { connect, createSigner } from "@permaweb/aoconnect"
import { isEmpty, last, isNotNil, mergeLeft } from "ramda"
import { toAddr, buildTags } from "./utils.js"
import { send as _send, verify, createRequest } from "./signer.js"

const seed = num => {
  const array = new Uint8Array(num)
  return crypto.getRandomValues(array)
}

class HB {
  constructor({ url = "http://localhost:10001", jwk } = {}) {
    this.url = url
    this.signer = createSigner(jwk, this.url)
    this.addr = toAddr(jwk.n)
    this._request2 = createRequest({ signer: this.signer, url: this.url })
    const { request } = connect({
      MODE: "mainnet",
      URL: this.url,
      device: "",
      signer: this.signer,
    })
    this._request = request
    this.hyperbuddy = {
      metrics: async (args = {}) => {
        return this.parseMetrics(
          await this.fetch(this.path("hyperbuddy", "metrics", false), false)
        )
      },
    }
    this.json = {
      commit: async args => {
        return await this.send({ path: "/~json@1.0/commit", ...args })
      },
      verify: async args => {
        return await this.send({ path: "/~json@1.0/verify", ...args })
      },
      deserialize: async args => {
        return await this.send({ path: "/~json@1.0/deserialize", ...args })
      },
      serialize: async args => {
        return await this.send({ path: "/~json@1.0/serialize", ...args })
      },
    }
    this.meta = {
      info: async (args = {}) => {
        let { method = "GET", json = true, key } = args
        if (method.toLowerCase() === "post") {
          return await this.send({ path: "/~meta@1.0/info", ...args })
        } else {
          return key
            ? await this.fetch(
                this.path("meta", `info/${key}`, args.json ?? false),
                args.json ?? false
              )
            : await this.fetch(this.path("meta", "info", json))
        }
      },
      build: async () => {
        return await this.fetch(this.path("meta", "build"))
      },
    }
  }

  async send(args) {
    return await _send(await this._request2(args))
  }

  async getAddr() {
    return (await this.send({ path: "/~meta@1.0/info/address", method: "GET" }))
      .body
  }

  async getImage() {
    const result = await this.send({
      path: "/~wao@1.0/cache_wasm_image",
      method: "POST",
      filename: "test/aos-2-pure-xs.wasm",
    })
    return result.headers.get("image")
  }

  async messageAOS(action = "Eval", tags = {}, data) {
    let _tags = mergeLeft(tags, {
      device: "process@1.0",
      method: "POST",
      path: `/${this.pid}/schedule`,
      scheduler: this.scheduler,
      Type: "Message",
      Action: action,
      Target: this.pid,
    })
    if (data) _tags.data = data
    let res = await this.send(_tags)
    const slot = res.headers.get("slot")
    return { slot, outbox: await this.computeAOS(this.pid, slot) }
  }

  path(dev = "meta", path = "info", json = true, params = {}) {
    if (!/@/.test(dev)) dev += "@1.0"
    let _params = ""
    if (!isEmpty(params)) {
      let i = 0
      for (const k in params) {
        _params += `${i === 0 ? "?" : "&"}${k}=${params[k]}`
        i++
      }
    }
    return `${this.url}/~${dev}/${path}${json ? "/serialize~json@1.0" : ""}${_params}`
  }

  async fetch(url, json = true) {
    return await fetch(url).then(r => (json ? r.json() : r.text()))
  }

  async computeAOS(pid, slot) {
    return await fetch(
      `${this.url}/${pid}/compute/results/outbox/serialize~json@1.0?slot=${slot}`
    ).then(r => r.json())
  }

  async spawnAOS(image) {
    const addr = await this.getAddr()
    this.scheduler ??= addr
    image ??= this.image ?? (await this.getImage())
    const res = await this.send({
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
      "scheduler-device": "scheduler@1.0",
      "execution-device": "stack@1.0",
      "device-stack": [
        "wasi@1.0",
        "json-iface@1.0",
        "wasm-64@1.0",
        "multipass@1.0",
      ],
      "output-prefix": "wasm",
      "patch-from": "/results/outbox",
      "stack-keys": ["init", "compute", "snapshot", "normalize"],
      passes: 2,
    })
    const pid = res.headers.get("process")
    this.pid ??= pid
    return pid
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

  async messages({ target, from, to, limit } = {}) {
    let params = `target=${target}`
    if (isNotNil(from)) params += `&from=${from}`
    if (isNotNil(to)) params += `&to=${to}`
    params += `&accept=application/aos-2`
    let res = await fetch(`${this.url}/~scheduler@1.0/schedule?${params}`).then(
      r => r.json()
    )
    if (res.page_info.has_next_page) {
      res.next = async () => {
        const from2 = last(res.edges).cursor + 1
        return await this.message({ target, from: from2, to, limit })
      }
    }
    return res
  }

  async process({ tags = {}, data } = {}) {
    tags = mergeLeft(tags, {
      data,
      Type: "Process",
      "Data-Protocol": "ao",
      Variant: "ao.TN.1",
      scheduler: this._info.address,
      authority: this._info.address,
      "scheduler-location": this.info.address,
      "random-seed": seed(16),
      module: "JArYBF-D8q2OmZ4Mok00sD2Y_6SYEQ7Hjx-6VZ_jl3g",
      device: "process@1.0",
      "scheduler-device": "scheduler@1.0",
      "execution-device": "genesis-wasm@1.0",
    })
    const res = await this.post({ tags })
    return await res.process.text()
  }

  async schedule({ tags = {}, data, process, action = "Eval" } = {}) {
    tags = mergeLeft(tags, {
      path: `${process}/schedule`,
      type: "Message",
      action,
      data,
      "Data-Protocol": "ao",
      Variant: "ao.N.1",
    })
    return (await this.post({ tags })).slot.text()
  }

  async compute({ tags = {}, process, slot } = {}) {
    return await this.request({
      method: "GET",
      path: `/${process}/compute&slot+integer=${slot}/results/json`,
    })
  }

  async dryrun({ tags = {}, process, action, data } = {}) {
    if (typeof action === "string") tags.Action = action
    let json = { Tags: buildTags(tags) }
    if (data) json.Data = data
    return await fetch(
      `${this.url}/~relay@1.0/call?relay-method=POST&relay-path=/dry-run?process-id=${process}/&content-type=application/json&body=${JSON.stringify(json)}`
    ).then(r => r.json())
  }

  async get({ device, path = "~meta@1.0/info" }) {
    return (await this.request({ device, path, method: "GET" })).body
  }

  async post({ device, tags, path = "/schedule" }) {
    return await this.request({ tags, device, path, method: "POST" })
  }

  async request({ device, tags, method = "POST", path = "/schedule" }) {
    let _tags = mergeLeft(tags, { path, method })
    if (device) _tags.device = device
    return await this._request(_tags)
  }
}

export default HB
