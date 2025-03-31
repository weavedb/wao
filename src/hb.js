import { connect, createSigner } from "aoconnect-wao"
import { mergeLeft } from "ramda"
import { randomBytes } from "node:crypto"
import { buildTags } from "./utils.js"
class HB {
  constructor({ url = "http://localhost:10000" } = {}) {
    this.url = url
  }

  async metrics() {
    const txt = await fetch(`${this.url}/metrics`).then(r => r.text())
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

  async info() {
    const txt = await fetch(
      `${this.url}/~meta@1.0/info/serialize~json@1.0`
    ).then(r => r.json())
    return txt
  }

  async init(jwk) {
    this._info = await this.info()
    const { request } = connect({
      MODE: "mainnet",
      URL: this.url,
      device: "",
      signer: createSigner(jwk),
    })
    this._request = request
    return this
  }

  async process({ tags = {}, data = "1984" } = {}) {
    tags = mergeLeft(tags, {
      data,
      Type: "Process",
      "Data-Protocol": "ao",
      Variant: "ao.N.1",
      scheduler: this._info.address,
      authority: this._info.address,
      "scheduler-location": this.info.address,
      "random-seed": randomBytes(16).toString("hex"),
      module: "JArYBF-D8q2OmZ4Mok00sD2Y_6SYEQ7Hjx-6VZ_jl3g",
      device: "process@1.0",
      "scheduler-device": "scheduler@1.0",
      "execution-device": "genesis-wasm@1.0",
    })
    return await this.post({ tags })
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

  async dryrun({ tags = {}, process, action } = {}) {
    if (typeof action === "string") tags.Action = action
    return await fetch(
      `${this.url}/~relay@1.0/call?relay-method=POST&relay-path=/dry-run?process-id=${process}/&content-type=application/json&body=${JSON.stringify({ Tags: buildTags(tags) })}`
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
