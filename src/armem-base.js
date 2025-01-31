import _Arweave from "arweave"
const Arweave = _Arweave.default ?? _Arweave

import { last } from "ramda"
import { buildTags, tags } from "./utils.js"

export default class ArMemBase {
  constructor({ MU_URL, CU_URL, SU_URL, GATEWAY_URL, scheduler, cache } = {}) {
    this.__type__ = "mem"
    this.isInit = false
    this.keyInit = false
    this.keys = {}
    this.arweave = Arweave.init()
    this.arweave.transactions.getTransactionAnchor = () => this.getAnchor()
    this.arweave.transactions.getPrice = () => 0
    this.scheduler = scheduler
    this.SU_URL = SU_URL
  }
  getAnchor() {
    return this.blocks.length === 0
      ? "Do_Uc2Sju_ffp6Ev0AnLVdPtot15rvMjP-a9VVaA5fM"
      : last(this.blockmap[last(this.blocks)].txs)
  }
  async get(key, field) {
    await this.init()
    if (!field) return this[key]
    return this[key]?.[field]
  }
  async set(val, key, field) {
    await this.init()
    if (!field) {
      this[key] = val
      if (this.db) await this.db.put(`${key}`, this[key])
    } else {
      this[key] ??= {}
      this[key][field] = val
      if (this.db) await this.db.put(`${key}.${field}`, this[key][field])
    }
  }
  initSync() {
    this.addrmap = {}
    this.txs = {}
    this.jwks = {}
    this.blocks = []
    this.blockmap = {}
    this.env = {}
    this.modules = {
      aos2_0_1: "Do_Uc2Sju_ffp6Ev0AnLVdPtot15rvMjP-a9VVaA5fM",
      aos1: "cNlipBptaF9JeFAf4wUmpi43EojNanIBos3EfNrEOWo",
      sqlite: "ghSkge2sIUD_F00ym5sEimC63BDBuBrq4b5OcwxOjiw",
    }
    this.modmap = {}
    this.msgs = {}
    this.wasms = {
      "Do_Uc2Sju_ffp6Ev0AnLVdPtot15rvMjP-a9VVaA5fM": {
        file: "aos2_0_1",
        format: "wasm64-unknown-emscripten-draft_2024_02_15",
      },
      cNlipBptaF9JeFAf4wUmpi43EojNanIBos3EfNrEOWo: {
        file: "aos_1",
        format: "wasm64-unknown-emscripten-draft_2024_02_15",
      },
      ghSkge2sIUD_F00ym5sEimC63BDBuBrq4b5OcwxOjiw: {
        file: "sqlite",
        format: "wasm64-unknown-emscripten-draft_2024_02_15",
      },
    }
    let txs = []
    for (const k in this.modules) {
      const key = this.modules[k]
      txs.push(key)
      this.txs[key] = {
        id: key,
        block: 0,
        tags: buildTags(null, {
          "Data-Protocol": "ao",
          Variant: "ao.TN.1",
          Type: "Module",
          "Module-Format": "wasm64-unknown-emscripten-draft_2024_02_15",
          "Input-Encoding": "JSON-V1",
          "Output-Encoding": "JSON-V1",
          "Memory-Limit": "1-gb",
          "Compute-Limit": "9000000000000",
          Extension: "WeaveDrive",
        }),
      }
    }
    if (this.scheduler && this.SU_URL) {
      const key = this.scheduler
      txs.push(key)
      this.addrmap[this.scheduler] = { address: this.scheduler }
      this.txs[key] = {
        id: key,
        block: 0,
        owner: this.scheduler,
        tags: buildTags(null, {
          "Data-Protocol": "ao",
          Variant: "ao.TN.1",
          Type: "Scheduler-Location",
          Url: this.SU_URL,
          "Time-To-Live": 1000 * 60 * 60 * 24 * 365 * 10,
        }),
      }
    }
    this.blockmap["0"] = {
      txs,
      timestamp: Date.now(),
      height: 0,
      previous: "",
      id: "0",
    }
    this.blocks.push("0")
    this.height = 1
  }
  async putAll(key) {
    for (let k in this[key]) await this.set(this[key][k], key, k)
  }
  async init() {
    if (this.isInit) return
    this.isInit = true
    if (this.db) {
      for (const v of ["height", "blocks"]) this[v] = await this.get(v)
      for (const v of [
        "txs",
        "jwks",
        "env",
        "modules",
        "wasms",
        "addrmap",
        "blockmap",
        "modmap",
        "msgs",
      ]) {
        this[v] ??= {}
        const items = await this.db.getKeys({ start: v, end: v + "a" })
        for (const v2 of items || []) {
          const key = v2.split(".")[0]
          const field = v2.split(".")[1]
          if (key === v) {
            this[v][field] = await this.db.get(v2)
            if (v === "env") {
              console.log(this[v][field])
            }
          }
        }
      }
    } else {
      for (const v of ["height", "blocks"]) await this.set(this[v], v)
      for (const v of ["modules", "wasms", "addrmap", "blockmap"]) {
        await this.putAll(v)
      }
    }
  }
  async getWasm(module) {
    let mod = module ?? this.modules.aos2_0_1
    if (!mod) throw Error("module not found")
    let format = null
    let _wasm = await this.wasms[mod]
    let wasm = _wasm?.data
    if (!wasm) {
      if (_wasm?.file) {
        wasm = await this._getWasm(this.wasms[mod].file)
        format = _wasm.format
      } else {
        const tx = await this.get("txs", mod)
        if (tx) {
          wasm = Buffer.from(tx.data, "base64")
          format = tags(tx.tags)["Module-Format"]
        }
      }
    } else {
      format = _wasm.format
    }
    format ??= "wasm64-unknown-emscripten-draft_2024_02_15"
    return { format, mod, wasm }
  }
}
