import _Arweave from "arweave"
import { is, last, keys } from "ramda"
import { tags, buildTags, dirname } from "./utils.js"
const Arweave = _Arweave.default ?? _Arweave
import lf from "localforage"
import wdb from "./lua/weavedb-lite.js"

export default class ArMem {
  constructor({ MU_URL, CU_URL, SU_URL, GATEWAY_URL, scheduler, cache } = {}) {
    this.__type__ = "mem"
    this.isInit = false
    this.keyInit = false
    this.keys = {}
    this.db = {
      put: async (key, val) => {
        let _val = null
        if (typeof val === "object" && !is(Array, val)) {
          _val = {}
          for (let k in val) {
            if (typeof val[k] !== "function") {
              if (is(Array, val[k])) {
                let _arr = []
                for (let v of val[k]) {
                  if (
                    typeof v === "object" &&
                    !is(Array, v) &&
                    !(v instanceof Uint8Array) &&
                    !(v instanceof ArrayBuffer)
                  ) {
                    let __val = {}
                    for (let k2 in v) {
                      if (typeof v[k2] !== "function") __val[k2] = v[k2]
                    }
                    _arr.push(__val)
                  } else {
                    _arr.push(v)
                  }
                }
                _val[k] = _arr
              } else if (
                typeof val[k] === "object" &&
                !(val[k] instanceof Uint8Array) &&
                !(val[k] instanceof ArrayBuffer)
              ) {
                let __val = {}
                for (let k2 in val[k]) {
                  if (typeof val[k][k2] !== "function") __val[k2] = val[k][k2]
                }
                _val[k] = __val
              } else {
                _val[k] = val[k]
              }
            }
          }
        } else _val = val
        await lf.setItem(key, _val)
        this.keys[key] = true
        await lf.setItem("keys", keys(this.keys))
      },
      get: async key => {
        return await lf.getItem(key)
      },
      getKeys: async ({ start, end }) => {
        if (!this.keyInit) {
          const _keys = (await lf.getItem("keys")) ?? []
          this.keys = {}
          for (const v of _keys) this.keys[v] = true
        }
        let __keys = []
        for (const k in this.keys) {
          const sp = k.split(".")
          if (sp[0] === start) __keys.push(k)
        }
        return __keys
      },
    }
    this.arweave = Arweave.init()
    this.arweave.transactions.getTransactionAnchor = () => this.getAnchor()
    this.arweave.transactions.getPrice = () => 0
    this.scheduler = scheduler
    this.SU_URL = SU_URL
    this.initSync()
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
          if (key === v) this[v][field] = await this.db.get(v2)
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
    const __dirname = await dirname()
    let format = null
    let _wasm = await this.wasms[mod]
    let wasm = _wasm?.data
    if (!wasm) {
      if (_wasm?.file) {
        wasm = Buffer.from(wdb, "base64")
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
}
