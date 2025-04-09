import _Arweave from "arweave"
const Arweave = _Arweave.default ?? _Arweave
import { Bundle } from "arbundles"
import { compress, decompress } from "./compress.js"
import { last, assoc, is, isNil } from "ramda"
import { buildTags, tags } from "./utils.js"
import base64url from "base64url"

function eq(buf1, buf2, chunkSize = 1024 * 1024) {
  if (buf1.byteLength !== buf2.byteLength) return false

  const view1 = new Uint8Array(buf1)
  const view2 = new Uint8Array(buf2)

  for (let i = 0; i < buf1.byteLength; i += chunkSize) {
    const slice1 = view1.subarray(i, i + chunkSize)
    const slice2 = view2.subarray(i, i + chunkSize)

    if (!slice1.every((val, idx) => val === slice2[idx])) return false
  }

  return true
}

export default class ArMemBase {
  constructor({
    MU_URL,
    CU_URL,
    SU_URL,
    GATEWAY_URL,
    scheduler,
    cache,
    init,
    Waosm,
  } = {}) {
    this.__type__ = "mem"
    this._init = init
    this.Waosm = Waosm
    this.isInit = false
    this.keyInit = false
    this.keys = {}
    this.arweave = Arweave.init()
    this.arweave.transactions.getTransactionAnchor = () => this.getAnchor()
    this.arweave.transactions.getPrice = () => 0
    this.scheduler = scheduler
    this.SU_URL = SU_URL
  }
  compress(memory) {
    const waosm = new this.Waosm()
    return waosm.compress(memory)
  }
  decompress(memory, size) {
    const waosm = new this.Waosm()
    return waosm.decompress(memory, size)
  }

  async owner(di) {
    return base64url.encode(
      Buffer.from(await crypto.subtle.digest("SHA-256", di.rawOwner))
    )
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
      if (this.db) {
        if (key === "env") {
          let memory = val.memory
          try {
            memory = this.compress(val.memory)
          } catch (e) {
            console.log(e)
          }
          this[key][field].original_size = val.memory.length
          await this.db.put(
            `${key}.${field}`,
            assoc("memory", memory, this[key][field])
          )
        } else {
          await this.db.put(`${key}.${field}`, this[key][field])
        }
      }
    }
  }
  initSync() {
    this.items = {}
    this.addrmap = {}
    this.txs = {}
    this.blocks = []
    this.blockmap = {}
    this.env = {}
    this.modules = {}
    this.modmap = {}
    this.msgs = {}

    this.wasms = {
      "JArYBF-D8q2OmZ4Mok00sD2Y_6SYEQ7Hjx-6VZ_jl3g": { file: "aos2_0_3" },
      "Do_Uc2Sju_ffp6Ev0AnLVdPtot15rvMjP-a9VVaA5fM": { file: "aos2_0_1" },
      ghSkge2sIUD_F00ym5sEimC63BDBuBrq4b5OcwxOjiw: { file: "sqlite" },
    }
    for (const k in this.wasms) {
      this.wasms[k].format ??= "wasm64-unknown-emscripten-draft_2024_02_15"
      this.modules[this.wasms[k].file] = k
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
    if (typeof this._init === "function") await this._init()
    if (this.db) {
      for (const v of ["height", "blocks"]) this[v] = await this.get(v)
      for (const v of [
        "items",
        "txs",
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
            if (key.match(/^env/)) {
              let v3 = await this.db.get(v2)
              if (is(Uint8Array, v3.memory)) {
                v3.compressed = true
              }
              this[v][field] = v3
            } else {
              this[v][field] = await this.db.get(v2)
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
  async getTx(id) {
    let tx = this.txs[id]
    if (isNil(tx)) return null
    if (tx.bundle) {
      try {
        let bundle = new Bundle(this.txs[tx.bundle].data)
        for (let di of bundle.items) {
          if (id === di.id) {
            const data = di.data
            const data_size = Buffer.byteLength(di.rawData).toString()
            let data_type = ""
            for (const t of di.tags)
              if (t.name === "Content-Type") data_type = t.value
            const owner = await this.owner(di)
            tx = {
              _data: { size: data_size, type: data_type },
              anchor: di.anchor,
              signature: di.signature,
              recipient: di.target,
              id: await di.id,
              item: di,
              owner,
              tags: di.tags,
              data,
            }
          }
        }
      } catch (e) {}
    }
    return tx
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
        const tx = await this.getTx(mod)
        if (tx) {
          wasm = Buffer.from(tx.data, "base64")
          format = tags(tx.tags)["Module-Format"]
        }
      }
    } else format = _wasm.format
    format ??= "wasm64-unknown-emscripten-draft_2024_02_15"
    return { format, mod, wasm }
  }
}
