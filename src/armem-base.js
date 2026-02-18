import _Arweave from "arweave"
const Arweave = _Arweave.default ?? _Arweave
import { Bundle } from "arbundles"
import { compress, decompress } from "./compress.js"
import { last, assoc, is, isNil } from "ramda"
import { buildTags, tags } from "./utils.js"
import base64url from "base64url"
import RemoteAR from "./ar-remote.js"

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
    variant,
    ar_url,
  } = {}) {
    this.variant = variant
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
    this._txCache = new Map()
    this._TX_CACHE_MAX = 200
    this.ar_url = ar_url || null
    this._remote = ar_url ? new RemoteAR(ar_url) : null
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
    if (this._remote) {
      return this._remoteAnchor ?? "Do_Uc2Sju_ffp6Ev0AnLVdPtot15rvMjP-a9VVaA5fM"
    }
    // D1 ready: use lastBlockId scalar (block.id === last tx.id)
    if (this._d1Ready) {
      return this.lastBlockId || "Do_Uc2Sju_ffp6Ev0AnLVdPtot15rvMjP-a9VVaA5fM"
    }
    // Fallback: use blocks array
    if (this.blocks.length === 0) {
      return "Do_Uc2Sju_ffp6Ev0AnLVdPtot15rvMjP-a9VVaA5fM"
    }
    const lastBlock = this.blockmap[last(this.blocks)]
    if (!lastBlock) return "Do_Uc2Sju_ffp6Ev0AnLVdPtot15rvMjP-a9VVaA5fM"
    return last(lastBlock.txs)
  }
  async get(key, field) {
    await this.init()
    if (!field) {
      // Remote AR fallback for height
      if (key === "height" && this._remote && !this[key]) {
        this[key] = await this._remote.getHeight()
      }
      return this[key]
    }
    // Lazy load from storage if not in memory
    if (this[key]?.[field] === undefined && this.db) {
      const val = await this.db.get(`${key}.${field}`)
      if (val !== null) {
        this[key] ??= {}
        if (key === "env" && val) {
          // Try loading memory from R2 first
          if (this.db.r2GetMemory && !val.memory) {
            try {
              const r2mem = await this.db.r2GetMemory(field)
              if (r2mem) {
                val.memory = r2mem
                val.compressed = true
              }
            } catch (e) {}
          }
          // Fallback: memory was stored inline in DO (pre-migration)
          if (val.memory && is(Uint8Array, val.memory)) {
            val.compressed = true
            // Lazy migrate: move inline memory to R2
            if (this.db.r2PutMemory) {
              try {
                await this.db.r2PutMemory(field, val.memory)
                // Remove memory from DO entry to save space
                const meta = { ...val }
                delete meta.memory
                meta._r2_memory = true
                await this.db.put(`${key}.${field}`, meta)
              } catch (e) {}
            }
          }
        }
        this[key][field] = val
      }
    }
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
          if (val.memory) {
            let memory = val.memory
            try {
              memory = this.compress(val.memory)
            } catch (e) {
              console.log(e)
            }
            this[key][field].original_size = val.memory.length

            // R2 path: store compressed memory in R2, metadata in DO
            if (this.db.r2PutMemory) {
              try {
                await this.db.r2PutMemory(field, memory)
                // Store metadata without memory in DO
                const meta = { ...this[key][field] }
                delete meta.memory
                meta._r2_memory = true
                await this.db.put(`${key}.${field}`, meta)
              } catch (e) {
                // Fallback to inline storage
                await this.db.put(
                  `${key}.${field}`,
                  assoc("memory", memory, this[key][field])
                )
              }
            } else {
              await this.db.put(
                `${key}.${field}`,
                assoc("memory", memory, this[key][field])
              )
            }
          } else {
            // Remote CU process — memory lives on satellite, store metadata only
            await this.db.put(`${key}.${field}`, this[key][field])
          }
        } else if (key === "txs" && this._d1Ready) {
          // D1 + R2 are authoritative for tx metadata, skip DO storage
          this._txCache.set(field, val)
        } else {
          await this.db.put(`${key}.${field}`, this[key][field])
        }
      }
    }
  }
  // Get all field keys for a given prefix (e.g. all process IDs under "env")
  async getFieldKeys(key) {
    await this.init()
    if (!this.db) return Object.keys(this[key] ?? {})
    const stored = await this.db.getKeys({ start: key + ".", end: key + "0" })
    const dbKeys = (stored || []).map(k => k.split(".")[1]).filter(Boolean)
    const memKeys = Object.keys(this[key] ?? {})
    return [...new Set([...memKeys, ...dbKeys])]
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
      "ISShJH1ij-hPPt9St5UFFr_8Ys3Kj5cyg7zrMGt7H9s": {
        file: "aos2_0_6",
        variant: "ao.TN.1",
      },
      "WASM32-D8q2OmZ4Mok00sD2Y_6SYEQ7Hjx-6VZ_jl3g": {
        file: "aos2_0_4_32",
        format: "wasm32-unknown-emscripten4",
      },
      "JArYBF-D8q2OmZ4Mok00sD2Y_6SYEQ7Hjx-6VZ_jl3g": { file: "aos2_0_3" },
      "Do_Uc2Sju_ffp6Ev0AnLVdPtot15rvMjP-a9VVaA5fM": { file: "aos2_0_1" },
      ghSkge2sIUD_F00ym5sEimC63BDBuBrq4b5OcwxOjiw: { file: "sqlite" },
    }
    for (const k in this.wasms) {
      this.wasms[k].format ??= "wasm64-unknown-emscripten-draft_2024_02_15"
      this.modules[this.wasms[k].file] = k
    }

    let txs = []
    for (const key in this.wasms) {
      const w = this.wasms[key]
      txs.push(key)
      this.txs[key] = {
        id: key,
        block: 0,
        owner: this.scheduler || "",
        tags: buildTags(null, {
          "Data-Protocol": "ao",
          Variant: w.variant ?? this.variant ?? "ao.TN.1",
          Type: "Module",
          "Module-Format":
            w.format ?? "wasm64-unknown-emscripten-draft_2024_02_15",
          "Input-Encoding": w.input_encoding ?? "JSON-V1",
          "Output-Encoding": w.output_encoding ?? "JSON-V1",
          "Memory-Limit": w.memory_limit ?? "1-gb",
          "Compute-Limit": w.compute_limit ?? "9000000000000",
          Extension: w.extension ?? "WeaveDrive",
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
          Variant: this.variant ?? "ao.TN.1",
          Type: "Scheduler-Location",
          Url: this.SU_URL,
          "Time-To-Live": 1000 * 60 * 60 * 24 * 365 * 10,
        }),
      }
    }
    this.blockmap["0"] = {
      txs,
      timestamp: Date.now(),
      height: 1,
      previous: "",
      id: "0",
    }
    this.blocks.push("0")
    this.lastBlockId = "0"
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
      let fresh = true
      // Probe D1 readiness: binding exists AND schema is applied
      this._d1Ready = false
      if (this.db.d1) {
        try {
          await this.db.d1.prepare("SELECT 1 FROM blocks LIMIT 0").all()
          this._d1Ready = true
        } catch {}
      }
      // Load scalars: always load height + blocks; also lastBlockId when D1 ready
      for (const v of ["height", "blocks"]) {
        const val = await this.db.get(v)
        if (val !== null) {
          this[v] = val
          fresh = false
        }
      }
      if (this._d1Ready) {
        const lbid = await this.db.get("lastBlockId")
        if (lbid !== null) {
          this.lastBlockId = lbid
          fresh = false
        }
        // Migration: derive lastBlockId from blocks if not yet stored
        if (this.lastBlockId === "0" && this.blocks.length > 0) {
          this.lastBlockId = this.blocks[this.blocks.length - 1]
        }
      }
      // Always load blocks + blockmap so the O(n) scan fallback in tgql.js works.
      // Skip eagerly loading txs when D1 is ready (D1 is authoritative for tx queries).
      const collections = this._d1Ready
        ? ["items", "env", "modules", "wasms", "addrmap", "blockmap", "modmap", "msgs"]
        : ["items", "txs", "env", "modules", "wasms", "addrmap", "blockmap", "modmap", "msgs"]
      for (const v of collections) {
        this[v] ??= {}
        const items = await this.db.getKeys({ start: v, end: v + "a" })
        if (items?.length) fresh = false
        for (const v2 of items || []) {
          const key = v2.split(".")[0]
          const field = v2.split(".")[1]
          if (key === v && field) {
            if (key === "env") {
              let v3 = await this.db.get(v2)
              if (v3 && is(Uint8Array, v3.memory)) {
                v3.compressed = true
              }
              // If memory was offloaded to R2, fetch it
              if (v3 && !v3.memory && v3._r2_memory && this.db.r2GetMemory) {
                try {
                  const r2mem = await this.db.r2GetMemory(field)
                  if (r2mem) {
                    v3.memory = r2mem
                    v3.compressed = true
                  }
                } catch (e) {}
              }
              this[v][field] = v3
            } else {
              this[v][field] = await this.db.get(v2)
            }
          }
        }
      }
      if (fresh) {
        // First boot: persist initial state from initSync()
        const scalarKeys = this._d1Ready ? ["height", "blocks", "lastBlockId"] : ["height", "blocks"]
        for (const v of scalarKeys) await this.set(this[v], v)
        for (const v of ["modules", "wasms", "addrmap", "blockmap", "txs"]) {
          await this.putAll(v)
        }
      }
      // Seed D1 with initial data when D1 is empty, and ensure seed txs
      // from initSync() are present even when D1 already has data
      if (this._d1Ready && this.db?.d1WriteBlock) {
        try {
          const probe = await this.db.d1.prepare("SELECT COUNT(*) as cnt FROM blocks").first()
          if (!probe || probe.cnt === 0) {
            for (const bId of this.blocks) {
              const block = this.blockmap[bId]
              if (block) await this.db.d1WriteBlock(block)
            }
            for (const txId in this.txs) {
              const tx = this.txs[txId]
              if (tx) await this.db.d1WriteTx(tx, tx.block_id ?? "0", tx.block ?? 1)
            }
            for (const addr in this.addrmap) {
              await this.db.d1WriteAddrmap(addr, this.addrmap[addr])
            }
            for (const name in this.modules) {
              await this.db.d1WriteModule(name, this.modules[name])
            }
            for (const id in this.wasms) {
              await this.db.d1WriteWasm(id, this.wasms[id])
            }
          } else {
            // D1 has data but seed txs from initSync() might be missing
            // (e.g. Scheduler-Location added after initial boot)
            const txKeys = Object.keys(this.txs)
            for (const txId of txKeys) {
              const existing = await this.db.d1.prepare("SELECT 1 FROM txs WHERE id = ?").bind(txId).first()
              if (!existing) {
                const tx = this.txs[txId]
                if (tx) await this.db.d1WriteTx(tx, tx.block_id ?? "0", tx.block ?? 1)
              }
            }
            for (const addr in this.addrmap) {
              const existing = await this.db.d1.prepare("SELECT 1 FROM addrmap WHERE address = ?").bind(addr).first()
              if (!existing) await this.db.d1WriteAddrmap(addr, this.addrmap[addr])
            }
          }
        } catch (e) {
          console.error("[ArMem] D1 seed/backfill error:", e?.message || e)
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
    // Check LRU cache first
    if (this._txCache.has(id)) return this._txCache.get(id)
    // Check in-memory txs (always available for non-DB path, seed data for DB path)
    let tx = this.txs[id] ?? null
    // Try D1 first (indexed lookup), then DO storage fallback
    if (!tx && this.db) {
      await this.init()
      if (this.db.d1GetTxById) {
        try { tx = await this.db.d1GetTxById(id) } catch (e) {
          if (!e?.message?.includes("no such table")) throw e
        }
      }
      if (!tx) {
        const val = await this.db.get(`txs.${id}`)
        if (val !== null) tx = val
      }
    }
    // Remote AR fallback — standalone units fetch from main AR
    if (!tx && this._remote) {
      tx = await this._remote.getTx(id)
    }
    if (isNil(tx)) return null
    // Handle bundle references (DO stores {bundle: parentId} for bundled items)
    if (tx.bundle) {
      try {
        let bundleTx = this.txs[tx.bundle] ?? null
        if (!bundleTx && this.db) {
          bundleTx = await this.db.get(`txs.${tx.bundle}`)
        }
        if (bundleTx) {
          // Lazy-load bundle tx data from R2 if needed
          if (!bundleTx.data && this.db?.r2GetTxData) {
            bundleTx.data = await this.db.r2GetTxData(tx.bundle)
          }
          if (bundleTx.data) {
            let bdata = bundleTx.data
            if (typeof bdata === "string") {
              const b64 = bdata.replace(/-/g, "+").replace(/_/g, "/")
              bdata = Buffer.from(b64, "base64")
            }
            let bundle = new Bundle(bdata)
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
          }
        }
      } catch (e) {}
    }
    // Lazy-load data from R2 if not present
    if (tx && !tx.data && this.db?.r2GetTxData) {
      tx.data = await this.db.r2GetTxData(tx.id ?? id)
    }
    // Add to LRU cache
    if (tx) {
      if (this._txCache.size >= this._TX_CACHE_MAX) {
        this._txCache.delete(this._txCache.keys().next().value)
      }
      this._txCache.set(id, tx)
    }
    return tx
  }
  async getWasm(module) {
    let mod = module ?? this.modules.aos2_0_1
    if (!mod) throw Error("module not found")
    let format = null
    // Lazy load wasm entry if needed
    if (!this.wasms[mod] && this.db) {
      const val = await this.db.get(`wasms.${mod}`)
      if (val !== null) this.wasms[mod] = val
    }
    let _wasm = await this.wasms[mod]
    let wasm = _wasm?.data
    if (!wasm) {
      if (_wasm?.file) {
        wasm = await this._getWasm(this.wasms[mod].file)
        format = _wasm.format
      } else {
        // Try R2/KV cache for wasm binary
        if (this.db?.r2GetWasm) {
          try { wasm = await this.db.r2GetWasm(mod) } catch (e) {}
        }
        // Remote AR fallback for wasm binary
        if (!wasm && this._remote) {
          const remoteData = await this._remote.data(mod)
          if (remoteData) {
            wasm = remoteData instanceof Uint8Array ? remoteData : Buffer.from(remoteData, "base64")
            format = _wasm?.format
          }
        }
        if (!wasm) {
          const tx = await this.getTx(mod)
          if (tx) {
            wasm = Buffer.from(tx.data, "base64")
            format = tags(tx.tags)["Module-Format"]
            // Cache to R2 for future reads
            if (this.db?.r2PutWasm) {
              try { await this.db.r2PutWasm(mod, wasm) } catch (e) {}
            }
          }
        } else {
          format = _wasm?.format
        }
      }
    } else format = _wasm.format
    format ??= "wasm64-unknown-emscripten-draft_2024_02_15"
    return { format, mod, wasm }
  }
}
