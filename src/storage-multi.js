// Multi-backend storage adapter
// Routes data to D1 (indexed queries), R2 (large blobs), or DO (hot compute state)
// based on key prefix.

function sanitize(val) {
  if (val == null) return val
  if (typeof val === "function") return null
  if (typeof val !== "object") return val
  if (val instanceof Uint8Array || val instanceof ArrayBuffer) return val
  if (ArrayBuffer.isView(val)) return val
  if (Array.isArray(val)) return val.map(sanitize)
  const out = {}
  for (const k of Object.keys(val)) {
    const v = val[k]
    if (typeof v !== "function") out[k] = sanitize(v)
  }
  return out
}

export default class StorageMulti {
  constructor({ storage, d1, r2, kv }) {
    this.storage = storage // DO storage (always available)
    this.d1 = d1 || null   // D1 database (optional)
    this.r2 = r2 || null   // R2 bucket (optional)
    this.kv = kv || null   // KV namespace (optional)
  }

  // --- D1 helpers ---

  async d1WriteTx(tx, blockId, blockHeight) {
    if (!this.d1) return
    const tags = tx.tags || []
    let dataSize = "0"
    let dataType = ""
    if (tx._data) {
      dataSize = tx._data.size || "0"
      dataType = tx._data.type || ""
    } else if (tx.data_size) {
      dataSize = tx.data_size
    }

    await this.d1.batch([
      this.d1.prepare(
        `INSERT OR REPLACE INTO txs (id, block_id, block_height, owner, recipient, anchor, signature, data_size, data_type, bundle_id, parent_id)
         VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)`
      ).bind(
        tx.id,
        blockId,
        blockHeight,
        tx.owner || "",
        tx.recipient || "",
        tx.anchor || "",
        tx.signature || null,
        dataSize,
        dataType,
        tx.bundledIn?.id || tx.parent?.id || null,
        tx.parent?.id || null
      ),
      ...tags.map(t =>
        this.d1.prepare(
          `INSERT INTO tx_tags (tx_id, name, value) VALUES (?, ?, ?)`
        ).bind(tx.id, t.name, t.value)
      ),
    ])
  }

  async d1WriteBlock(block) {
    if (!this.d1) return
    await this.d1.prepare(
      `INSERT OR REPLACE INTO blocks (id, height, timestamp, previous)
       VALUES (?, ?, ?, ?)`
    ).bind(block.id, block.height, block.timestamp, block.previous || "")
    .run()
  }

  async d1WriteAddrmap(address, entry) {
    if (!this.d1) return
    await this.d1.prepare(
      `INSERT OR REPLACE INTO addrmap (address, key) VALUES (?, ?)`
    ).bind(address, entry.key || entry.address || "")
    .run()
  }

  async d1WriteModule(name, wasmId) {
    if (!this.d1) return
    await this.d1.prepare(
      `INSERT OR REPLACE INTO modules (name, wasm_id) VALUES (?, ?)`
    ).bind(name, wasmId)
    .run()
  }

  async d1WriteWasm(id, entry) {
    if (!this.d1) return
    await this.d1.prepare(
      `INSERT OR REPLACE INTO wasms (id, format, file, variant) VALUES (?, ?, ?, ?)`
    ).bind(
      id,
      entry.format || "wasm64-unknown-emscripten-draft_2024_02_15",
      entry.file || null,
      entry.variant || null
    ).run()
  }

  // --- R2 helpers ---

  async r2PutMemory(pid, memory) {
    if (!this.r2) return false
    await this.r2.put(`env/${pid}/memory`, memory)
    return true
  }

  async r2GetMemory(pid) {
    if (!this.r2) return null
    const obj = await this.r2.get(`env/${pid}/memory`)
    if (!obj) return null
    return new Uint8Array(await obj.arrayBuffer())
  }

  async r2PutTxData(txId, data) {
    if (!this.r2) return false
    await this.r2.put(`txdata/${txId}`, data)
    return true
  }

  async r2GetTxData(txId) {
    if (!this.r2) return null
    const obj = await this.r2.get(`txdata/${txId}`)
    if (!obj) return null
    return await obj.text()
  }

  async r2PutWasm(id, data) {
    if (!this.r2) return false
    await this.r2.put(`wasms/${id}`, data)
    return true
  }

  async r2GetWasm(id) {
    if (!this.r2) return null
    const obj = await this.r2.get(`wasms/${id}`)
    if (!obj) return null
    return new Uint8Array(await obj.arrayBuffer())
  }

  // --- D1 read helpers ---

  async d1GetTxById(id) {
    if (!this.d1) return null
    const row = await this.d1.prepare("SELECT * FROM txs WHERE id = ?").bind(id).first()
    if (!row) return null
    const { results: tags } = await this.d1.prepare(
      "SELECT name, value FROM tx_tags WHERE tx_id = ?"
    ).bind(id).all()
    const tx = {
      id: row.id,
      owner: row.owner || "",
      recipient: row.recipient || "",
      anchor: row.anchor || "",
      signature: row.signature || "",
      tags,
      _data: { size: row.data_size || "0", type: row.data_type || "" },
      block: row.block_id || "",
    }
    if (row.bundle_id) tx.bundledIn = { id: row.bundle_id }
    if (row.parent_id) tx.parent = { id: row.parent_id }
    return tx
  }

  // --- DO storage pass-through (dodb compatible interface) ---

  async put(key, val) {
    await this.storage.put(key, sanitize(val))
  }

  async get(key) {
    return (await this.storage.get(key)) ?? null
  }

  async getKeys({ start, end }) {
    const map = await this.storage.list({ start, end })
    return [...map.keys()]
  }
}
