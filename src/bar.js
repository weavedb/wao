import MAR from "./ar.js"
import { buildTags, tags as t } from "./utils.js"
import * as WarpArBundles from "warp-arbundles"
const pkg = WarpArBundles.default ?? WarpArBundles
const { DataItem } = pkg
import { Bundle, bundleAndSignData, ArweaveSigner } from "arbundles"
import base64url from "base64url"
import GQL from "./tgql.js"
import { last, is, includes, isNil } from "ramda"

function parseSignatureInput(input) {
  const match = input.match(
    /^([^=]+)=\(([^)]+)\);alg="([^"]+)";keyid="([^"]+)"$/
  )
  if (!match) throw new Error("Invalid signature-input format")

  const [, label, fieldsStr, alg, keyid] = match
  const fields = fieldsStr.split('" "').map(f => f.replace(/"/g, ""))
  return { label, fields, alg, keyid }
}

class AR extends MAR {
  constructor(opt = {}) {
    super({ ...opt, in_memory: true })
    this.log = opt.log === true
    this.in_memory = true
    this.mem = opt.mem ?? new opt.ArMem()
    this.gql = new GQL({ mem: this.mem })
    this.arweave = this.mem.arweave
    // Block batching (DB path only)
    this._pendingBatch = []
    this._flushTimer = null
    this._batchWindow = opt.batchWindow ?? 0 // ms, 0 = disabled
  }
  isHttpMsg(item) {
    if (typeof item === "object" && item !== null) {
      const tags = t(item.tags)
      if (!isNil(tags["signature-input"])) return true
    }
    return false
  }

  async httpmsg(msg) {
    const tags = t(msg.tags)
    let owner = null
    try {
      const { keyid: n } = parseSignatureInput(tags["signature-input"])
      owner = await this.arweave.wallets.jwkToAddress({ n })
    } catch (e) {
      owner = tags.Owner ?? null
    }
    await this.mem.set(msg, "txs", msg.id)
    return { item: msg, id: tags.id, tags, owner }
  }

  async owner(di) {
    return base64url.encode(
      Buffer.from(await crypto.subtle.digest("SHA-256", di.rawOwner))
    )
  }

  async dataitem({ target = "", data = "1984", tags = {}, signer, item }) {
    let di = item
    if (!di) {
      if (!item?.signature) {
        const _item = await signer({ data, tags: buildTags(tags), target })
        di = new DataItem(_item.raw)
      }
    } else tags = t(di.tags)
    const owner = await this.owner(di)
    return { id: await di.id, owner, item: di, tags }
  }

  async post({ data = "1984", tags = {}, jwk }) {
    let err = null
    ;({ err, jwk } = await this.checkWallet({ jwk }))
    if (err) return { err }
    let tx = await this.arweave.createTransaction({ data: data })
    for (const v of buildTags(null, tags)) tx.addTag(v.name, v.value)
    return await this.postTx(tx, jwk)
  }

  async postItems(items, jwk) {
    // Remote mode: forward bundle to main AR via HTTP
    if (this.mem._remote) {
      let err = null
      ;({ err, jwk } = await this.checkWallet({ jwk }))
      if (err) return { err }
      if (!is(Array, items)) items = [items]
      const bundle = await bundleAndSignData(items, new ArweaveSigner(jwk))
      const tx = await this.mem.arweave.createTransaction(
        { data: bundle.binary },
        jwk
      )
      tx.addTag("Bundle-Format", "binary")
      tx.addTag("Bundle-Version", "2.0.0")
      return await this.postTx(tx, jwk, items.map(i => ({ id: i.id })))
    }
    let err = null
    ;({ err, jwk } = await this.checkWallet({ jwk }))
    if (err) return { err }
    if (!is(Array, items)) items = [items]
    let _items = []
    for (const di of items) {
      di._id = await di.id
      const data_size = Buffer.byteLength(di.rawData).toString()
      let data_type = ""
      for (const t of di.tags)
        if (t.name === "Content-Type") data_type = t.value
      const owner = await this.owner(di)
      await this.mem.set({ key: di.owner, address: owner }, "addrmap", owner)
      // D1: write addrmap
      if (this.mem.db?.d1WriteAddrmap) {
        try { await this.mem.db.d1WriteAddrmap(owner, { key: di.owner, address: owner }) } catch (e) {}
      }
      let _item = {
        _data: { size: data_size, type: data_type },
        anchor: di.anchor,
        signature: di.signature,
        recipient: di.target,
        id: await di.id,
        item: di,
        owner,
        tags: di.tags,
        data: di.data,
      }
      // Extract raw data to R2
      if (_item.data && this.mem.db?.r2PutTxData) {
        await this.mem.db.r2PutTxData(_item.id, _item.data)
        _item._r2_data = true
        _item.data = ""
      }
      await this.mem.set(_item, "txs", await di.id)
      _items.push(_item)
    }
    const bundle = await bundleAndSignData(items, new ArweaveSigner(jwk))
    const tx = await this.mem.arweave.createTransaction(
      { data: bundle.binary },
      jwk
    )
    tx.addTag("Bundle-Format", "binary")
    tx.addTag("Bundle-Version", "2.0.0")
    return await this.postTx(tx, jwk, _items)
  }

  async postTx(tx, jwk, items = []) {
    // Remote mode: sign locally, POST to main AR
    if (this.mem._remote) {
      let err = null
      ;({ err, jwk } = await this.checkWallet({ jwk }))
      if (err) return { err }
      if (!tx.id) await this.mem.arweave.transactions.sign(tx, jwk)
      const res = await this.mem._remote.postTx(tx)
      return { res: { id: tx.id, status: 200 }, err: null, id: tx.id }
    }
    let err = null
    ;({ err, jwk } = await this.checkWallet({ jwk }))
    if (err) return { err }

    let res = null
    if (!tx.id) await this.mem.arweave.transactions.sign(tx, jwk)
    let height
    if (this.mem._d1Ready) {
      // D1 ready: increment in-memory, flush every 10 txs
      this.mem.height = (this.mem.height ?? 0) + 1
      height = this.mem.height
      if (height % 10 === 0) {
        await this.mem.set(height, "height")
      }
    } else {
      height = (await this.mem.get("height")) + 1
      await this.mem.set(height, "height")
    }
    let previous = this.mem._d1Ready
      ? (this.mem.lastBlockId || "")
      : (last(await this.mem.get("blocks")) ?? "")
    let block = {
      id: tx.id,
      timestamp: Date.now(),
      height,
      previous,
      txs: [],
    }
    let msg = null
    if (items) {
      for (const item of items) {
        let _txs = item
        _txs.parent = { id: tx.id }
        _txs.bundledIn = { id: tx.id }
        _txs.anchor = ""
        const _tags = t(item.tags)
        if (
          includes(_tags.Type, [
            "Message",
            "Process",
            "Module",
            "Scheduler-Location",
            "Scheduler-Transfer",
            "Attestation",
            "Available",
          ])
        ) {
          msg = { id: item.id, type: _tags.Type }
          if (msg.type === "Process") msg.pid = item.recipient
        }
        let data_type = ""
        for (const v of item.item.tags) {
          if (v.name === "Content-Type") data_type = v.value
        }
        block.txs.push(item.id)
        _txs.block = block.id
        // Re-store item with parent/bundledIn/block, but drop the raw
        // DataItem binary (item.item) to save space — data is in R2 or
        // can be reconstructed from the wrapper bundle tx.
        const slim = { ..._txs, bundle: tx.id }
        delete slim.item
        await this.mem.set(slim, "txs", item.id)
      }
    }
    let _tags = []
    for (const v of tx.tags) {
      _tags.push({
        name: base64url.decode(v.name),
        value: base64url.decode(v.value),
      })
    }
    const __tags = t(_tags)
    if (__tags.Type === "Module") {
      await this.mem.set(
        {
          data: Buffer.from(tx.data, "base64"),
          format: __tags["Module-Format"],
        },
        "wasms",
        tx.id
      )
    }
    tx.tags = _tags
    tx.owner = await this.arweave.wallets.jwkToAddress({ n: tx.owner })
    tx.recipient = tx.target || ""
    let _txs = tx
    block.txs.push(tx.id)
    _txs.block = block.id
    // Extract raw data to R2
    if (_txs.data && this.mem.db?.r2PutTxData) {
      await this.mem.db.r2PutTxData(tx.id, _txs.data)
      _txs._r2_data = true
      _txs.data = ""
    }
    await this.mem.set(_txs, "txs", tx.id)
    // Update block tracking — always maintain blocks array + blockmap
    // so the O(n) scan fallback in tgql.js works even if D1 is stale
    if (this.mem._d1Ready) {
      await this.mem.set(block.id, "lastBlockId")
      this.mem.lastBlockId = block.id
    }
    this.mem.blocks ??= []
    this.mem.blocks.push(block.id)
    await this.mem.set(this.mem.blocks, "blocks")
    await this.mem.set(block, "blockmap", block.id)

    if (jwk) {
      const owner = await this.arweave.wallets.jwkToAddress(jwk)
      await this.mem.set({ address: owner, key: jwk.n }, "addrmap", owner)
      // D1: write addrmap
      if (this.mem.db?.d1WriteAddrmap) {
        try { await this.mem.db.d1WriteAddrmap(owner, { address: owner, key: jwk.n }) } catch (e) {}
      }
    }

    // D1 write: block + txs + tags (only when D1 schema is verified ready)
    if (this.mem._d1Ready && this.mem.db?.d1WriteBlock) {
      const mainTx = {
        id: tx.id,
        owner: tx.owner,
        recipient: tx.recipient || "",
        anchor: tx.anchor || "",
        signature: tx.signature || "",
        tags: _tags,
        _data: tx._data || { size: tx.data ? String(tx.data.length) : "0", type: "" },
        bundledIn: tx.bundledIn,
        parent: tx.parent,
      }
      if (this._batchWindow > 0) {
        this._pendingBatch.push({ block, mainTx, items: [...items] })
        if (!this._flushTimer) {
          this._flushTimer = setTimeout(() => this._flushBatch(), this._batchWindow)
        }
      } else {
        await this.mem.db.d1WriteBlock(block)
        await this.mem.db.d1WriteTx(mainTx, block.id, block.height)
        for (const item of items) {
          await this.mem.db.d1WriteTx(item, block.id, block.height)
        }
      }
    }

    res = { id: tx.id, status: 200, statusText: "200" }
    if (this.log) {
      if (msg) {
        console.log(
          `New ${msg.type}:\t${msg.id}${msg.pid ? ` > ${msg.pid}` : ""}`
        )
      } else {
        console.log(`New Post:\t${tx.id}`)
      }
    }
    return { res, err, id: tx.id }
  }

  async _flushBatch() {
    this._flushTimer = null
    if (!this._pendingBatch.length) return
    const batch = this._pendingBatch.splice(0)
    for (const { block, mainTx, items } of batch) {
      await this.mem.db.d1WriteBlock(block)
      await this.mem.db.d1WriteTx(mainTx, block.id, block.height)
      for (const item of items) {
        await this.mem.db.d1WriteTx(item, block.id, block.height)
      }
    }
    // Flush height to DO after batch
    await this.mem.set(this.mem.height, "height")
  }

  async tx(id) {
    return await this.mem.getTx(id)
  }

  async data(id, _string, log) {
    let decode = true
    let string = _string
    if (is(Object, _string)) {
      if (!isNil(_string.decode)) decode = _string.decode
      if (!isNil(_string.string)) string = _string.string
    }
    let tx = await this.mem.getTx(id)
    let _data = tx?.data ?? null
    if (_data && is(String, _data)) {
      _data = tobuff(_data)
    }
    let isBuf = is(Uint8Array, _data) || is(ArrayBuffer, _data)
    let isStr = is(String, _data)
    if (decode === false) {
      if (isStr) _data = new TextEncoder().encode(_data)
      return base64url.encode(_data)
    } else {
      if (isBuf && string) {
        return _data.toString()
      } else if (isStr && string !== true) {
        return new TextEncoder().encode(_data)
      }
    }
    return _data
  }
}
function tobuff(base64url) {
  const base64 = base64url.replace(/-/g, "+").replace(/_/g, "/")
  const paddedBase64 = base64.padEnd(
    base64.length + ((4 - (base64.length % 4)) % 4),
    "="
  )
  return Buffer.from(paddedBase64, "base64")
}

export default AR
