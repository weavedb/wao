import MAR from "./ar.js"
import { buildTags, tags as t } from "./utils.js"
import * as WarpArBundles from "warp-arbundles"
const pkg = WarpArBundles.default ?? WarpArBundles
const { DataItem } = pkg

import base64url from "base64url"
import ArMem from "./armem.js"
import GQL from "./tgql.js"
import { last } from "ramda"
class AR extends MAR {
  constructor(opt = {}) {
    super({ ...opt, in_memory: true })
    this.in_memory = true
    this.mem = opt.mem ?? new ArMem()
    this.gql = new GQL({ mem: this.mem })
    this.arweave = this.mem.arweave
  }

  async dataitem({ target = "", data = "1984", tags = {}, signer, item }) {
    let di = item
    if (!di) {
      const _tags = buildTags(tags)
      const _item = await signer({ data, tags: _tags, target })
      di = new DataItem(_item.raw)
    } else {
      tags = t(di.tags)
    }
    const raw_owner = di.rawOwner
    const hashBuffer = Buffer.from(
      await crypto.subtle.digest("SHA-256", raw_owner),
    )
    const owner = base64url.encode(hashBuffer)
    return { id: await di.id, owner, item: di, tags }
  }

  async post({ data = "1984", tags = {}, jwk }) {
    let tx = await this.arweave.createTransaction({ data: data })
    let _tags = buildTags(null, tags)
    for (const v of _tags) tx.addTag(v.name, v.value)
    const owner = await this.arweave.wallets.jwkToAddress(jwk)
    this.mem.addrmap[owner] = jwk.n
    return await this.postTx(tx, jwk, {
      recipient: "",
      tags: _tags,
      data,
      owner,
    })
  }

  async postItem(di, jwk) {
    const tx = await this.mem.arweave.createTransaction(
      { data: di.binary },
      jwk,
    )
    tx.addTag("Bundle-Format", "binary")
    tx.addTag("Bundle-Version", "2.0.0")
    const data_size = Buffer.byteLength(di.rawData).toString()
    let data_type = ""
    for (const t of di.tags) if (t.name === "Content-Type") data_type = t.value
    const rowner = di.rawOwner
    const hashBuffer = Buffer.from(
      await crypto.subtle.digest("SHA-256", rowner),
    )
    const owner = base64url.encode(hashBuffer)
    this.mem.addrmap[owner] = di.owner
    let data = di.data
    try {
      data = base64url.decode(di.data)
    } catch (e) {}
    let _item = {
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
    this.mem.txs[await di.id] = _item
    return await this.postTx(tx, jwk, _item)
  }

  async postTx(tx, jwk, item) {
    let [res, err] = [null, null]
    if (!tx.id) await this.mem.arweave.transactions.sign(tx, jwk)
    this.mem.height += 1
    let block = {
      id: tx.id,
      timestamp: Date.now(),
      height: this.mem.height,
      previous: last(this.mem.blocks) ?? "",
    }
    if (item) {
      if (!item.id) {
        item.id = tx.id
        this.mem.txs[tx.id] = item
        this.mem.txs[tx.id].parent = null
        this.mem.txs[tx.id].signature = tx.signature
        this.mem.txs[tx.id].anchor = tx.last_tx
        let data_type = ""
        for (const v of tx.tags) {
          if (
            v.get("name", { decode: true, string: true }) === "Content-Type"
          ) {
            data_type = v.get("value", { decode: true, string: true })
          }
        }
        this.mem.txs[tx.id]._data = { size: tx.data_size, type: data_type }
      } else {
        this.mem.txs[item.id].parent = { id: block.id }
      }
      block.txs = [item.id]
      this.mem.txs[item.id].block = block.id
    } else {
      let _tags = []
      for (const v of tx.tags) {
        _tags.push({
          name: base64url.decode(v.name),
          value: base64url.decode(v.value),
        })
      }
      tx.tags = _tags
      this.mem.txs[tx.id] = tx
      block.txs = [tx.id]
      this.mem.txs[tx.id].block = block.id
    }
    this.mem.blocks.push(block.id)
    this.mem.blockmap[block.id] = block

    if (jwk) {
      const owner = await this.arweave.wallets.jwkToAddress(jwk)
      this.mem.addrmap[owner] = jwk.n
    }
    res = { id: tx.id, status: 200, statusText: "200" }
    return { res, err, id: tx.id }
  }

  async tx(id) {
    return this.mem.txs[id]
  }

  async data(id, string) {
    let tx = this.mem.txs[id]
    let _data = this.mem.txs[id]?.data ?? null
    if (tx.format === 2 && _data && string) _data = base64url.decode(_data)
    return _data
  }
}

export default AR
