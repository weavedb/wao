import MAR from "./ar.js"
import { buildTags, tags as t } from "./utils.js"
import * as WarpArBundles from "warp-arbundles"
const pkg = WarpArBundles.default ?? WarpArBundles
const { DataItem } = pkg
import { bundleAndSignData, ArweaveSigner } from "arbundles"

import base64url from "base64url"
import ArMem from "./armem.js"
import GQL from "./tgql.js"
import { last, is, includes } from "ramda"
class AR extends MAR {
  constructor(opt = {}) {
    super({ ...opt, in_memory: true })
    this.log = opt.log === true
    this.in_memory = true
    this.mem = opt.mem ?? new ArMem()
    this.gql = new GQL({ mem: this.mem })
    this.arweave = this.mem.arweave
  }
  async owner(di) {
    const raw_owner = di.rawOwner
    const hashBuffer = Buffer.from(
      await crypto.subtle.digest("SHA-256", raw_owner),
    )
    return base64url.encode(hashBuffer)
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
    const owner = await this.owner(di)
    return { id: await di.id, owner, item: di, tags }
  }

  async post({ data = "1984", tags = {}, jwk }) {
    let err = null
    ;({ err, jwk } = await this.checkWallet({ jwk }))
    if (err) return { err }
    let tx = await this.arweave.createTransaction({ data: data })
    let _tags = buildTags(null, tags)
    for (const v of _tags) tx.addTag(v.name, v.value)
    return await this.postTx(tx, jwk)
  }

  async postItems(items, jwk) {
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
      this.mem.addrmap[owner] = { key: di.owner, address: owner }
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
      _items.push(_item)
    }
    const bundle = await bundleAndSignData(items, new ArweaveSigner(jwk))
    const tx = await this.mem.arweave.createTransaction(
      { data: bundle.binary },
      jwk,
    )
    tx.addTag("Bundle-Format", "binary")
    tx.addTag("Bundle-Version", "2.0.0")
    return await this.postTx(tx, jwk, _items)
  }

  async postTx(tx, jwk, items = []) {
    let err = null
    ;({ err, jwk } = await this.checkWallet({ jwk }))
    if (err) return { err }

    let res = null
    if (!tx.id) await this.mem.arweave.transactions.sign(tx, jwk)
    this.mem.height += 1
    let block = {
      id: tx.id,
      timestamp: Date.now(),
      height: this.mem.height,
      previous: last(this.mem.blocks) ?? "",
      txs: [],
    }
    let msg = null
    if (items) {
      for (const item of items) {
        this.mem.txs[item.id] = item
        this.mem.txs[item.id].parent = { id: tx.id }
        this.mem.txs[item.id].bundledIn = { id: tx.id }
        this.mem.txs[item.id].anchor = ""
        const _tags = t(item.tags)
        if (
          includes(_tags.Type, [
            "Message",
            "Process",
            "Module",
            "Scheduler-Location",
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
        //this.mem.txs[tx.id]._data = { size: tx.data_size, type: data_type }
        block.txs.push(item.id)
        this.mem.txs[item.id].block = block.id
      }
    }

    let _tags = []
    for (const v of tx.tags) {
      _tags.push({
        name: base64url.decode(v.name),
        value: base64url.decode(v.value),
      })
    }
    tx.tags = _tags
    tx.owner = await this.arweave.wallets.jwkToAddress({ n: tx.owner })
    this.mem.txs[tx.id] = tx
    block.txs.push(tx.id)
    this.mem.txs[tx.id].block = block.id
    this.mem.blocks.push(block.id)
    this.mem.blockmap[block.id] = block

    if (jwk) {
      const owner = await this.arweave.wallets.jwkToAddress(jwk)
      this.mem.addrmap[owner] = { address: owner, key: jwk.n }
    }
    res = { id: tx.id, status: 200, statusText: "200" }
    if (this.log) {
      if (msg) {
        console.log(
          `New ${msg.type}:\t${msg.id}${msg.pid ? ` > ${msg.pid}` : ""}`,
        )
      } else {
        console.log(`New Post:\t${tx.id}`)
      }
    }
    return { res, err, id: tx.id }
  }

  async tx(id) {
    return this.mem.txs[id]
  }

  async data(id, string) {
    let tx = this.mem.txs[id]
    let _data = this.mem.txs[id]?.data ?? null
    if (is(Uint8Array, _data)) {
      try {
        _data = Buffer.from(_data).toString("base64")
      } catch (e) {}
    } else {
      try {
        JSON.parse(_data)
        _data = Buffer.from(_data).toString("base64")
      } catch (e) {}
    }
    if (tx.format === 2 && _data && string) _data = base64url.decode(_data)
    return _data
  }
}

export default AR
