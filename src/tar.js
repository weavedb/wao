import MAR from "./ar.js"
import { buildTags } from "./utils.js"
import { DataItem } from "warp-arbundles"
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

  async dataitem({ target = "", data = "1984", tags = {}, signer }) {
    const _tags = buildTags(tags)
    const item = await signer({ data, tags: _tags, target })
    const di = new DataItem(item.raw)
    const raw_owner = di.rawOwner
    const hashBuffer = Buffer.from(
      await crypto.subtle.digest("SHA-256", raw_owner),
    )
    const owner = base64url.encode(hashBuffer)
    return { id: item.id, owner, item }
  }

  async post({ data = "1984", tags = {}, jwk }) {
    let tx = await this.arweave.createTransaction({ data: data })
    let _tags = buildTags(null, tags)
    for (const v of _tags) tx.addTag(v.name, v.value)
    return await this.postTx(tx, jwk, {
      recipient: "",
      tags: _tags,
      data,
      owner: await this.arweave.wallets.jwkToAddress(jwk),
    })
  }

  async postItem(item, jwk) {
    const tx = await this.mem.arweave.createTransaction({ data: item.raw }, jwk)
    tx.addTag("Bundle-Format", "binary")
    tx.addTag("Bundle-Version", "2.0.0")
    const di = new DataItem(item.raw)
    const rowner = di.rawOwner
    const hashBuffer = Buffer.from(
      await crypto.subtle.digest("SHA-256", rowner),
    )
    const owner = base64url.encode(hashBuffer)
    let data = di.data
    try {
      data = base64url.decode(di.data)
    } catch (e) {}
    let _item = {
      recipient: di.target,
      id: item.id,
      item: di,
      owner,
      tags: di.tags,
      data,
    }
    this.mem.txs[item.id] = _item
    return await this.postTx(tx, jwk, _item)
  }

  async postTx(tx, jwk, item) {
    let [res, err] = [null, null]
    await this.mem.arweave.transactions.sign(tx, jwk)
    if (item) {
      this.mem.height += 1

      const block = {
        id: tx.id,
        timestamp: Date.now(),
        height: this.mem.height,
        previous: last(this.mem.blocks) ?? "",
      }

      if (!item.id) {
        item.id = tx.id
        this.mem.txs[item.id] = item
        this.mem.txs[item.id].parent = { id: block.id }
      } else {
        this.mem.txs[item.id].parent = { id: "" }
      }
      block.txs = [item.id]
      this.mem.txs[item.id].block = block.id
      this.mem.txs[item.id].parent = { id: block.id }
      this.mem.blocks.push(block.id)
      this.mem.blockmap[block.id] = block
    }
    res = { id: tx.id, status: 200, statusText: "200" }
    return { res, err, id: tx.id }
  }

  tx(id) {
    return this.mem.txs[id]
  }

  data(id) {
    return this.mem.txs[id].data
  }
}

export default AR
