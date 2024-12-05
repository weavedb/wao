import MAR from "./ar.js"
import { buildTags } from "./utils.js"
import { DataItem } from "warp-arbundles"
import base64url from "base64url"
import ArMem from "./armem.js"

class AR extends MAR {
  constructor(opt = {}) {
    super({ ...opt, in_memory: true })
    this.in_memory = true
    this.mem = opt.mem ?? new ArMem()
  }

  async txs(pid) {
    let _txs = []
    for (let v of this.mem.env[pid].txs) {
      _txs.push({ tags: v.tags, id: v.id })
    }
    return _txs
  }

  async dataitem({ data = "1984", tags = {}, signer }) {
    const _tags = buildTags(tags)
    const item = await signer({ data, tags: _tags })
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
    this.mem.height += 1
    return await this.postTx(tx, jwk, {
      height: this.mem.height,
      tags: _tags,
      data,
    })
  }

  async postItem(item, jwk) {
    const tx = await this.mem.arweave.createTransaction({
      data: item.raw,
    })
    tx.addTag("Bundle-Format", "binary")
    tx.addTag("Bundle-Version", "2.0.0")
    const di = new DataItem(item.raw)
    const rowner = di.rawOwner
    const hashBuffer = Buffer.from(
      await crypto.subtle.digest("SHA-256", rowner),
    )
    const owner = base64url.encode(hashBuffer)
    this.mem.height += 1
    let data = di.data
    try {
      data = base64url.decode(di.data)
    } catch (e) {}
    this.mem.txs[item.id] = {
      id: item.id,
      item: di,
      owner,
      height: this.mem.height,
      tags: di.tags,
      data,
    }
    this.mem.blocks.push(item.id)
    return await this.postTx(tx, jwk)
  }

  async postTx(tx, jwk, item) {
    let [res, err] = [null, null]
    await this.mem.arweave.transactions.sign(tx, jwk)
    if (item) {
      if (!item.id) item.id = tx.id
      this.mem.txs[item.id] = item
      this.mem.blocks.push(item.id)
    }
    res = { id: tx.id, status: 200, statusText: "200" }
    //res = await this.arweave.transactions.post(tx)
    //if (res.status !== 200) err = res.statusText
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
