import { createDataItemSigner } from "@permaweb/aoconnect"
import { DataItem } from "warp-arbundles"
import base64url from "base64url"
import { buildTags } from "./utils.js"

export default class ArMem {
  constructor() {
    this.txs = {}
    this.height = 0
    this.blocks = []
  }
  async post({ data = "1984", tags = {}, jwk, signer }) {
    signer ??= createDataItemSigner(jwk)
    const _tags = buildTags(tags)
    const item = await signer({ data, tags: _tags })
    return await this.postSignedTx(item)
  }
  async postSignedTx(item) {
    const di = new DataItem(item.raw)
    const rowner = di.rawOwner
    const hashBuffer = Buffer.from(
      await crypto.subtle.digest("SHA-256", rowner),
    )
    const owner = base64url.encode(hashBuffer)
    this.height += 1
    let data = di.data
    try {
      data = base64url.decode(di.data)
    } catch (e) {}
    this.txs[item.id] = {
      id: item.id,
      item: di,
      owner,
      height: this.height,
      tags: di.tags,
      data,
    }
    this.blocks.push(item.id)
    return this.txs[item.id]
  }
  tx(id) {
    return this.txs[id]
  }
  data(id) {
    return this.txs[id].data
  }
}
