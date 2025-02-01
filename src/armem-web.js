import wdb from "./lua/weavedb-lite.js"
import Base from "./armem-base.js"
import db from "./lfdb.js"
import init, { Compressor, Decompressor } from "./waosm/waosm.js"

export default class ArMem extends Base {
  constructor(args = {}) {
    super({ ...args, init, Compressor, Decompressor })
    this.db = db(this)
    this.initSync()
  }
  async _getWasm(file) {
    return Buffer.from(wdb, "base64")
  }
}
