import sqlite from "./lua/sqlite.js"
import aos2_0_3 from "./lua/aos2_0_3.js"
import aos2_0_1 from "./lua/aos2_0_1.js"
import aos2_0_4_32 from "./lua/aos2_0_4_32.js"
import Base from "./armem-base.js"
import db from "./lfdb.js"
import init, { Waosm } from "./waosm/waosm.js"
const wasm = { sqlite, aos2_0_3, aos2_0_1, aos2_0_4_32 }

export default class ArMem extends Base {
  constructor(args = {}) {
    super({ ...args, init, Waosm })
    this.db = db(this, args.cache)
    this.initSync()
  }
  async _getWasm(file) {
    return Buffer.from(wasm[file], "base64")
  }
}
