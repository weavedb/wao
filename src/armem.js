import Arweave from "arweave"
import { last } from "ramda"
import { tags, buildTags, dirname } from "./utils.js"
import { open } from "lmdb"
import { readFileSync } from "fs"
import { resolve } from "path"
import Base from "./armem-base.js"
import { Compressor, Decompressor } from "./waosm-node.js"

export default class ArMem extends Base {
  constructor(args = {}) {
    const { cache } = args
    super({ ...args, Compressor, Decompressor })
    if (cache) this.db = open({ path: cache, compression: true })
    this.initSync()
  }
  async _getWasm(file) {
    const __dirname = await dirname()
    return readFileSync(resolve(__dirname, `lua/${file}.wasm`))
  }
}
