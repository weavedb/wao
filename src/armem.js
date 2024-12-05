import Arweave from "arweave"
import { last } from "ramda"

export default class ArMem {
  constructor() {
    this.__type__ = "mem"
    this.arweave = Arweave.init()
    this.arweave.transactions.getTransactionAnchor = () => {
      return this.blocks.length === 0 ? "" : last(this.blocks)
    }
    this.arweave.transactions.getPrice = () => 0
    this.txs = {}
    this.height = 0
    this.blocks = []
    this.env = {}
    this.modules = {
      aos2_0_1: "Do_Uc2Sju_ffp6Ev0AnLVdPtot15rvMjP-a9VVaA5fM",
      aos1: "cNlipBptaF9JeFAf4wUmpi43EojNanIBos3EfNrEOWo",
      sqlite: "ghSkge2sIUD_F00ym5sEimC63BDBuBrq4b5OcwxOjiw",
    }
    this.modmap = {}
    this.msgs = {}
    this.wasms = {
      "Do_Uc2Sju_ffp6Ev0AnLVdPtot15rvMjP-a9VVaA5fM": {
        file: "aos2_0_1",
        format: "wasm64-unknown-emscripten-draft_2024_02_15",
      },
      cNlipBptaF9JeFAf4wUmpi43EojNanIBos3EfNrEOWo: {
        file: "aos_1",
        format: "wasm64-unknown-emscripten-draft_2024_02_15",
      },
      ghSkge2sIUD_F00ym5sEimC63BDBuBrq4b5OcwxOjiw: {
        file: "sqlite",
        format: "wasm64-unknown-emscripten-draft_2024_02_15",
      },
    }
  }
}
