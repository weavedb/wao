import Arweave from "arweave"
import { last } from "ramda"
import { buildTags } from "./utils.js"
export default class ArMem {
  constructor({ MU_URL, CU_URL, SU_URL, GATEWAY_URL, scheduler } = {}) {
    this.__type__ = "mem"
    this.arweave = Arweave.init()
    this.arweave.transactions.getTransactionAnchor = () => this.getAnchor()
    this.arweave.transactions.getPrice = () => 0
    this.addrmap = {}
    this.txs = {}
    this.jwks = {}
    this.blocks = []
    this.blockmap = {}
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
    let txs = []
    for (const k in this.modules) {
      const key = this.modules[k]
      txs.push(key)
      this.txs[key] = {
        id: key,
        block: 0,
        tags: buildTags(null, {
          "Data-Protocol": "ao",
          Variant: "ao.TN.1",
          Type: "Module",
          "Module-Format": "wasm64-unknown-emscripten-draft_2024_02_15",
          "Input-Encoding": "JSON-V1",
          "Output-Encoding": "JSON-V1",
          "Memory-Limit": "1-gb",
          "Compute-Limit": "9000000000000",
        }),
      }
    }
    if (scheduler && SU_URL) {
      const key = scheduler
      txs.push(key)
      this.addrmap[scheduler] = { address: scheduler }
      this.txs[key] = {
        id: key,
        block: 0,
        owner: scheduler,
        tags: buildTags(null, {
          "Data-Protocol": "ao",
          Variant: "ao.TN.1",
          Type: "Scheduler-Location",
          Url: SU_URL,
          "Time-To-Live": 1000 * 60 * 60 * 24 * 365 * 10,
        }),
      }
    }
    this.blockmap["0"] = {
      txs,
      timestamp: Date.now(),
      height: 0,
      previous: "",
      id: "0",
    }
    this.blocks.push("0")
    this.height = 1
  }
  getAnchor() {
    return this.blocks.length === 0
      ? "Do_Uc2Sju_ffp6Ev0AnLVdPtot15rvMjP-a9VVaA5fM"
      : last(this.blockmap[last(this.blocks)].txs)
  }
}
