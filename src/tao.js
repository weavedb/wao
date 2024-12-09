import MAO from "./ao.js"
import { createDataItemSigner } from "@permaweb/aoconnect"
import { srcs, buildTags } from "./utils.js"
import AR from "./tar.js"
import { connect } from "./aoconnect.js"
import { acc } from "./test.js"
import { mergeLeft } from "ramda"

class AO extends MAO {
  constructor(opt = {}) {
    super({ ...opt, in_memory: true })
    this.in_memory = true
    const {
      modules,
      results,
      assign,
      result,
      message,
      spawn,
      dryrun,
      monitor,
      unmonitor,
      mem,
    } = connect(opt.mem)
    this.module = mem.modules.aos2_0_1
    this.assign = assign
    this.result = result
    this.results = results
    this.message = message
    this.spawn = spawn
    this.dryrun = dryrun
    this.monitor = monitor
    this.unmonitor = unmonitor
    this.mem = mem
    this.ar = new AR({ mem })
  }

  async init(jwk) {
    if (!jwk && this.acc[0]) jwk = this.acc[0].jwk
    await this.ar.init(jwk)
    return this
  }

  async postModule({ data, tags = {}, jwk }) {
    let err = null
    ;({ jwk, err } = await this.ar.checkWallet({ jwk }))
    if (err) return { err }
    const t = mergeLeft(tags, {
      "Data-Protocol": "ao",
      Variant: "ao.TN.1",
      Type: "Module",
      "Module-Format": "wasm64-unknown-emscripten-draft_2024_02_15",
      "Input-Encoding": "JSON-V1",
      "Output-Encoding": "JSON-V1",
      "Memory-Limit": "1-gb",
      "Compute-Limit": "9000000000000",
    })

    const _tags = buildTags(null, t)
    const signer = createDataItemSigner(jwk)
    const { id, owner } = await this.ar.dataitem({ tags: _tags, data, signer })
    await this.ar.post({ data, tags: t, jwk })
    this.mem.wasms[id] = { data, format: t["Module-Format"] }
    return id
  }
}

export default AO
