import MAO from "./ao.js"
import { createDataItemSigner } from "@permaweb/aoconnect"
import { srcs, buildTags } from "./utils.js"
import AR from "./tar.js"
import { connect } from "./aoconnect.js"
import { acc } from "./test.js"
import { mergeLeft, is, map } from "ramda"

let log = `
local json = require("json")
ao = ao or {}

function ao.log(...)
    local args = {...}
    local txt = json.encode(args)
    if type(ao.outbox.Output) == 'string' then
      ao.outbox.Output = {ao.outbox.Output}
    end
    table.insert(ao.outbox.Output, txt)
    ao.outbox.Logs = ao.outbox.Logs or {}
    table.insert(ao.outbox.Logs, txt)
end

function ao.result(result)
    if ao.outbox.Error or result.Error then
      return { Error = result.Error or ao.outbox.Error, Output = ao.outbox.Logs }
    end
    return {
        Output = ao.outbox.Logs or result.Output or ao.outbox.Output,
        Messages = ao.outbox.Messages,
        Spawns = ao.outbox.Spawns,
        Assignments = ao.outbox.Assignments,
    }
end
`

const renderLogs = logs => {
  if (is(Array, logs) && logs.length > 0) {
    for (const v of logs) {
      let l = v
      try {
        const p = JSON.parse(v)
        if (JSON.stringify(p) === v) l = p
        console.log(...l)
      } catch (e) {
        console.log(l)
      }
    }
  }
}

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
    this.result = async (...opt) => {
      const res = await result(...opt)
      renderLogs(res.Output)
      return res
    }

    this.results = results
    this.message = message
    this.spawn = async (...opt) => {
      const res = await spawn(...opt)
      await this.load({ data: log, pid: res })
      return res
    }
    this.dryrun = async (...opt) => {
      const res = await dryrun(...opt)
      renderLogs(res.Output)
      return res
    }
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
    const signer = createDataItemSigner(jwk)
    const { id, owner, item } = await this.ar.dataitem({
      tags: t,
      data,
      signer,
    })
    await this.ar.postItems(item, jwk)
    this.mem.wasms[id] = { data, format: t["Module-Format"] }
    return { id }
  }
}

export default AO
