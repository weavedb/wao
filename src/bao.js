import MAO from "./ao.js"
import { createDataItemSigner } from "@permaweb/aoconnect-69"
import { srcs, buildTags } from "./utils.js"
import { mergeLeft, is, map } from "ramda"
import HB from "./hb.js"

const wait = ms => new Promise(res => setTimeout(() => res(), ms))
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
    if (opt.hb_url) opt.hb = new HB({ url: opt.hb_url })
    super({ ...opt, in_memory: true })
    this.log = opt.log ?? true
    this.in_memory = true
    this.createDataItemSigner = opt.createDataItemSigner ?? createDataItemSigner
    this.hb = opt.hb
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
      recover,
      mem,
    } = opt.connect(opt.mem, {
      variant: opt.variant,
      extensions: opt.extensions,
      cache: opt.cache,
      reset: opt.reset,
      hb: this.hb,
    })
    this.recover = recover
    this.assign = assign
    this.result = async (...opt) => {
      let res = await result(...opt)
      if (!res) {
        await wait(100) // todo: why do we need to wait?
        res = await result(...opt)
      }
      if (res) renderLogs(res.Output)
      return res
    }

    this.results = results
    this.message = message
    this.spawn = async (...opt) => {
      const res = await spawn(...opt)
      if (!opt[0].http_msg && this.log) await this.load({ data: log, pid: res })
      return res
    }
    this.dryrun = async (...opt) => {
      const res = await dryrun(...opt)
      if (res) renderLogs(res.Output)
      return res
    }

    this.monitor = monitor
    this.unmonitor = unmonitor
    this.mem = mem
    this.ar = new opt.AR({ mem })
  }

  async init(jwk) {
    if (!jwk && this.acc[0]) jwk = this.acc[0].jwk
    await this.ar.init(jwk)
    if (this.hb) await this.hb.init(jwk)
    return this
  }

  async postModule({ data, tags = {}, jwk }) {
    let err = null
    ;({ jwk, err } = await this.ar.checkWallet({ jwk }))
    if (err) return { err }
    const t = mergeLeft(tags, {
      "Data-Protocol": "ao",
      Variant: this.variant ?? "ao.TN.1",
      Type: "Module",
      "Module-Format": "wasm64-unknown-emscripten-draft_2024_02_15",
      "Input-Encoding": "JSON-V1",
      "Output-Encoding": "JSON-V1",
      "Memory-Limit": "1-gb",
      "Compute-Limit": "9000000000000",
    })
    const signer = this.createDataItemSigner(jwk)
    const { id, owner, item } = await this.ar.dataitem({
      tags: t,
      data,
      signer,
    })
    await this.ar.postItems(item, jwk)
    return { id }
  }
}

export default AO
