import HB from "./hb.js"
import { DataItem } from "arbundles"
import {
  toANS104Request,
  parseSignatureInput,
  tags,
  toGraphObj,
} from "./utils.js"
import { connect } from "./aoconnect.js"
import { GQL, cu, su, mu } from "./test.js"
import { keys, map, isNil, reverse, omit } from "ramda"
import { httpbis, createVerifier } from "http-message-signatures"
import { createPublicKey } from "node:crypto"
const { verifyMessage } = httpbis

class Adaptor {
  constructor({ hb_url, aoconnect, log = false, db }) {
    this.data = {}
    let hb = null
    if (hb_url) hb = new HB({ url: hb_url })
    const {
      ar: _ar,
      message,
      spawn,
      dryrun,
      result,
      results,
      mem,
      monitor,
      unmonitor,
      recover,
    } = connect(aoconnect, { log, cache: db, hb })
    this.recover = recover
    this.monitor = monitor
    this.unmonitor = unmonitor
    this.spawn = spawn
    this._ar = _ar
    this.message = message
    this.dryrun = dryrun
    this.result = result
    this.results = results
    this.mem = mem
    this.gql = new GQL({ mem })
  }
  async get(req) {
    return await this[req.device](req)
  }
  async bd(req) {
    return await this[`bd_${req.method.toLowerCase()}`](req)
  }
  async ar(req) {
    return await this[`ar_${req.method.toLowerCase()}`](req)
  }
  async su(req) {
    return await this[`su_${req.method.toLowerCase()}`](req)
  }
  async mu(req) {
    return await this[`mu_${req.method.toLowerCase()}`](req)
  }
  async cu(req) {
    return await this[`cu_${req.method.toLowerCase()}`](req)
  }
  async bd_get(req) {
    switch (req.path) {
      default:
    }
  }

  async bd_post(req) {
    switch (req.path) {
      case "/tx":
        return await this.bd_post_tx(req)
      default:
        return await this.bad()
    }
  }

  async ar_get(req) {
    switch (req.path) {
      case "/":
        return await this.ar_get_root(req)
      case "/wallet/:id/balance":
        return await this.ar_get_wallet_balance(req)
      case "/mint/:id/:amount":
        return await this.ar_get_mint(req)
      case "/tx/:id/offset":
        return await this.ar_get_tx_offset(req)
      case "/tx_anchor":
        return await this.ar_get_tx_anchor(req)
      case "/mine":
        return await this.ar_get_mine(req)
      case "/:id":
        return await this.ar_get_id(req)
      case "/price/:id":
        return await this.ar_get_price(req)
      default:
        return await this.bad()
    }
  }

  async ar_post(req) {
    switch (req.path) {
      case "/graphql":
        return await this.ar_post_graphql(req)
      case "/:id":
        return await this.ar_post_id(req)
      default:
        return await this.bad()
    }
  }

  async su_get(req) {
    switch (req.path) {
      case "/":
        return await this.su_get_root(req)
      case "/timestamp":
        return await this.su_get_timestamp(req)
      case "/:pid":
        return await this.su_get_pid(req)
      default:
        return await this.bad()
    }
  }

  async su_post(req) {
    switch (req.path) {
      default:
        return await this.bad()
    }
  }

  async mu_get(req) {
    switch (req.path) {
      case "/":
        return await this.mu_get_root(req)
      case "/monitor:pid":
        return await this.mu_get_monitor(req)
      default:
        return await this.bad()
    }
  }

  async mu_delete(req) {
    switch (req.path) {
      case "/monitor:pid":
        return await this.mu_delete_monitor(req)
      default:
        return await this.bad()
    }
  }

  async mu_post(req) {
    switch (req.path) {
      case "/":
        return await this.mu_post_root(req)
      case "/monitor:pid":
        return await this.mu_post_monitor(req)
      default:
        return await this.bad()
    }
  }

  async cu_get(req) {
    switch (req.path) {
      case "/":
        return await this.cu_get_root(req)
      case "/result/:mid":
        return await this.cu_get_result(req)
      case "/results/:pid":
        return await this.cu_get_results(req)
      case "/state/:pid":
        return await this.cu_get_state(req)
      default:
        return await this.bad()
    }
  }

  async cu_post(req) {
    switch (req.path) {
      case "/result/:mid": // not in the AO spec, but HyperBEAM queries it
        return await this.cu_post_result(req)
      case "/dry-run": // not in the AO spec
        return await this.cu_post_dryrun(req)
      default:
        return await this.bad()
    }
  }
  async bad({}) {
    return { status: 400, error: "bad request" }
  }

  async bd_post_tx({ query, params, body, headers, method }) {
    // todo: items from HB has wrong sigs and ids
    return { send: "Success" }
    if (!Buffer.isBuffer(body)) {
      console.log("BD: Invalid body | expected raw Buffer")
      return { status: 400, send: "Invalid body: expected raw Buffer" }
    }
    const lines = body.toString("utf8").split(/\r?\n/)
    const sigs = {}
    let currentKey = null

    for (let line of lines) {
      const trimmed = line.trim()
      if (/^--[a-zA-Z0-9_\-=]+/.test(trimmed)) {
        currentKey = null
        continue
      }
      const headerMatch = trimmed.match(/^([a-zA-Z0-9_-]+):\s*(.*)$/)
      if (headerMatch && !headerMatch[2].includes(": ")) {
        const key = headerMatch[1]
        const value = headerMatch[2]
        sigs[key] = value
        currentKey = key
      } else if (currentKey) sigs[currentKey] += "\n" + line
    }
    sigs.target = headers.process
    sigs.slot = headers.slot

    const input = parseSignatureInput(headers["signature-input"])
    const key = { kty: "RSA", n: input.keyid, e: "AQAB" }
    const verifier = createVerifier(
      createPublicKey({ key, format: "jwk" }),
      "rsa-pss-sha512"
    )
    let id = null
    try {
      const isValid = await verifyMessage(
        { keyLookup: params => ({ verify: verifier }) },
        {
          method: method,
          headers: headers,
          url: `http://ao.com${headers.path}`,
        }
      )
      const item = toANS104Request(sigs).item

      if (sigs.slot === "0" || sigs.type === "Process") {
        for (let v of item.tags) if (v.name === "Type") v.value = "Process"
        const res = await this.spawn({
          http_msg: item,
          module: sigs.module,
          scheduler: sigs.scheduler,
        })
      } else if (sigs.type === "Message") {
        for (let v of item.tags) if (v.name === "id") item.target = v.value
        const res = await this.message({
          slot: sigs.slot,
          http_msg: item,
          process: sigs.target,
        })
      }
    } catch (e) {
      console.log(e)
    }
    return { send: "Success" }
  }

  async cu_get_root({ query, params, body, headers, method }) {
    return { json: { timestamp: Date.now(), address: cu.addr } }
  }

  async cu_get_state({ query, params, body, headers, method }) {
    const pid = params.pid
    const memory = this.mem.env[pid]?.memory ?? null
    if (!memory) {
      return {
        status: 404,
        error: `TransactionNotFound: Process ${pid} was not found on gateway`,
      }
    } else return { send: Buffer.from(memory) }
  }

  async cu_post_dryrun({ query, params, body, headers, method }) {
    const process = query["process-id"]
    const { Id: id, Owner: owner, Tags: tags, Data: data } = body
    const res2 = await this.dryrun({ id, owner, tags, data, process })
    if (!res2) return { status: 400, error: true }
    else {
      delete res2.Memory
      return { json: res2 }
    }
  }
  async cu_post_results({ query, params, body, headers, method }) {
    const pid = params.pid
    const { from = null, to = null, sort = "ASC", limit = 25 } = query
    let results = this.mem.env[pid]?.results ?? []
    if (sort === "DESC") results = reverse(results)
    let _res = []
    let i = 1
    let count = 0
    let started = isNil(from)
    for (let v of results) {
      if (started) {
        _res.push({ cursor: v, node: this.mem.msgs[v]?.res })
        count++
        if (!isNil(to) && v === to) break
        if (limit <= count) break
      } else if (from === v) started = true

      i++
    }
    return { json: { edges: _res } }
  }

  async cu_get_result({ query, params, body, headers, method }) {
    let message = params.mid
    const process = query["process-id"]
    // check if recovery is ongoing and
    if (isNil(this.mem.env[process])) {
      const { success } = await this.recover(process)
      if (!success) {
        console.log("process not found:", query["process-id"])
        return { status: 404, error: "not Found" }
      }
    }
    const slot = message
    if (!/^--[0-9a-zA-Z_-]{43,44}$/.test(message)) {
      message = this.mem.env[process]?.results?.[slot]
    }
    if (isNil(message)) {
      await this.recover(process)
      message = this.mem.env[process]?.results?.[slot]
      if (isNil(message)) return { status: 404, error: "not Found" }
    }
    const res2 = await this.result({ message, process })
    if (isNil(message)) return { json: res2 }
  }

  async su_get_root({ query, params, body, headers, method }) {
    return {
      json: {
        Unit: "Scheduler",
        Timestamp: Date.now(),
        Address: su.addr,
        Processes: keys(this.mem.env),
      },
    }
  }

  async su_get_timestamp({ query, params, body, headers, method }) {
    return { json: { timestamp: Date.now(), block_height: this.mem.height } }
  }
  async su_get_pid({ query, params, body, headers, method }) {
    const pid = params.pid
    const edges = map(async v => {
      const tx = await this.mem.getTx(v)
      const _tags = tags(v.tags)
      const mid = _tags.Message
      const mtx = await this.mem.getTx(mid)
      return {
        cursor: v,
        node: {
          message: {
            id: mtx.id,
            tags: mtx.tags,
            owner: this.mem.addrmap[mtx.owner],
            anchor: mtx.anchor ?? null,
            target: pid,
            signature: mtx.signature,
            data: tx.data,
          },
          assignment: {
            id: tx.id,
            tags: tx.tags,
            owner: this.mem.addrmap[tx.owner],
            anchor: tx.anchor ?? null,
            target: null,
            signature: tx.signature,
          },
        },
      }
    })(reverse(this.mem.env[pid]?.results ?? [])) // need mod
    return { json: { page_info: { has_next_page: false }, edges } }
  }

  async mu_get_root({ query, params, body, headers, method }) {
    return { send: "ao messenger unit" }
  }

  async mu_post_root({ query, params, body, headers, method }) {
    let valid = await DataItem.verify(body)
    let type = null
    let item = null
    if (valid) item = new DataItem(body)
    const _tags = tags(item.tags)
    let err = null
    if (_tags.Type === "Process") {
      const res = await this.spawn({
        item,
        module: _tags.Module,
        scheduler: _tags.Scheduler,
      })
      if (!res) err = "bad requrest"
    } else if (_tags.Type === "Message") {
      await this.message({ item, process: item.target })
    } else err = true

    if (err) return { status: 400, error: err }
    else return { json: { id: item.id } }
  }

  async mu_post_monitor({ query, params, body, headers, method }) {
    await this.monitor({ process: params.pid })
    return { json: { id: params.pid, messag: "cron monitored!" } }
  }
  async mu_delete_monitor({ query, params, body, headers, method }) {
    await this.unmonitor({ process: params.pid })
    return { json: { id: params.pid, message: "cron deleted!" } }
  }

  async ar_get_root({ query, params, body, headers, method }) {
    return {
      json: {
        version: 1,
        timestamp: Date.now(),
        height: this.mem.height,
        network: "wao.LN.1",
        current: this.mem.getAnchor(),
      },
    }
  }
  async ar_get_wallet_balance({ query, params, body, headers, method }) {
    return { send: "0" }
  }
  async ar_get_mint({ query, params, body, headers, method }) {
    return { json: { id: "0" } }
  }
  async ar_get_tx_offset({ query, params, body, headers, method }) {
    return { status: 400, send: null }
  }
  async ar_get_tx_anchor({ query, params, body, headers, method }) {
    return { send: this.mem.getAnchor() }
  }
  async ar_get_mine({ query, params, body, headers, method }) {
    return { json: params }
  }
  async ar_get_id({ query, params, body, headers, method }) {
    const _data = await this._ar.data(params.id)
    if (!_data) return { status: 404, send: null }
    else return { send: Buffer.from(_data, "base64") }
  }
  async ar_get_price({ query, params, body, headers, method }) {
    return { send: "0" }
  }
  async ar_post_graphql({ query, params, body, headers, method }) {
    try {
      const { query, variables } = body
      const { tar, args } = toGraphObj({ query, variables })
      let res2 = null
      if (tar === "transactions") {
        res2 = await this.gql.txs({ ...args })
      } else if (tar === "blocks") {
        res2 = await this.gql.blocks({ ...args })
      }
      const edges = map(v => ({ node: v, cursor: v.cursor }), res2)
      return {
        json: {
          data: { transactions: { pageInfo: { hasNextPage: true }, edges } },
        },
      }
    } catch (e) {
      console.log(e)
      return { status: 400, error: "bad request" }
    }
  }
  async ar_post_id({ query, params, body, headers, method }) {
    // id = "tx" | "chunk"
    if (body.chunk) {
      if (this.data[body.data_root]) {
        this.data[body.data_root].data += body.chunk
        const buf = Buffer.from(body.chunk, "base64")
        if (!this.data[body.data_root].chunks) {
          this.data[body.data_root].chunks = buf
        } else {
          this.data[body.data_root].chunks = Buffer.concat([
            this.data[body.data_root].chunks,
            buf,
          ])
        }
        delete body.chunk
        if (
          this.data[body.data_root].data_size <=
          this.data[body.data_root].chunks.length
        ) {
          this.data[body.data_root].data =
            this.data[body.data_root].chunks.toString("base64")
          await this._ar.postTx(this.data[body.data_root])
          delete this.data[body.data_root]
        }
      }
      return { json: { id: body.id } }
    } else {
      if (body.data_root && body.data === "") {
        this.data[body.data_root] = body
      } else {
        await this._ar.postTx(body)
      }
      return { json: { id: body.id } }
    }
  }
}
export default Adaptor
