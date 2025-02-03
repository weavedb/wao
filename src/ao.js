import * as WarpArBundles from "warp-arbundles"
const pkg = WarpArBundles.default ?? WarpArBundles
const { createData, ArweaveSigner } = pkg
import AR from "./ar.js"
import md5 from "md5"
import {
  createDataItemSigner,
  connect,
  assign,
  result,
  results,
  message,
  spawn,
  dryrun,
  monitor,
  unmonitor,
} from "@permaweb/aoconnect"

import {
  dissoc,
  equals,
  concat,
  is,
  mergeLeft,
  o,
  uniqBy,
  prop,
  isNil,
  includes,
  map,
  reverse,
} from "ramda"

import {
  allChecked,
  searchTag,
  checkTag,
  wait,
  isLocalhost,
  tag,
  ltags,
  action,
  isData,
  getTagVal,
  srcs,
  buildTags,
  isRegExp,
  mergeChecks,
  isCheckComplete,
  mergeOut,
  isOutComplete,
  jsonToStr,
  optAO,
} from "./utils.js"

function createDataItemSigner2(wallet) {
  const signer = async ({ data, tags, target, anchor }) => {
    const signer = new ArweaveSigner(wallet)
    const dataItem = createData(data, signer, { tags, target, anchor })
    const sig = dataItem.sign(signer).then(async () => {
      return { id: await dataItem.id, raw: await dataItem.getRaw() }
    })
    return sig
  }
  return signer
}

class AO {
  constructor(opt = {}) {
    let _port = null
    if (typeof opt === "number") {
      _port = opt
      opt = {}
    }
    let {
      authority = srcs.authority,
      module,
      module_type = "aos2",
      scheduler = srcs.scheduler,
      aoconnect,
      ar,
      in_memory = false,
      port,
    } = opt
    if (!_port && port) _port = port
    if (!aoconnect && _port) aoconnect = optAO(_port)
    if (!ar && _port) ar = { port: _port }
    this.wao = opt.wao
    if (isNil(this.wao)) this.wao = in_memory || !isNil(ar?.port)
    if (!module) {
      switch (module_type) {
        case "sqlite":
          module = srcs.module_sqlite
          break
        case "aos2":
          module = srcs.module_aos2
          break
        default:
          module = srcs.module
      }
    }
    this.__type__ = "ao"
    if (ar?.__type__ === "ar") this.ar = ar
    else {
      let _ar = typeof ar === "object" ? ar : {}
      this.ar = new AR(_ar)
    }

    if (!in_memory && aoconnect) {
      const {
        results,
        assign,
        result,
        message,
        spawn,
        dryrun,
        monitor,
        unmonitor,
      } = connect(aoconnect)
      this.assign = assign
      this.result = result
      this.results = results
      this.message = message
      this.spawn = spawn
      this.dryrun = dryrun
      this.monitor = monitor
      this.unmonitor = unmonitor
    } else {
      this.assign = assign
      this.result = result
      this.results = results
      this.message = message
      this.spawn = spawn
      this.dryrun = dryrun
      this.monitor = monitor
      this.unmonitor = unmonitor
    }
    if (this.wao) {
      this.module = srcs.module_wao
      this.scheduler = srcs.scheduler_wao
      this.authority = srcs.authority_wao
    } else {
      this.module = module
      this.scheduler = scheduler
      this.authority = authority
    }
  }

  async init(jwk) {
    await this.ar.init(jwk)
    return this
  }

  toSigner(wallet) {
    if (wallet?.n && typeof window !== "undefined") {
      return createDataItemSigner2(wallet)
    } else if (wallet.test) {
      return createDataItemSigner(wallet.jwk)
    } else {
      return createDataItemSigner(wallet)
    }
  }

  async pipe({ jwk, fns = [], cb }) {
    let [_, out, res, i] = [{}, {}, [], 0]
    let nextArgs = fns[0].args ?? {}
    let [ret, err, pid, mid, id] = [null, null, null, null, null]
    if (!jwk) fns.unshift({ fn: "checkWallet" })
    const copy = ({ _, inp, out, args, from, to, pid, mid, id }) => {
      const _from = from.split(".")
      const _to = to.split(".")
      let [field, val, target] = [null, null, null]
      if (_to[0] === "_") {
        target = _
        field = _to.slice(1).join(".")
      } else if (_to[0] === "args") {
        target = args
        field = _to.slice(1).join(".")
      } else if (_to[0] === "out") {
        target = out
        field = _to.slice(1).join(".")
      } else {
        target = out
        field = _to.join(".")
      }

      if (from === "inp") val = inp
      else if (from === "mid") val = mid
      else if (from === "id") val = id
      else if (from === "pid") val = pid
      else if (_from[0] === "args") val = args[_from.slice(1).join(".")] ?? null
      else if (_from[0] === "out") val = out[_from.slice(1).join(".")] ?? null
      else if (_from[0] === "inp") val = inp[_from.slice(1).join(".")] ?? null
      else val = inp[from] ?? null

      if (target) target[field] = val
    }
    const binds = {
      post: this.ar,
      bundle: this.ar,
      postTx: this.ar,
      checkWallet: this.ar,
      transfer: this.ar,
    }
    for (let v of fns) {
      let bind = null
      if (typeof v.fn === "string" && !v.bind) bind = binds[v.fn] ?? this
      else bind = v.bind ?? this

      const name =
        (v.name ?? typeof !v.fn)
          ? "msg"
          : typeof v.fn === "string"
            ? v.fn
            : null
      const fn = typeof v.fn === "string" ? bind[v.fn] : (v.fn ?? this.msg)

      const _fn = fn.bind(bind)
      const _res = await _fn({ jwk, ...nextArgs })
      res.push(_res)
      if (_res.err) {
        err = _res.err
        break
      }
      if (typeof v.err === "function") {
        const _err = await v.err({
          name,
          _,
          jwk,
          res: _res.res,
          args: nextArgs,
          out,
          pid,
          inp: _res.out,
          mid,
          id,
        })
        if (_err) {
          err = _err
          break
        }
      }
      if (_res.pid) pid = _res.pid
      if (_res.mid) mid = _res.mid
      if (_res.id) id = _res.id
      if (_res.jwk) jwk = _res.jwk
      nextArgs = fns[i + 1]?.args ?? {}
      if (typeof v.then === "function") {
        const _ret = await v.then({
          name,
          _,
          jwk,
          res: _res.res,
          args: nextArgs,
          out,
          pid,
          inp: _res.out,
          mid,
          id,
        })
        if (typeof _ret !== "undefined") {
          ret = _ret
          break
        }
      } else if (is(Object, v.then)) {
        for (const k in v.then) {
          copy({
            name,
            _,
            inp: _res.out,
            out,
            args: nextArgs,
            pid,
            mid,
            from: v.then[k],
            to: k,
            id,
          })
        }
      }
      if (typeof cb === "function") {
        cb({
          name,
          fns,
          i: i + 1,
          _,
          jwk,
          res: _res.res,
          args: nextArgs,
          out,
          pid,
          inp: _res.out,
          mid,
          id,
        })
      }
      i++
    }
    return ret ?? { jwk, err, res, out, pid, mid, id, ..._ }
  }

  async postModule({ data, jwk, tags = {}, overwrite = false }) {
    const _tags = mergeLeft(tags, {
      "Data-Protocol": "ao",
      Variant: "ao.TN.1",
      Type: "Module",
      "Module-Format": "wasm64-unknown-emscripten-draft_2024_02_15",
      "Input-Encoding": "JSON-V1",
      "Output-Encoding": "JSON-V1",
      "Memory-Limit": "1-gb",
      "Compute-Limit": "9000000000000",
      Extension: "WeaveDrive",
    })

    const fns = [
      {
        fn: "post",
        args: { data, tags: _tags },
        then: ({ id }) => {
          if (!this.module || overwrite) this.module = id
        },
      },
    ]
    return await this.pipe({ jwk, fns })
  }

  async postScheduler({ jwk, url, tags = {}, overwrite = false }) {
    const _tags = mergeLeft(tags, {
      "Data-Protocol": "ao",
      Variant: "ao.TN.1",
      Type: "Scheduler-Location",
      Url: url,
      "Time-To-Live": "3600000",
    })
    const then = async ({ jwk, out, _ }) => {
      _.scheduler = await this.ar.toAddr(jwk)
      if (!this.scheduler || overwrite) this.scheduler = _.scheduler
    }
    const fns = [{ fn: "post", args: { tags: _tags }, then }]
    return await this.pipe({ jwk, fns })
  }

  async spwn({
    boot,
    module = this.module,
    scheduler = this.scheduler,
    memory,
    jwk,
    tags = {},
    data,
    auth,
  } = {}) {
    let err = null
    ;({ jwk, err } = await this.ar.checkWallet({ jwk }))
    if (err) return { err }
    let pid = null
    try {
      if (boot) tags["On-Boot"] = boot
      if (auth) tags.Authority = auth
      if (!tags.Authority && this.authority) tags.Authority = this.authority
      let _tags = buildTags(null, tags)
      pid = await this.spawn({
        memory,
        module,
        scheduler,
        signer: this.toSigner(jwk),
        tags: _tags,
        data: jsonToStr(data),
      })
    } catch (e) {
      err = e
    }
    const p = pid ? this.p(pid) : null
    return { err, pid, p }
  }

  async msg({
    pid,
    jwk,
    data,
    act = "Eval",
    tags = {},
    check = [],
    get,
    timeout = 0,
    mode = "aoconnect",
    limit = 25,
  }) {
    let err = null
    ;({ jwk, err } = await this.ar.checkWallet({ jwk }))
    if (err) return { err }
    let anchors = {}
    let hash = null
    let mid = null
    const getNewTxs = async (pid, _txs, _txmap) => {
      let exists = false
      if (mode === "gql") {
        const txs = await this.ar.gql.txs({
          recipient: pid,
          limit,
          fields: ["id", "recipient", "tags", { owner: ["address"] }],
        })
        for (const v of reverse(txs)) {
          if (v.id === mid) exists = true
          else if (exists) {
            if (isNil(_txmap[v.id])) {
              const t = ltags(v.tags)
              if (t.type === "Message") {
                v.from = t["from-process"] ?? v.owner?.address
                _txs.unshift(v)
                _txmap[v.id] = { checked: false, ref: t["x-reference"] }
              }
            }
          }
        }
      } else {
        for (let v of reverse(await this.res({ pid, limit }))) {
          const hash2 = md5(JSON.stringify(dissoc("cursor", v)))
          if (!exists) {
            if (hash2 === hash) exists = true
          } else {
            if (isNil(_txmap[v.cursor + pid])) {
              _txmap[v.cursor + pid] = { checked: false, res: v }
              _txs.unshift({ id: v.cursor + pid })
            }
          }
        }
      }
    }
    const checkOut = async (get, _txs, _txmap, out) => {
      for (let v of _txs) {
        if (isNil(_txmap[v.id].res)) {
          const res = await this.res({ pid, mid: v.id })
          if (!hash) console.log(res)
          _txmap[v.id].res = res
        }
        if (!isNil(_txmap[v.id].res) && _txmap[v.id].out !== true) {
          _txmap[v.id].out = true
          const _out = getTagVal(get, _txmap[v.id].res, v.from)
          out = mergeOut(out, _out, get)
          if (isOutComplete(out, get)) break
        }
      }
      return out
    }
    let [res, out, results] = [null, null, []]
    let _tags = buildTags(act, tags)
    let start = Date.now()
    try {
      mid = await this.message({
        process: pid,
        signer: this.toSigner(jwk),
        tags: _tags,
        data: jsonToStr(data),
      })
      if (!is(Array, check)) check = [check]
      let _txs = [{ id: mid, from: await this.ar.toAddr(jwk) }]
      res = await this.res({ pid, mid })
      hash = md5(JSON.stringify(res))
      let _txmap = { [mid]: { checked: false, res } }
      results.push({ mid, res })
      let checked = false
      do {
        for (let v of _txs) {
          if (!_txmap[v.id].checked) {
            _txmap[v.id].checked = true
            if (isNil(_txmap[v.id].res)) {
              const _res = await this.res({ pid, mid: v.id })
              _txmap[v.id].res = _res
            }
            if (!isNil(check) && check.length > 0) {
              checked = allChecked(check, _txmap[v.id].res, v.from)
            } else {
              checked = true
            }
            if (checked) break
          }
        }
        if (checked) break
        await wait(1000)
        await getNewTxs(pid, _txs, _txmap)
      } while (Date.now() - start < timeout)

      if (!checked) {
        err = "something went wrong!"
      } else {
        out = mergeOut(out, await checkOut(get, _txs, _txmap, out), get)
        if (!isOutComplete(out, get) && !isNil(get)) {
          while (Date.now() - start < timeout) {
            await getNewTxs(pid, _txs, _txmap)
            out = mergeOut(out, await checkOut(get, _txs, _txmap, out), get)
            if (isOutComplete(out, get)) break
            await wait(1000)
          }
        }
      }
    } catch (e) {
      err = e
    }
    return { mid, res, err, out, results }
  }

  async asgn({ pid, mid, jwk, check = [], get }) {
    let err = null
    ;({ jwk, err } = await this.ar.checkWallet({ jwk }))
    if (err) return { err }

    let [res, out] = [null, null]
    try {
      mid = await this.assign({
        process: pid,
        message: mid,
        signer: this.toSigner(jwk),
      })
      res = await this.res({ pid, mid })
      if (!res) err = "something went wrong"
      if (res.Error) err = res.Error
      else {
        if (!is(Array, check)) check = [check]
        if (!allChecked(check, res)) err = "something went wrong"
        if (!err && !isNil(get)) out = getTagVal(get, res) // todo: from
      }
    } catch (e) {
      err = e
    }
    return { mid, res, err, out }
  }

  async dry({ pid, jwk, data, act = "Eval", tags = {}, check = [], get }) {
    let err = null
    ;({ jwk, err } = await this.ar.checkWallet({ jwk }))
    if (err) return { err }
    let [res, out] = [null, null]
    let _tags = buildTags(act, tags)
    try {
      const _res = await this.dryrun({
        process: pid,
        signer: this.toSigner(jwk),
        tags: _tags,
        data: jsonToStr(data),
      })
      res = _res
      let checks = []
      if (!is(Array, check)) check = [check]
      if (!allChecked(check, res)) err = "something went wrong"
      if (!err && !isNil(get)) out = getTagVal(get, res)
    } catch (e) {
      err = e
    }
    return { res, err, out }
  }
  async res({ pid, mid, limit, asc, from, to }) {
    if (!mid) {
      let sort = asc ? "ASC" : "DESC"
      const res = await this.results({ process: pid, limit, sort, from, to })
      if (!res.edges) return null
      return map(v => ({ cursor: v.cursor, ...v.node }))(res.edges)
    } else {
      return await this.result({ process: pid, message: mid })
    }
  }
  async eval({ pid, jwk, data }) {
    const fns = [
      {
        args: { pid, data, act: "Eval" },
        err: ({ res }) => {
          return (
            typeof res?.Output?.data !== "object" &&
            !(
              typeof res?.Output?.prompt === "string" &&
              /aos\-/.test(res?.Output?.prompt)
            )
          )
        },
      },
    ]
    return await this.pipe({ jwk, fns })
  }

  async transform({ src, data, fills }) {
    let [err, out] = [null, null]
    let _data = data ?? (await this.ar.data(src, true))

    if (!_data) err = "data doesn't exist"
    else {
      for (const k in fills ?? {}) {
        let text = fills[k]
        if (typeof text === "number") text = Number(text).toString()
        _data = _data.replace(
          new RegExp(`\<${k}\>`, "g"),
          text.replace(/'/g, "\\'"),
        )
      }
      out = _data
    }
    return { err, out }
  }

  async load({ src, data, fills, pid, jwk }) {
    let fns = [
      {
        fn: this.transform,
        args: { src, fills, data },
        then: { "args.data": "inp" },
      },
      { fn: this.eval, args: { pid } },
    ]
    return await this.pipe({ jwk, fns })
  }

  async wait({ pid, attempts = 10 }) {
    let exist = false
    let err = null
    while (attempts > 0) {
      if (!this.in_memory) await wait(1000)
      const { res, err: _err } = await this.dry({ pid, data: "#Inbox" })
      if (res?.error) return { err: res.error, pid }
      if (typeof res?.Output === "object") break
      attempts -= 1
      if (attempts === 0) err = "timeout"
    }
    return { err, pid }
  }
  async attest({ id, jwk, tags }) {
    return await this.ar.post({
      tags: mergeLeft(tags, {
        "Data-Protocol": "ao",
        Type: "Attestation",
        Message: id,
      }),
      jwk,
    })
  }

  async avail({ ids, jwk, tags }) {
    return await this.ar.post({
      tags: mergeLeft(tags, {
        "Data-Protocol": "WeaveDrive",
        Variant: "WeaveDrive.tn.1",
        Type: "Available",
        ID: ids,
      }),
      jwk,
    })
  }

  async deploy({
    boot,
    loads,
    src,
    src_data,
    fills = {},
    module = this.module,
    scheduler = this.scheduler,
    jwk,
    tags = {},
    data,
  }) {
    let [fns, isBoot] = [[], false]
    if (boot === true && !data) {
      isBoot = true
      fns = [
        {
          fn: this.transform,
          args: { src, fills, data: src_data },
          then: { "args.data": "inp" },
        },
        {
          fn: this.spwn,
          args: { boot: "Data", module, scheduler, tags },
          then: { "args.pid": "pid" },
        },
      ]
    } else {
      fns = [
        {
          fn: this.spwn,
          args: { boot, module, scheduler, tags, data },
          then: { "args.pid": "pid" },
        },
      ]
    }
    fns.push({ fn: this.wait, then: { "args.pid": "pid" } })
    let i = 0
    for (const v of !loads
      ? src_data
        ? [{ data: src_data, src, fills }]
        : []
      : loads) {
      if (!isBoot || i !== 0) {
        let args = v
        if (typeof args === "string") args = { data: v }
        fns.push({ fn: this.load, args, then: { "args.pid": "pid" } })
      }
      i++
    }
    let result = await this.pipe({ jwk, fns })
    if (result.pid) result.p = this.p(result.pid)
    return result
  }
  p(pid) {
    return new Process(pid, this)
  }
}

const getParams = (tags, opts) => {
  let _tags = tags
  let _opts = opts

  if (
    (!isNil(tags?.get) ||
      !isNil(tags?.check) ||
      !isNil(tags?.data) ||
      is(Boolean, tags) ||
      is(String, tags)) &&
    isNil(opts)
  ) {
    _opts = tags
    _tags = null
  }

  if (isNil(_opts)) _opts = { get: true }
  else if (is(Boolean, _opts) || is(String, _opts)) _opts = { get: _opts }
  return { _tags, _opts }
}

class Process {
  constructor(pid, ao) {
    this.ao = ao
    this.pid = pid
  }

  async load(opt = {}) {
    return await this.ao.load({ pid: this.pid, ...opt })
  }
  async msg(act, tags, opts) {
    const { _tags, _opts } = getParams(tags, opts)
    return await this.ao.msg({ pid: this.pid, act, tags: _tags, ..._opts })
  }
  async dry(act, tags, opts) {
    const { _tags, _opts } = getParams(tags, opts)
    return await this.ao.dry({ pid: this.pid, act, tags: _tags, ..._opts })
  }
  async m(...opt) {
    const res = await this.msg(...opt)
    if (res.err) throw Error(res.err)
    return res.out
  }
  async d(...opt) {
    const res = await this.dry(...opt)
    if (res.err) throw Error(res.err)
    return res.out
  }
}

export default AO
