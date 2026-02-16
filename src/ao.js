import * as WarpArBundles from "warp-arbundles"
const pkg = WarpArBundles.default ?? WarpArBundles
const { createData, ArweaveSigner } = pkg
import AR from "./ar.js"
import HB from "./hb.js"
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
} from "@permaweb/aoconnect-69"

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
  last,
} from "ramda"

import {
  allChecked,
  searchTag,
  checkTag,
  wait,
  isLocalhost,
  tag,
  tags as _tags,
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

const getHash = res =>
  md5(
    JSON.stringify({
      Messages: res.Messages ?? [],
      Spawns: res.Spawns ?? [],
      Assignments: res.Assignments ?? [],
    })
  )

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
      hb: _hb,
      mode,
      authority = srcs.authority,
      module,
      module_type = "aos2",
      scheduler = srcs.scheduler,
      aoconnect,
      ar,
      in_memory = false,
      port,
    } = opt
    if (_hb) {
      this.format = _hb === "ans104" ? _hb : "httpsig"
      const _hbOpt = { format: this.format }
      if (typeof _hb === "string" && _hb !== "ans104") _hbOpt.url = _hb
      this.hb = new HB(_hbOpt)
      this.mode = mode ?? "legacy"
    }
    if (!_port && port) _port = port
    if (!aoconnect && _port) aoconnect = optAO(_port)
    if (!ar && _port) ar = { port: _port }
    this.wao = opt.wao
    this.variant = opt.variant
    this.module_type = module_type
    if (isNil(this.wao)) this.wao = in_memory || !isNil(ar?.port)
    if (!module) {
      switch (module_type) {
        case "sqlite":
          module = srcs.module_sqlite
          break
        case "aos2":
          module = srcs.module_aos2
          break
        case "mainnet":
          module = srcs.module_mainnet
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
      this.module = module ?? srcs.module_wao
      this.scheduler = srcs.scheduler_wao
      this.authority = srcs.authority_wao
    } else if (this.module_type === "mainnet") {
      this.module = module ?? srcs.module_mainnet
    } else {
      this.module = module
      this.scheduler = scheduler
      this.authority = authority
    }
  }

  async init(jwk) {
    await this.ar.init(jwk)
    if (this.hb) await this.hb.init(this.ar.jwk)
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
      "Data-Protocol": this.protocol ?? "ao",
      Variant:
        (this.variant ?? this.module_type === "mainnet")
          ? "ao.TN.1"
          : "ao.TN.1",
      Type: "Module",
      "Module-Format": "wasm64-unknown-emscripten-draft_2024_02_15",
      "Input-Encoding": "JSON-1",
      "Output-Encoding": "JSON-1",
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
      "Data-Protocol": this.protocol ?? "ao",
      Variant:
        (this.variant ?? this.module_type === "mainnet")
          ? "ao.TN.1"
          : "ao.TN.1",
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
    module,
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
      if (this.hb) {
        switch (this.mode) {
          case "aos":
          case "wasm":
            ;({ pid } = await this.hb.spawnAOS({ module }))
            break
          case "lua":
            ;({ pid } = await this.hb.spawnLua())
            break
          default:
            ;({ pid } = await this.hb.spawnLegacy({ module, tags, data }))
            try {
              await this.hb.computeLegacy({ pid, slot: 0 })
            } catch (_e) {}
        }
      } else {
        module ??= this.module
        if (!tags.Authority && this.authority) tags.Authority = this.authority
        let _tags = buildTags(null, tags)
        pid = await this.spawn({
          variant: this.variant,
          memory,
          module,
          scheduler,
          signer: this.toSigner(jwk),
          tags: _tags,
          data: jsonToStr(data),
        })
      }
    } catch (e) {
      err = e
    }
    const p = pid ? this.p(pid) : null
    return { err, pid, p }
  }

  async res({
    jwk,
    pid,
    mid,
    check = [],
    get,
    timeout = 5000,
    mode = "aoconnect",
    limit = 25,
  }) {
    let err = null
    let hash = null
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
        const _ress = await this.ress({ pid, limit })
        for (let v of reverse(_ress?.out ?? [])) {
          const hash2 = getHash(v)
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
          const res = await this.result({ process: pid, message: v.id })
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
    let start = Date.now()
    try {
      if (!is(Array, check)) check = [check]
      let _txs = [{ id: mid, from: await this.ar.toAddr(jwk) }]
      if (this.hb) {
        try {
          switch (this.mode) {
            case "aos":
            case "wasm":
              res = await this.hb.computeAOS({ pid, slot: mid })
              try {
                const { slot: pushSlot, outbox: pushOutbox } =
                  await this.hb.pushAOS({ pid, slot: mid, outbox: res })
                if (pushSlot > Number(mid) && pushOutbox) {
                  res = pushOutbox
                }
              } catch (_e2) {}
              break
            case "lua":
              res = await this.hb.computeLua({ pid, slot: mid })
              try {
                await this.hb.get({ path: `/${pid}/push`, slot: mid })
              } catch (_e2) {}
              break
            default:
              res = await this.hb.computeLegacy({ pid, slot: mid })
              try {
                await this.hb.get({ path: `/${pid}/push`, slot: mid })
              } catch (_e2) {}
              const _callerAddr = this.hb.addr
              const _hasCallerReply = res?.Messages?.some(
                m =>
                  m?.Data != null &&
                  m?.Data !== "" &&
                  m?.Target === _callerAddr,
              )
              if (!_hasCallerReply) {
                try {
                  const raw = await this.hb.getJSON({
                    path: `/${pid}/now/results/json/body`,
                  })
                  const parsed =
                    typeof raw === "string" ? JSON.parse(raw) : raw
                  if (parsed?.Messages || parsed?.Output) res = parsed
                } catch (_e2) {}
              }
          }
          if (res) res = this._normalizeHBResult(res)
        } catch (_e) {}
      } else {
        res = await this.result({ process: pid, message: mid })
      }
      hash = getHash(res)
      let _txmap = { [mid]: { checked: false, res } }
      results.push({ mid, res })
      let checked = false
      if (this.hb && !isNil(check) && check.length > 0) {
        // HB mode: push device handles recursive coroutine resolution.
        // After push resolves, poll now/results for the final result.
        const nowPath =
          this.mode === "aos" || this.mode === "wasm"
            ? `/${pid}/now/results/outbox`
            : this.mode === "lua"
              ? `/${pid}/now/results`
              : `/${pid}/now/results/json/body`
        checked = res ? allChecked(check, res) : false
        while (!checked && Date.now() - start < timeout) {
          try {
            let parsed
            if (this.mode === "legacy" || !this.mode) {
              const raw = await this.hb.getJSON({ path: nowPath })
              parsed = typeof raw === "string" ? JSON.parse(raw) : raw
            } else {
              const nowRes = await this.hb.get({ path: nowPath })
              parsed = this._parseHBOutbox(nowRes)
            }
            if (parsed) {
              const nextRes = this._normalizeHBResult(parsed)
              if (nextRes) {
                res = nextRes
                results.push({ mid: "now", res: nextRes })
                checked = allChecked(check, nextRes)
              }
            }
          } catch (_e) {}
          if (!checked) await wait(1000)
        }
        if (checked && !isNil(get)) out = getTagVal(get, res)
        if (!checked) err = "something went wrong!"
      } else {
        do {
          for (let v of _txs) {
            if (!_txmap[v.id].checked) {
              _txmap[v.id].checked = true
              if (isNil(_txmap[v.id].res)) {
                try {
                  const _res = await this.result({
                    process: pid,
                    message: v.id,
                  })
                  _txmap[v.id].res = _res
                } catch (_e) {}
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
          try {
            await getNewTxs(pid, _txs, _txmap)
          } catch (_e) {}
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
      }
    } catch (e) {
      err = e
    }
    return { mid, res, err, out, results }
  }

  async msg({
    pid,
    jwk,
    data,
    act = "Eval",
    tags = {},
    check = [],
    get,
    timeout = 5000,
    mode = "aoconnect",
    limit = 25,
  }) {
    let err
    ;({ jwk, err } = await this.ar.checkWallet({ jwk }))
    if (err) return { err }
    if (this.hb) {
      let mid, res = null
      const schedArgs = { pid, tags: tags ?? {}, action: act, data }
      switch (this.mode) {
        case "aos":
        case "wasm":
          mid = (await this.hb.scheduleAOS(schedArgs)).slot
          try {
            res = await this.hb.computeAOS({ pid, slot: mid })
            try {
              const { slot: pushSlot, outbox: pushOutbox } =
                await this.hb.pushAOS({ pid, slot: mid, outbox: res })
              if (pushSlot > Number(mid) && pushOutbox) {
                res = pushOutbox
                mid = pushSlot
              }
            } catch (_e2) {}
          } catch (_e) {}
          break
        case "lua":
          mid = (await this.hb.scheduleLua(schedArgs)).slot
          try {
            res = await this.hb.computeLua({ pid, slot: mid })
            try {
              await this.hb.get({ path: `/${pid}/push`, slot: mid })
            } catch (_e2) {}
          } catch (_e) {}
          break
        default:
          mid = (await this.hb.scheduleLegacy(schedArgs)).slot
          try {
            res = await this.hb.computeLegacy({ pid, slot: mid })
            try {
              await this.hb.get({ path: `/${pid}/push`, slot: mid })
            } catch (_e2) {}
            // Push updates now/ to its recursive resolution result, which may
            // include errors from other processes (cross-process trust errors).
            // Only use now/results when the compute result has no reply to the
            // caller — i.e. the handler used Send().receive() and the reply is
            // deferred until push resolves the coroutine.
            const callerAddr = this.hb.addr
            const hasCallerReply = res?.Messages?.some(
              m =>
                m?.Data != null &&
                m?.Data !== "" &&
                m?.Target === callerAddr,
            )
            if (!hasCallerReply) {
              try {
                const raw = await this.hb.getJSON({
                  path: `/${pid}/now/results/json/body`,
                })
                const parsed =
                  typeof raw === "string" ? JSON.parse(raw) : raw
                if (parsed?.Messages || parsed?.Output) res = parsed
              } catch (_e2) {}
            }
          } catch (_e) {}
      }
      if (res) res = this._normalizeHBResult(res)
      if (!isNil(check) && is(Array, check) ? check.length > 0 : true) {
        return await this.res({ pid, mid, jwk, check, get, timeout, mode, limit })
      }
      let out = null
      if (res && !isNil(get)) out = getTagVal(get, res)
      return { mid, res, err: null, out, results: [{ mid, res }] }
    } else {
      let _tags = buildTags(act, tags)
      const mid = await this.message({
        variant: this.variant,
        process: pid,
        signer: this.toSigner(jwk),
        tags: _tags,
        data: jsonToStr(data),
      })
      return await this.res({
        pid,
        mid,
        jwk,
        check,
        get,
        timeout,
        mode,
        limit,
      })
    }
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
      res = await this.result({ process: pid, message: mid })
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
      if (this.hb) {
        res = await this.hb.dryrun({
          action: act,
          pid,
          tags,
          data: jsonToStr(data),
        })
      } else {
        res = await this.dryrun({
          process: pid,
          signer: this.toSigner(jwk),
          tags: _tags,
          data: jsonToStr(data),
        })
      }
      let checks = []
      if (!is(Array, check)) check = [check]
      if (!allChecked(check, res)) err = "something went wrong"
      if (!err && !isNil(get)) out = getTagVal(get, res)
    } catch (e) {
      err = e
    }
    return { res, err, out }
  }

  async ress({ pid, limit, asc, from }) {
    let err = null
    let msgs = null
    let next = null
    let res = null
    try {
      let sort = asc ? "ASC" : "DESC"
      if (this.hb) {
        res = await this.hb.results({ process: pid, limit, sort, from })
      } else {
        res = await this.results({ process: pid, limit, sort, from })
      }
      if (!res.edges) return null
      msgs = map(v => ({ cursor: v.cursor, ...v.node }))(res.edges)
      if (res.page_info.has_next_page) {
        next = async () => {
          return await this.results({
            process: pid,
            limit,
            sort,
            from: last(res.edges).cursor,
          })
        }
      }
    } catch (e) {
      err = e
    }
    return { err, out: msgs, res }
  }

  async eval({ pid, jwk, data }) {
    const fns = [
      {
        args: { pid, data, act: "Eval" },
        err: ({ res }) => {
          if (this.hb) return false
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
          text.replace(/'/g, "\\'")
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
    if (this.hb) return { err: null, pid }
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
  } = {}) {
    let [fns, isBoot] = [[], false]
    // AOS/Lua HB modes don't support On-Boot in spawn, so always load via eval
    const hbNonLegacy = this.hb && this.mode && this.mode !== "legacy"
    if (boot === true && !data && !hbNonLegacy) {
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

  async var({ pid, data, json = true, pretty = false }) {
    if (json) {
      const _var = (
        await this.dry({ data: `require("json").encode(${data})`, pid })
      )?.res?.Output?.data
      if (_var) {
        try {
          return JSON.parse(strip(_var))
        } catch (e) {
          return strip(_var)
        }
      }
      return _var
    } else {
      const _var = (await this.dry({ data, pid }))?.res?.Output?.data
      return pretty ? _var : strip(_var)
    }
  }
  // Find the deepest slot from a push response's nested resulted-in structure.
  // Push resolves coroutines by scheduling responses to progressively deeper slots.
  _findDeepestSlot(pushOut, initial) {
    let max = initial
    const search = obj => {
      if (!obj || typeof obj !== "object") return
      if (obj.slot != null && Number(obj.slot) > max) max = Number(obj.slot)
      if (obj["resulted-in"]) search(obj["resulted-in"])
      for (const k of Object.keys(obj)) {
        if (/^\d+$/.test(k)) search(obj[k])
      }
    }
    search(pushOut)
    return max
  }

  // Parse HB outbox from either a structured object or raw multipart response.
  // Returns a numbered-key outbox object compatible with _normalizeHBResult,
  // or null if parsing fails.
  _parseHBOutbox(hbRes) {
    if (!hbRes) return null
    const out = hbRes.out
    // Already a structured object with numbered keys
    if (out && typeof out === "object" && !Array.isArray(out)) {
      const numKeys = Object.keys(out).filter(k => /^\d+$/.test(k))
      if (numKeys.length > 0) return out
    }
    // Raw multipart string — extract messages from part headers
    const raw = typeof out === "string" ? out : hbRes.body
    if (!raw || typeof raw !== "string") return null
    const messages = {}
    // Split on multipart boundary
    const parts = raw.split(/--[A-Za-z0-9_-]+/)
    for (const part of parts) {
      if (!part?.trim()) continue
      // Match parts named "body/N", "body/outbox/N", or just "N"
      const nameMatch = part.match(
        /content-disposition:\s*(?:form-data|inline)\s*;?\s*name="?(?:body\/)?(?:outbox\/)?(\d+)"?/i,
      )
      if (!nameMatch) continue
      const idx = nameMatch[1]
      if (messages[idx]) continue // skip sub-parts like Tags
      // Extract Data from header line
      const dataMatch = part.match(/[\r\n]Data:\s*(.*?)(?:\r?\n|$)/i)
      if (dataMatch) {
        messages[idx] = {
          data: dataMatch[1].trim(),
          Tags: [],
        }
      }
    }
    if (Object.keys(messages).length > 0) return messages
    return null
  }

  _normalizeHBResult(res) {
    if (!res || this.mode === "legacy" || !this.mode) return res
    // After push, compute results may come back as raw multipart strings.
    // Parse them into numbered-key objects before extracting messages.
    if (typeof res === "string") {
      const parsed = this._parseHBOutbox({ out: res, body: res })
      if (parsed) res = parsed
      else return { Messages: [], Output: { data: "" }, Spawns: [] }
    }
    let messages = []
    if (this.mode === "aos" || this.mode === "wasm") {
      const keys = Object.keys(res)
        .filter(k => /^\d+$/.test(k))
        .sort((a, b) => Number(a) - Number(b))
      messages = keys.map(k => {
        const m = res[k]
        return { Data: m?.data ?? m?.Data ?? "", Tags: [] }
      })
    } else if (this.mode === "lua") {
      // Handle both array format (from computeLua) and numbered-key format
      // (from _parseHBOutbox after push resolution).
      const numKeys = Object.keys(res)
        .filter(k => /^\d+$/.test(k))
        .sort((a, b) => Number(a) - Number(b))
      if (numKeys.length > 0) {
        messages = numKeys.map(k => {
          const m = res[k]
          return { Data: m?.data ?? m?.Data ?? "", Tags: [] }
        })
      } else {
        const outbox = Array.isArray(res) ? res : res?.outbox ?? []
        messages = (Array.isArray(outbox) ? outbox : []).map(m => ({
          Data: m?.data ?? m?.Data ?? "",
          Tags: [],
        }))
      }
    }
    return { Messages: messages, Output: { data: "" }, Spawns: [] }
  }

  p(pid) {
    return new Process(pid, this)
  }
}

function strip(str) {
  return str.replace(/\x1B\[[0-9;]*[JKmsu]/g, "")
}

const getParams = (tags, opts) => {
  let _tags = tags
  let _opts = opts

  if (
    (!isNil(tags?.get) ||
      !isNil(tags?.check) ||
      !isNil(tags?.jwk) ||
      !isNil(tags?.data) ||
      is(Boolean, tags) ||
      is(String, tags)) &&
    isNil(opts)
  ) {
    _opts = tags
    _tags = {}
  }

  if (isNil(_opts)) _opts = { get: false }
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
  async res(opts) {
    const { _opts } = getParams(null, opts)
    return await this.ao.res({ pid: this.pid, ..._opts })
  }
  async o(name, opt) {
    const res = await this[name](...opt)
    if (res.err) throw Error(res.err)
    return res.out
  }
  async m(...opt) {
    return this.o("msg", opt)
  }
  async d(...opt) {
    return this.o("dry", opt)
  }
  async v(data, json = true, pretty = false) {
    return await this.ao.var({ pid: this.pid, data, json, pretty })
  }
  async r(...opt) {
    return this.o("res", opt)
  }
}

export default AO
