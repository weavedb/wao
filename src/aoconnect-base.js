import _Arweave from "arweave"
const Arweave = _Arweave.default ?? _Arweave
const arweave = Arweave.init()
import * as WarpArBundles from "warp-arbundles"
const pkg = WarpArBundles.default ?? WarpArBundles
const { DataItem } = pkg
import crypto from "crypto"
import base64url from "base64url"
import { toAddr, wait } from "./utils.js"
import AO from "./ao.js"
import { connect, createSigner } from "@permaweb/aoconnect"

import {
  tags,
  action,
  tag,
  buildTags,
  isJSON,
  jsonToStr,
  dirname,
} from "./utils.js"
import weavedrive from "./weavedrive.js"
import {
  is,
  clone,
  fromPairs,
  map,
  mergeLeft,
  isNil,
  dissoc,
  o,
  reverse,
  includes,
} from "ramda"

let onRecovery = {}
let ongoing = {}

export default ({ AR, scheduler, mu, su, cu, acc, AoLoader, ArMem } = {}) => {
  return (mem, { cache, log = false, extensions = {}, hb, variant } = {}) => {
    const isMem = mem?.__type__ === "mem"
    if (!isMem) {
      let args = { cache }
      if (mem?.SU_URL) {
        args.scheduler = scheduler
        args.variant = variant
        args = mergeLeft(mem, args)
      }
      mem = new ArMem(args)
    }
    const ar = new AR({ mem, log })
    for (const k in extensions) extensions[k] = new extensions[k](ar).ext
    extensions.WeaveDrive = new weavedrive(ar).ext

    const transform = input => {
      const output = { Tags: [] }
      if (input.Data) output.Data = input.Data
      Object.entries(input).forEach(([key, value]) => {
        if (key !== "Data" && key !== "Tags" && typeof value === "string") {
          output.Tags.push({ name: key, value })
        }
      })
      input.Tags?.forEach(tag => {
        const tagKey = Object.keys(tag)[0]
        const tagValue = tag[tagKey]
        if (typeof tagValue === "string") {
          output.Tags.push({ name: tagKey, value: tagValue })
        }
      })

      return output
    }
    const capitalize = __tags => {
      let cap = {
        authority: "Authority",
        action: "Action",
        "on-boot": "On-Boot",
        scheduler: "Scheduler",
        module: "Module",
      }
      for (let v of __tags) v.name = cap[v.name] ?? v.name
      return __tags
    }

    const genMsg = async (Id, p, data, Tags, from, Owner, dry = false) => {
      if (!dry) p.height += 1
      let __tags = Tags?.length ? Tags : []
      return {
        Id,
        Target: p.id,
        Owner,
        Data: data?.length ? data : "",
        "Block-Height": (await mem.get("height")).toString(),
        Timestamp: Date.now().toString(),
        Module: p.module,
        From: from,
        Cron: false,
        Tags: capitalize(__tags),
      }
    }

    const genEnv = async ({ pid, owner = "", module = "" }) => {
      return {
        Process: {
          Id: pid,
          Tags: capitalize((await mem.getTx(pid))?.tags ?? []),
          Owner: owner,
        },
        Module: {
          Id: module,
          Tags: capitalize((await mem.getTx(module))?.tags ?? []),
        },
      }
    }

    const spawn = async (opt = {}) => {
      if (!opt.module) throw Error("module missing")
      if (!opt.scheduler) throw Error("scheduler missing")
      const { mod, wasm, format } = await mem.getWasm(opt.module, mem)
      let id, owner, item, __tags
      let msg_owner = mu.addr
      if (ar.isHttpMsg(opt.http_msg)) {
        id = opt.id ?? opt.http_msg.target
        if (ongoing[id]) {
          let i = 0
          while (i < 30 && ongoing[id]) {
            await wait(100)
            i++
          }
          return id
        } else {
          ongoing[id] = true
          ;({ owner, item, tags: __tags } = await ar.httpmsg(opt.http_msg, id))
          opt.tags = buildTags(null, __tags)
          opt.data = opt.http_msg.data
          msg_owner = owner
        }
      } else {
        opt.tags = buildTags(
          null,
          mergeLeft(tags(opt.tags ?? []), {
            "Data-Protocol": "ao",
            Variant: variant ?? "ao.TN.1",
            Type: "Process",
            Module: mod,
            Scheduler: opt.scheduler,
            "Content-Type": "text/plain",
            Authority: mu.addr,
          })
        )
        let ex = false
        for (let v of opt.tags) if (v.name === "Type") ex = true
        if (!ex) opt.tags.push({ name: "Type", value: "Process" })
        if (opt.for) opt.tags.push({ name: "Pushed-For", value: opt.for })
        ;({
          id,
          owner,
          item,
          tags: __tags,
        } = await ar.dataitem({
          item: opt.item,
          data: opt.data,
          signer: opt.signer,
          tags: tags(opt.tags),
        }))
        opt.tags = buildTags(null, __tags)
        if (opt.item) opt.data = base64url.decode(item.data)
        await ar.postItems(item, su.jwk)
      }

      const now = Date.now
      const t = tags(opt.tags)
      const ext = t.Extension || "WeaveDrive"
      const wdrive = extensions[ext]
      let handle = null
      try {
        handle = await AoLoader(wasm, {
          format,
          WeaveDrive: wdrive,
          spawn: item, // todo: httpmsg
          module: await mem.getTx(mod), // todo: httpmsg
        })
      } catch (e) {
        console.log(mod, e)
      }
      if (!handle) return null
      let _module = null
      _module = { handle, id: mod }
      Date.now = now
      const _tags = tags(opt.tags)
      let res = null
      let memory = opt.memory ?? null
      let p = {
        extension: ext,
        format,
        id: id,
        epochs: [],
        handle: _module.handle,
        module: _module.id,
        hash: id,
        memory,
        owner,
        height: 0,
        results: [id],
      }
      let __msg = null
      if (memory) {
        // forking...
      } else if (_tags["On-Boot"] || true) {
        let data = ""
        if (_tags["On-Boot"] === "Data") data = opt.data ?? ""
        else data = (await mem.get("msgs", _tags["On-Boot"]))?.data ?? ""
        let msg = await genMsg(id, p, data, opt.tags, owner, msg_owner, true)
        __msg = msg
        const _env = await genEnv({
          pid: p.id,
          owner: p.owner,
          module: p.module,
        })
        res = await _module.handle(null, msg, _env)
        p.memory = res.Memory
        delete res.Memory
      } else {
        p.height += 1
      }
      const _msg = {
        ...o(dissoc("signer"), dissoc("memory"))(opt),
        res,
        msg: __msg,
      }
      await mem.set(_msg, "msgs", id)
      if (_tags["Cron-Interval"]) {
        let [num, unit] = _tags["Cron-Interval"].split("-")
        let int = 0
        switch (unit.replace(/s$/, "")) {
          case "millisecond":
            int = num
            break
          case "second":
            int = num * 1000
            break
          case "minute":
            int = num * 1000 * 60
            break
          case "hour":
            int = num * 1000 * 60 * 60
            break
          case "day":
            int = num * 1000 * 60 * 60 * 24
            break
          case "month":
            int = num * 1000 * 60 * 60 * 24 * 30
            break
          case "year":
            int = num * 1000 * 60 * 60 * 24 * 365
            break
        }
        let cronTags = []
        for (const k in _tags) {
          if (/^Cron-Tag-/.test(k)) {
            cronTags.push({ name: k.replace(/Cron-Tag-/, ""), value: _tags[k] })
          }
        }
        p.cronTags = cronTags
        p.span = int
      }
      await mem.set(p, "env", id)
      delete ongoing[id]
      return id
    }

    function genHashChain(previousHash, previousMessageId = null) {
      const hasher = crypto.createHash("sha256")
      hasher.update(Buffer.from(previousHash, "base64"))
      if (previousMessageId) {
        hasher.update(Buffer.from(previousMessageId, "base64"))
      }
      return base64url(hasher.digest())
    }

    const assign = async opt => {
      const p = await mem.get("env", opt.process)
      if (!p || !opt.process) return null
      let _opt = await mem.get("msgs", opt.message)
      let hash = p.hash
      try {
        hash = genHashChain(p.hash, opt.message)
      } catch (e) {
        console.log(e)
      }
      p.hash = hash
      opt.tags = buildTags(
        null,
        mergeLeft(tags(opt.tags ?? []), {
          Timestamp: Date.now(),
          Epoch: p.epochs.length,
          Nonce: "0",
          "Data-Protocol": "ao",
          Variant: variant ?? "ao.TN.1",
          Type: "Assignment",
          "Block-Height": await mem.get("height"),
          Process: opt.process,
          Message: opt.message,
          "Hash-Chain": hash,
        })
      )
      p.epochs.push([opt.message])
      const { id, owner, item } = await ar.dataitem({
        data: opt.data,
        signer: opt.signer,
        tags: tags(opt.tags),
        target: opt.process,
      })
      if (opt.message_item) {
        await ar.postItems([opt.message_item, item], su.jwk)
      } else {
        await ar.postItems(item, su.jwk)
      }
      try {
        let data = _opt.data ?? ""
        let _tags = _opt.tags
        let from = _opt.from ?? opt.from ?? owner
        if (_opt.item) {
          data = base64url.decode(_opt.item.data)
          _tags = _opt.item.tags
          const t = tags(_tags)
          if (t["From-Process"]) from = t["From-Process"]
          if (!from) {
            from = await arweave.wallets.jwkToAddress({
              kty: "RSA",
              n: _opt.item.owner,
              e: "AQAB",
            })
          }
        }
        // check: is owner=mu.addr right?
        const _owner = opt.message_item?.owner
          ? toAddr(opt.message_item.owner)
          : mu.addr
        const msg = await genMsg(opt.message, p, data, _tags, from, _owner)
        const _env = await genEnv({
          pid: p.id,
          owner: p.owner,
          module: p.module,
        })
        if (!p.handle) {
          const { format, mod, wasm } = await mem.getWasm(p.module)
          const wdrive = extensions[p.extension]
          p.handle = await AoLoader(wasm, {
            format,
            WeaveDrive: wdrive,
            spawn: (await mem.getTx(p.id))?.item,
            module: await mem.getTx(mod),
          })
          mem.env[opt.process].handle = p.handle
        }
        if (p.compressed) {
          const start = Date.now()
          p.memory = mem.decompress(p.memory, p.original_size)
          p.compressed = false
        }
        const res = await p.handle(p.memory, msg, _env)
        p.memory = res.Memory
        delete res.Memory
        p.results.push(opt.message)
        await mem.set(p, "env", opt.process)
        const _msg = { ...dissoc("signer", _opt), res, msg }
        await mem.set(_msg, "msgs", opt.message)
        for (const v of res.Messages ?? []) {
          if (await mem.get("env", v.Target)) {
            await message({
              for: opt.message,
              process: v.Target,
              tags: v.Tags,
              data: v.Data,
              signer: mu.signer,
              from: opt.process,
              target: v.Target,
            })
          } else {
            const t = tags(v.Tags)
            // this behaviour is different from AOS (temporary hack for remote tests)
            if (t.__HyperBEAM__) {
              let result = null
              let slot = null
              try {
                const fetchHB = async (
                  path,
                  { json = true, params = "" } = {}
                ) => {
                  return await fetch(
                    `${t.__HyperBEAM__}${path}${json ? "/serialize~json@1.0" : ""}${params ? "?" + params : ""}`
                  ).then(r => r[json ? "json" : "text"]())
                }
                const info = await fetchHB("/~meta@1.0/info")
                const { request } = connect({
                  MODE: "mainnet",
                  URL: t.__HyperBEAM__,
                  device: "",
                  signer: createSigner(mu.jwk),
                })
                let _tags = {
                  ...t,
                  "From-Process": opt.process,
                  "Pushed-For": id,
                  method: "POST",
                  path: `/${v.Target}/schedule`,
                  scheduler: info.address,
                }
                ;({ slot } = await request(_tags))
                const res = await fetchHB(`/${v.Target}~process@1.0/compute`, {
                  params: `slot=${slot}`,
                })
                const {
                  results: { data },
                } = res
                result = data
              } catch (e) {
                console.log(e)
              }
              if (result) {
                /*
                await message({
                  for: slot,
                  process: opt.process,
                  tags: buildTags({
                    "X-Reference": t.Reference,
                  }),
                  data: JSON.stringify(result),
                  signer: mu.signer,
                  from: v.Target,
                  target: opt.process,
                })*/
              }
            } else if (t.__SU__) {
              const sch = t.__SU__
              const tar = v.Target
              /*const procs =
                (await fetch(sch).then(r => r.json()))?.Processes ?? []*/
              const ao = await new AO({ port: sch.split(":").pop() - 2 }).init(
                mu.jwk
              )
              try {
                let _tags = {
                  ...t,
                  "From-Process": opt.process,
                  "Pushed-For": id,
                }
                const pr = (await mem.getTx(opt.process))?.tags ?? []
                const module = tags(pr).Module
                if (module) _tags["From-Module"] = module
                const { mid } = await ao.msg({
                  act: _tags["Action"] ?? null,
                  pid: tar,
                  tags: _tags,
                  data: v.Data ?? "",
                })
              } catch (e) {
                console.log(e)
              }
            } else if (t.__Scheduler__) {
              const sch = t.__Scheduler__
              const tar = v.Target
              const procs =
                (await fetch(sch).then(r => r.json()))?.Processes ?? []
              const ao = await new AO({ port: sch.split(":").pop() - 3 }).init(
                mu.jwk
              )
              try {
                let _tags = {
                  ...t,
                  "From-Process": opt.process,
                  "Pushed-For": id,
                }
                const pr = (await mem.getTx(opt.process))?.tags ?? []
                const module = tags(pr).Module
                if (module) _tags["From-Module"] = module
                const { mid } = await ao.msg({
                  act: _tags["Action"] ?? null,
                  pid: tar,
                  tags: _tags,
                  data: v.Data ?? "",
                })
              } catch (e) {
                console.log(e)
              }
            } else {
              await record({
                for: opt.message,
                tags: v.Tags,
                data: v.Data,
                signer: mu.signer,
                from: opt.process,
                target: v.Target,
              })
            }
          }
        }
        for (const v of res.Spawns ?? []) {
          const __tags = tags(v.Tags)
          await spawn({
            for: opt.message,
            module: __tags.Module,
            scheduler,
            tags: v.Tags,
            data: v.Data,
            from: __tags["From-Process"],
            signer: mu.signer,
          })
        }
        for (const v of res.Assignments ?? []) {
          for (const v2 of v.Processes) {
            await assign({
              message: v.Message,
              process: v2,
              from: opt.process,
              signer: mu.signer,
            })
          }
        }
        return id
      } catch (e) {
        console.log(e)
      }
      return null
    }
    const message = async opt => {
      let id = ""
      let owner = ""
      let item = null
      if (ar.isHttpMsg(opt.http_msg)) {
        id = opt.id ?? opt.http_msg.target
        const key = `${opt.process}:${id}}`
        if (ongoing[key]) {
          let i = 0
          while (i < 30 && ongoing[key]) {
            await wait(100)
            i++
          }
          return id
        } else {
          ongoing[key] = true
          ;({ owner, item } = await ar.httpmsg(opt.http_msg, id))
          // check if process exists, and recover if necessary
          const p = await mem.get("env", opt.process)
          const new_slot = opt.slot * 1
          const last_slot = !p ? -1 : p.results.length - 1
          if (last_slot + 1 < new_slot) {
            if (!hb || opt.recovery) return null
            await recover(opt.process)
          }
        }
      } else {
        id = opt?.item?.id ?? ""
        owner = opt.owner ?? ""
        item = opt.item
      }
      const p = await mem.get("env", opt.process)
      if (!p) return null
      if (!opt.item && opt.signer) {
        opt.tags = buildTags(
          null,
          mergeLeft(tags(opt.tags ?? []), {
            "Data-Protocol": "ao",
            Variant: variant ?? "ao.TN.1",
            Type: "Message",
          })
        )
        if (opt.for) {
          opt.tags.push({ name: "Pushed-For", value: opt.for })
          opt.tags.push({ name: "From-Process", value: opt.from })
          const pr = (await mem.getTx(opt.from))?.tags ?? []
          const module = tags(pr).Module
          if (module) opt.tags.push({ name: "From-Module", value: module })
        }
        ;({ item, id, owner } = await ar.dataitem({
          data: opt.data,
          signer: opt.signer,
          tags: tags(opt.tags),
          target: opt.process,
        }))
      }
      const _msg = dissoc("signer", opt)
      await mem.set(_msg, "msgs", id)
      if (ar.isHttpMsg(opt.http_msg)) {
        p.epochs.push([id])
        let _opt = opt
        try {
          // todo: not sure if this is correct
          let from = _opt.from ?? opt.from ?? owner
          let data = opt.http_msg.data ?? ""
          let _tags = _opt.http_msg.tags
          // todo: check if owner=mu.addr right?
          const msg = await genMsg(opt.http_msg, p, data, _tags, from, owner)
          const _env = await genEnv({
            pid: p.id,
            owner: p.owner,
            module: p.module,
          })
          if (!p.handle) {
            const { format, mod, wasm } = await mem.getWasm(p.module)
            const wdrive = extensions[p.extension]
            p.handle = await AoLoader(wasm, {
              format,
              WeaveDrive: wdrive,
              spawn: (await mem.getTx(p.id))?.item,
              module: await mem.getTx(mod),
            })
            mem.env[opt.process].handle = p.handle
          }
          if (p.compressed) {
            const start = Date.now()
            p.memory = mem.decompress(p.memory, p.original_size)
            p.compressed = false
          }
          const res = await p.handle(p.memory, msg, _env)
          p.memory = res.Memory
          delete res.Memory
          p.results.push(id)
          await mem.set(p, "env", opt.process)
          const _msg = { ...dissoc("signer", _opt), res, msg }
          await mem.set(_msg, "msgs", id)
        } catch (e) {
          console.log(e)
        }
      } else {
        await assign({
          message_item: item,
          message: id,
          process: opt.process,
          from: owner,
          signer: mu.signer,
        })
      }
      const key = `${opt.process}:${id}}`
      delete ongoing[key]
      return id
    }
    const record = async opt => {
      let id = opt?.item?.id ?? ""
      let owner = opt.owner ?? ""
      let item = opt.item
      opt.tags = buildTags(
        null,
        mergeLeft(tags(opt.tags ?? []), {
          "Data-Protocol": "ao",
          Variant: variant ?? "ao.TN.1",
          Type: "Message",
        })
      )
      if (opt.for) {
        opt.tags.push({ name: "Pushed-For", value: opt.for })
        opt.tags.push({ name: "From-Process", value: opt.from })
        const pr = (await mem.getTx(opt.from))?.tags ?? []
        const module = tags(pr).Module
        if (module) opt.tags.push({ name: "From-Module", value: module })
      }
      ;({ item, id, owner } = await ar.dataitem({
        data: opt.data,
        signer: opt.signer,
        tags: tags(opt.tags),
        target: opt.process,
      }))
      const _msg = dissoc("signer", opt)
      await mem.set(_msg, "msgs", id)
      await ar.postItems(item, su.jwk)
    }
    const recover = async (pid, next) => {
      let count = 0
      let success = false
      if (hb) {
        if (onRecovery[pid]) {
          let success = false
          let i = 0
          while (true) {
            await wait(1000)
            if (!onRecovery[pid]) {
              success = true
              break
            }
            if (i > 10) break
            i++
          }
          return { success }
        } else {
          onRecovery[pid] = true
          try {
            const p = await mem.get("env", pid)
            const from = p ? p.results.length : 0
            const msgs = next ? await next() : await hb.messages({ pid, from })
            for (let v of msgs.edges) {
              let item = {}
              for (let k in v.node.message) {
                item[k.toLowerCase()] = v.node.message[k]
              }

              item.tags.push({ name: "signature-input", value: "http-sig-" })
              item.tags.push({
                name: "siot",
                value: Number(v.cursor).toString(),
              })
              item.tags.push({ name: "Owner", value: v.node.message.Owner })
              // todo:  why all the ids from Hyperbeam are the same?
              item.tags.push({ name: "id", value: v.node.message.Id })
              item.target = pid

              let _tags = tags(item.tags)
              if (_tags.Type === "Process") {
                await spawn({
                  id: v.node.message.Id,
                  http_msg: item,
                  scheduler: _tags.Scheduler,
                  module: _tags.Module,
                  slot: v.cursor,
                  recovery: true,
                })
              } else {
                await message({
                  id: v.node.message.Id,
                  process: pid,
                  http_msg: item,
                  slot: v.cursor,
                  recovery: true,
                })
              }
              count++
            }
            if (msgs.next) await recover(pid, msgs.next)
          } catch (e) {
            console.log(e)
          }
          delete onRecovery[pid]
        }
        success = true
      }
      return { recovered: count, pid, success }
    }

    const result = async opt => {
      return (await mem.get("msgs", opt.message))?.res
    }

    return {
      message,
      unmonitor: async opt => {
        const p = await mem.get("env", opt.process)
        try {
          clearInterval(p.cron)
          p.cron = null
        } catch (e) {}
      },
      monitor: async opt => {
        const p = await mem.get("env", opt.process)
        if (isNil(p.cron)) {
          p.cron = setInterval(async () => {
            await message({
              tags: p.cronTags,
              process: opt.process,
              signer: mu.signer,
              from: mu.addr,
            })
          }, p.span)
        }
      },
      spawn,
      assign,
      ar,
      result,
      results: async opt => {
        const p = await mem.get("env", opt.process)
        if (!p) return { edges: [] }
        let results = p?.results || []
        const { from = null, to = null, sort = "ASC", limit = 25 } = opt || {}

        if (sort === "DESC") results = reverse(results)
        let _res = []
        let count = 0
        let started = isNil(from)
        for (let v of results) {
          if (started) {
            _res.push({ cursor: v, node: await result({ message: v }) })
            count++
            if (!isNil(to) && v === to) break
            if (limit <= count) break
          } else if (from === v) started = true
        }
        return { edges: _res }
      },
      dryrun: async opt => {
        const p = await mem.get("env", opt.process)
        if (!p) return null
        let id = opt.id ?? ""
        let owner = opt.owner ?? mu.addr
        if (!opt.id && opt.signer) {
          ;({ id, owner } = await ar.dataitem({ ...opt, target: opt.process }))
        }
        try {
          const msg = await genMsg(
            id,
            p,
            opt.data ?? "",
            opt.tags,
            owner,
            owner,
            true
          )
          const _env = await genEnv({
            pid: p.id,
            owner: p.owner,
            module: p.module,
          })
          function cloneMemory(memory) {
            const buffer = memory.buffer.slice(0)
            return new WebAssembly.Memory({
              initial: memory.buffer.byteLength / 65536,
              maximum: memory.maximum || undefined,
              shared: memory.shared || false,
            })
          }
          if (!p.handle) {
            const { format, mod, wasm } = await mem.getWasm(p.module)
            const wdrive = extensions[p.extension]
            p.handle = await AoLoader(wasm, {
              format,
              WeaveDrive: wdrive,
              spawn: (await mem.getTx(p.id)).item,
              module: await mem.getTx(mod),
            })
            mem.env[opt.process].handle = p.handle
          }
          if (p.compressed) {
            const start = Date.now()
            p.memory = mem.decompress(p.memory, p.original_size)
            p.compressed = false
          }
          const res = await p.handle(p.memory, msg, _env)
          delete res.Memory
          return res
        } catch (e) {
          console.log(e)
        }
        return null
      },
      recover,
      mem,
    }
  }
}
