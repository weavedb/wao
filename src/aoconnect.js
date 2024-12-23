import * as WarpArBundles from "warp-arbundles"
const pkg = WarpArBundles.default ?? WarpArBundles
const { DataItem } = pkg

import crypto from "crypto"
import base64url from "base64url"

import {
  tags,
  action,
  tag,
  buildTags,
  isJSON,
  jsonToStr,
  dirname,
} from "./utils.js"
import ArMem from "./armem.js"
import weavedrive from "./weavedrive.js"
import AoLoader from "@permaweb/ao-loader"
import { readFileSync } from "fs"
import { resolve } from "path"
import { scheduler, mu, su, cu, acc } from "./test.js"
import { is, clone, fromPairs, map, mergeLeft, isNil } from "ramda"
import AR from "./tar.js"

export const connect = (mem, { log = false, extensions = {} } = {}) => {
  const isMem = mem?.__type__ === "mem"
  if (!isMem) {
    let args = {}
    if (mem?.SU_URL) {
      args = mem
      args.scheduler = scheduler
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

  const genMsg = (Id, p, data, Tags, from, Owner, dry = false) => {
    if (!dry) p.height += 1
    return {
      Id,
      Target: p.id,
      Owner,
      Data: data?.length ? data : "",
      "Block-Height": mem.height.toString(),
      Timestamp: Date.now().toString(),
      Module: p.module,
      From: from,
      Cron: false,
      Tags: Tags?.length ? Tags : [],
    }
  }

  const genEnv = ({ pid, owner = "", module = "", auth = "" }) => {
    return {
      Process: {
        Id: pid,
        Tags: [
          { name: "Data-Protocol", value: "ao" },
          { name: "Variant", value: "ao.TN.1" },
          { name: "Type", value: "Process" },
          { name: "Authority", value: auth },
        ],
        Owner: owner,
      },
      Module: {
        Id: module,
        Tags: [
          { name: "Data-Protocol", value: "ao" },
          { name: "Variant", value: "ao.TN.1" },
          { name: "Type", value: "Module" },
        ],
      },
    }
  }

  const spawn = async (opt = {}) => {
    if (!opt.module) throw Error("module missing")
    if (!opt.scheduler) throw Error("scheduler missing")
    let mod = opt.module ?? mem.modules["aos2_0_1"]
    let _module = null
    const __dirname = await dirname()
    let format = null
    let wasm = mem.wasms[mod]?.data
    if (!wasm) {
      if (mem.wasms[mod]?.file) {
        wasm = readFileSync(
          resolve(__dirname, `lua/${mem.wasms[mod].file}.wasm`),
        )
        format = mem.wasms[mod].format
      } else if (mem.txs[mod]) {
        wasm = Buffer.from(mem.txs[mod].data, "base64")
        format = tags(mem.txs[mod].tags)["Module-Format"]
      }
    } else {
      format = mem.wasms[mod].format
    }
    format ??= "wasm64-unknown-emscripten-draft_2024_02_15"

    if (!mod) throw Error("module not found")
    opt.tags = buildTags(
      null,
      mergeLeft(tags(opt.tags ?? []), {
        "Data-Protocol": "ao",
        Variant: "ao.TN.1",
        Type: "Process",
        SDK: "aoconnect",
        Module: mod,
        Scheduler: opt.scheduler,
        "Content-Type": "text/plain",
      }),
    )
    let ex = false
    opt.tags ??= []
    for (let v of opt.tags) if (v.name === "Type") ex = true
    if (!ex) opt.tags.push({ name: "Type", value: "Process" })
    const {
      id,
      owner,
      item,
      tags: __tags,
    } = await ar.dataitem({
      item: opt.item,
      data: opt.data,
      signer: opt.signer,
      tags: tags(opt.tags),
    })
    opt.tags = buildTags(null, __tags)
    if (opt.item) opt.data = base64url.decode(item.data)
    await ar.postItems(item, su.jwk)
    const now = Date.now
    const t = tags(opt.tags)
    const wdrive = extensions[t.Extension || "WeaveDrive"]
    let handle = null
    try {
      handle = await AoLoader(wasm, {
        format,
        WeaveDrive: wdrive,
        spawn: item,
        module: mem.txs[mod],
      })
    } catch (e) {}
    if (!handle) return null
    _module = { handle, id: mod }
    Date.now = now

    const _tags = tags(opt.tags)
    let res = null
    let memory = null
    let p = {
      id: id,
      epochs: [],
      handle: _module.handle,
      module: _module.id,
      hash: id,
      memory,
      owner,
      height: 0,
      res: { [id]: res },
      results: [id],
      txs: [],
      opt,
    }
    if (_tags["On-Boot"] || true) {
      let data = ""
      if (_tags["On-Boot"] === "Data") data = opt.data ?? ""
      else data = mem.msgs[_tags["On-Boot"]]?.data ?? ""
      let msg = genMsg(id, p, data, opt.tags, owner, mu.addr, true)
      const _env = genEnv({
        pid: p.id,
        owner: p.owner,
        module: p.module,
        auth: mu.addr,
      })

      const _t = tags(msg.Tags)
      res = await _module.handle(null, msg, _env)
      p.memory = res.Memory
      delete res.Memory
      p.res[id] = res
    } else {
      p.height += 1
    }
    mem.msgs[id] = opt
    mem.env[id] = p
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
      mem.env[id].cronTags = cronTags
      mem.env[id].span = int
    }
    return id
  }

  function genHashChain(previousHash, previousMessageId = null) {
    const hasher = crypto.createHash("sha256")
    hasher.update(Buffer.from(previousHash, "base64url"))
    if (previousMessageId) {
      hasher.update(Buffer.from(previousMessageId, "base64url"))
    }
    return base64url(hasher.digest())
  }

  const assign = async opt => {
    const p = mem.env[opt.process]
    if (!p) return null
    let _opt = mem.msgs[opt.message]
    let hash = genHashChain(p.hash, opt.message)
    p.hash = hash
    opt.tags = buildTags(
      null,
      mergeLeft(tags(opt.tags ?? []), {
        Timestamp: Date.now(),
        Epoch: p.epochs.length,
        Nonce: "0",
        "Data-Protocol": "ao",
        Variant: "ao.TN.1",
        SDK: "aoconnect",
        Type: "Assignment",
        "Block-Height": mem.height,
        Process: opt.process,
        Message: opt.message,
        "Hash-Chain": hash,
      }),
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
        try {
          data = base64url.decode(_opt.item.data)
          _tags = _opt.item.tags
          if (!from) {
            const raw_owner = _opt.item.rawOwner
            const hashBuffer = Buffer.from(
              await crypto.subtle.digest("SHA-256", raw_owner),
            )
            from = base64url.encode(hashBuffer)
          }
        } catch (e) {
          console.log(e)
        }
      }
      // check: is owner=mu.addr right?
      const msg = genMsg(opt.message, p, data, _tags, from, mu.addr)
      const _env = genEnv({
        pid: p.id,
        owner: p.owner,
        module: p.module,
        auth: mu.addr,
      })
      const res = await p.handle(p.memory, msg, _env)
      p.memory = res.Memory
      delete res.Memory
      p.res[opt.message] = res
      p.results.push(opt.message)
      p.txs.unshift({ id, ...opt })
      mem.msgs[opt.message] = _opt
      for (const v of res.Messages ?? []) {
        if (mem.env[v.Target]) {
          await message({
            process: v.Target,
            tags: v.Tags,
            data: v.Data,
            signer: mu.signer,
            from: opt.process,
          })
        }
      }
      for (const v of res.Spawns ?? []) {
        const __tags = tags(v.Tags)
        await spawn({
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
    const p = mem.env[opt.process]
    if (!p) return null
    let ex = false
    let id = opt?.item?.id ?? ""
    let owner = opt.owner ?? ""
    let item = opt.item
    if (!opt.item && opt.signer) {
      for (let v of opt.tags) if (v.name === "Type") ex = true
      opt.tags = buildTags(
        null,
        mergeLeft(tags(opt.tags ?? []), {
          "Data-Protocol": "ao",
          Variant: "ao.TN.1",
          Type: "Message",
          SDK: "aoconnect",
        }),
      )
      ;({ item, id, owner } = await ar.dataitem({
        data: opt.data,
        signer: opt.signer,
        tags: tags(opt.tags),
        target: opt.process,
      }))
    }
    mem.msgs[id] = opt
    await assign({
      message_item: item,
      message: id,
      process: opt.process,
      from: owner,
      signer: mu.signer,
    })
    return id
  }

  return {
    message,
    unmonitor: async opt => {
      const p = mem.env[opt.process]
      try {
        clearInterval(p.cron)
        p.cron = null
      } catch (e) {}
    },
    monitor: async opt => {
      const p = mem.env[opt.process]
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
    result: async opt => mem.env[opt.process].res[opt.message],
    results: async opt => {
      const p = mem.env[opt.process]
      let results = []
      const limit = opt.limit ?? 25
      if (opt.sort === "DESC") {
        for (let i = p.results.length - 1; 0 < i; i--) {
          results.push({ cursor: p.results[i], node: p.res[p.results[i]] })
          if (results.length >= limit) break
        }
      } else {
        for (let i = 0; i < p.results.length; i++) {
          results.push({ node: p.res[p.results[i]] })
          if (results.length >= limit) break
        }
      }
      return { edges: results }
    },
    dryrun: async opt => {
      const p = mem.env[opt.process]
      if (!p) return null
      let id = opt.id ?? ""
      let owner = opt.owner ?? ""
      if (!opt.id && opt.signer) {
        ;({ id, owner } = await ar.dataitem({ ...opt, target: opt.process }))
      }
      try {
        const msg = genMsg(
          id,
          p,
          opt.data ?? "",
          opt.tags,
          owner,
          mu.addr,
          true,
        )
        const _env = genEnv({
          pid: p.id,
          owner: p.owner,
          module: p.module,
          auth: mu.addr,
        })
        function cloneMemory(memory) {
          const buffer = memory.buffer.slice(0)
          return new WebAssembly.Memory({
            initial: memory.buffer.byteLength / 65536,
            maximum: memory.maximum || undefined,
            shared: memory.shared || false,
          })
        }
        const res = await p.handle(p.memory, msg, _env)
        return res
      } catch (e) {
        console.log(e)
      }
      return null
    },
    mem,
  }
}
