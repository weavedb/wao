import { DataItem } from "warp-arbundles"
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

export const connect = mem => {
  mem ??= new ArMem()
  const ar = new AR({ mem })
  const WeaveDrive = new weavedrive(ar).drive

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
      "Block-Height": p.height.toString(),
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

  const postModule = async ({ data, tags = {}, signer }) => {
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
    const { id, owner } = await ar.dataitem({ tags: _tags, data, signer })
    await ar.post({ data, signer, tags: t })
    const handle = await AoLoader(data, {
      format: t["Module-Format"],
      mode: "test",
      WeaveDrive,
    })

    // test me
    mem.modmap[id] = { handle, id, data, format: t["Module-Format"] }
    return id
  }

  const spawn = async (opt = {}) => {
    if (!opt.module) throw Error("module missing")
    if (!opt.scheduler) throw Error("scheduler missing")
    let mod = opt.module ?? mem.modules["aos2_0_1"]
    let _module = null
    const __dirname = await dirname()
    const wasm = readFileSync(
      resolve(__dirname, `lua/${mem.wasms[mod].file}.wasm`),
    )
    const handle = await AoLoader(wasm, {
      format: mem.wasms[mod].format,
      mode: "test",
      WeaveDrive,
    })
    _module = { handle, id: mod }
    if (!mod) throw Error("module not found")
    opt.tags = buildTags(
      null,
      mergeLeft(tags(opt.tags ?? []), {
        "Data-Protocol": "ao",
        Variant: "ao.TN.1",
        Type: "Process",
        SDK: "aoconnect",
        Module: opt.module,
        Scheduler: opt.scheduler,
        "Content-Type": "text/plain",
      }),
    )
    let ex = false
    opt.tags ??= []
    for (let v of opt.tags) if (v.name === "Type") ex = true
    if (!ex) opt.tags.push({ name: "Type", value: "Process" })
    const { id, owner, item } = await ar.dataitem({
      data: opt.data,
      signer: opt.signer,
      tags: tags(opt.tags),
    })
    await ar.postItem(item, su.jwk)
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
    if (_tags["On-Boot"]) {
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
    await ar.postItem(item, su.jwk)
    try {
      const msg = genMsg(
        opt.message,
        p,
        _opt.data ?? "",
        _opt.tags,
        _opt.from ?? opt.from ?? owner,
        mu.addr,
      )
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
      p.txs.unshift({ id: opt.message, ...opt })
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
        const _tags = tags(v.Tags)
        await spawn({
          module: _tags.Module,
          scheduler,
          tags: v.Tags,
          data: v.Data,
          from: _tags["From-Process"],
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
    let ex = false
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
    const { item, id, owner } = await ar.dataitem({
      data: opt.data,
      signer: opt.signer,
      tags: tags(opt.tags),
      target: opt.process,
    })
    await ar.postItem(item, su.jwk)
    mem.msgs[id] = opt
    await assign({
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
      const { id, owner } = await ar.dataitem({ ...opt, target: opt.process })
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
