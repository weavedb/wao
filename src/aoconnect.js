import { DataItem } from "warp-arbundles"
import base64url from "base64url"
import AoLoader from "@permaweb/ao-loader"
import { readFileSync } from "fs"
import { resolve } from "path"
import { is, clone, fromPairs, map, mergeLeft } from "ramda"
import accounts from "./accounts.js"
import { tags, action, tag, buildTags } from "./utils.js"
function isJSON(obj) {
  if (obj === null || obj === undefined) return false
  if (
    typeof obj !== "object" ||
    obj instanceof Buffer ||
    obj instanceof ArrayBuffer ||
    Array.isArray(obj)
  ) {
    return false
  }

  try {
    const str = JSON.stringify(obj)
    const parsed = JSON.parse(str)
    const isjson = typeof parsed === "object" && parsed !== null
    return isjson ? str : false
  } catch (e) {
    return false
  }
}

const jsonToStr = obj =>
  isJSON(obj) || (is(Number, obj) ? Number(obj).toString() : obj)

const dirname = async () =>
  typeof __dirname != "undefined"
    ? __dirname
    : (await import("./dirname.js")).default

export const acc = accounts.users
export const mu = accounts.mu
const scheduler = "GQ33BkPtZrqxA84vM8Zk-N2aO0toNNu_C-l-rawrBA"
let env = {}

export const connect = () => {
  let wasms = {
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
  let modules = {
    aos2_0_1: "Do_Uc2Sju_ffp6Ev0AnLVdPtot15rvMjP-a9VVaA5fM",
    aos1: "cNlipBptaF9JeFAf4wUmpi43EojNanIBos3EfNrEOWo",
    sqlite: "ghSkge2sIUD_F00ym5sEimC63BDBuBrq4b5OcwxOjiw",
  }
  let modmap = {}
  let msgs = {}
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

  const genMsg = (p, data, Tags, from, owner, dry = false) => {
    if (!dry) p.height += 1
    return {
      Id: p.height,
      Target: p.id,
      Owner: owner,
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

  const parse = async opt => {
    const item = await opt.signer({ data: opt.data ?? "", tags: opt.tags })
    const rowner = new DataItem(item.raw).rawOwner
    const hashBuffer = Buffer.from(
      await crypto.subtle.digest("SHA-256", rowner),
    )
    const owner = base64url.encode(hashBuffer)
    return { id: item.id, owner }
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
    const _tags = buildTags(t)
    const { id, owner } = await parse({ tags: _tags, data, signer })
    const handle = await AoLoader(data, { format: t["Module-Format"] })
    modmap[id] = { handle, id }
    return id
  }

  const spawn = async (opt = {}) => {
    if (!opt.module) throw Error("module missing")
    if (!opt.scheduler) throw Error("scheduler missing")
    let mod = opt.module ?? modules["aos2_0_1"]
    if (!modmap[mod] && wasms[mod]) {
      const __dirname = await dirname()
      const wasm = readFileSync(
        resolve(__dirname, `lua/${wasms[mod].file}.wasm`),
      )
      const handle = await AoLoader(wasm, { format: wasms[mod].format })
      modmap[mod] = { handle, id: mod }
    }
    if (!mod) throw Error("module not found")
    const _module = modmap[mod]
    let ex = false
    opt.tags ??= []
    for (let v of opt.tags) if (v.name === "Type") ex = true
    if (!ex) opt.tags.push({ name: "Type", value: "Process" })
    const { id, owner } = await parse(opt)
    const _tags = tags(opt.tags)
    let res = null
    let memory = null
    let p = {
      id: id,
      handle: _module.handle,
      module: _module.id,
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
      else data = msgs[_tags["On-Boot"]]?.data ?? ""
      let msg = genMsg(p, data, opt.tags, owner, mu.addr, true)
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
    msgs[id] = opt
    env[id] = p
    return id
  }

  const assign = async opt => {
    const p = env[opt.process]
    let _opt = clone(msgs[opt.message])
    const { id, owner } = await parse(_opt)
    try {
      const msg = genMsg(
        p,
        _opt.data ?? "",
        _opt.tags,
        _opt.from ?? owner,
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
      p.res[id] = res
      p.results.push(id)
      p.txs.unshift({ id: id, ..._opt })
      msgs[id] = _opt
      for (const v of res.Messages ?? []) {
        if (env[v.Target]) {
          await message({
            process: v.Target,
            tags: v.Tags,
            data: v.Data,
            signer: mu.signer,
            from: opt.process,
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
    const p = env[opt.process]
    let ex = false
    for (let v of opt.tags) if (v.name === "Type") ex = true

    if (!ex) opt.tags.push({ name: "Type", value: "Message" })
    const { id, owner } = await parse(opt)
    try {
      const msg = genMsg(
        p,
        opt.data ?? "",
        opt.tags,
        opt.from ?? owner,
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
      p.res[id] = res
      p.results.push(id)
      p.txs.unshift({ id: id, ...opt })
      msgs[id] = opt
      for (const v of res.Messages ?? []) {
        if (env[v.Target]) {
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
  return {
    scheduler,
    modules,
    accounts: acc,
    mu,
    message,
    txs: async pid => {
      let _txs = []
      for (let v of env[pid].txs) _txs.push({ tags: v.tags, id: v.id })
      return _txs
    },
    spawn,
    assign,
    result: async opt => env[opt.process].res[opt.message],
    results: async opt => {
      const p = env[opt.process]
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
      const p = env[opt.process]
      const { id, owner } = await parse(opt)
      try {
        const msg = genMsg(p, opt.data ?? "", opt.tags, owner, mu.addr, true)
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
    getProcesses: () => env,
  }
}
