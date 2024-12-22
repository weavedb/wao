import { AR, AO } from "./index.js"
import assert from "assert"
import { createDataItemSigner, connect } from "@permaweb/aoconnect"
import { dirname as _dirname, resolve } from "path"
import { mkdirSync, existsSync, writeFileSync, readFileSync } from "fs"
import { optAO } from "./utils.js"
import yargs from "yargs"

let {
  reset = false,
  cache = false,
  auth = null,
} = yargs(process.argv.slice(2)).argv

const dirname = async () =>
  typeof __dirname != "undefined"
    ? __dirname
    : (await import("./dirname.js")).default

export class Src {
  constructor({ ar, dir } = {}) {
    this.ar = ar
    this.dir = dir
    if (!dir) dirname().then(v => (this.dir = resolve(v, "lua")))
  }
  async init(dir) {
    if (!this.dir) {
      dir ??= await dirname()
      this.dir = resolve(dir, "lua")
    }
    return this
  }
  data(file, ext = "lua") {
    return readFileSync(
      `${this.dir}/${file}.${ext}`,
      ext === "wasm" ? null : "utf8",
    )
  }
  async upload(file, ext = "lua") {
    const res = await this.ar.post({ data: this.data(file, ext) })
    return res.err ? null : res.id
  }
}

export class Testnet {
  constructor({ port = 4000, arweave, aoconnect, docker = false } = {}) {
    this.docker = docker
    this.arweave = arweave ?? { port }
    this.aoconnect = aoconnect ?? optAO(port)
    this.ar = new AR(this.arweave)
  }
  async init(jwk) {
    this.authority = (
      await fetch(this.aoconnect.SU_URL).then(r => r.json())
    ).address
    this.src = await new Src({ ar: this.ar }).init()
    await this.ar.init(jwk)
    this.jwk = this.ar.jwk
    this.addr = this.ar.addr
    this.gql = this.ar.gql
    this.module_src = await this.src.upload("aos2_0_1", "wasm")
    this.ao = await new AO({
      ar: this.ar,
      aoconnect: this.aoconnect,
      authority: this.authority,
    }).init(this.ar.jwk)
    const { id } = await this.ao.postModule({
      data: await this.ar.data(this.module_src),
      overwrite: true,
    })
    this.module = id
    const { scheduler } = await this.ao.postScheduler({
      url: this.docker ? "http://su" : this.aoconnect.SU_URL,
      overwrite: true,
    })
    this.scheduler = scheduler
    return this
  }
}

export const setup = async ({
  aoconnect,
  arweave,
  cacheDir = ".cache",
  targets = { profile: false, note: false, asset: false },
} = {}) => {
  if (targets.asset || targets.note) targets.profile = true
  let opt = null
  console.error = () => {}
  console.warn = () => {}
  const dir = resolve(await dirname(), "lua")
  const _cacheDir = resolve(await dirname(), cacheDir)
  const optPath = `${_cacheDir}/opt.json`
  if (cache && !reset) {
    try {
      if (existsSync(optPath)) {
        opt = JSON.parse(readFileSync(optPath, "utf8"))
      } else {
        console.log("cache doesn't exist:", optPath)
      }
    } catch (e) {
      console.log(e)
    }
  }

  if (opt) {
    const ar = await new AR(opt.ar).init(opt.jwk)
    const src = new Src({ ar, dir })
    const ao = await new AO(opt.ao).init(opt.jwk)
    const ao2 = await new AO(opt.ao2).init(opt.jwk)
    console.log("cache:\t", optPath)
    console.log("addr:\t", ar.addr)
    return { opt, ar, ao2, ao, src }
  }

  // ar
  arweave ??= { port: 4000 }
  aoconnect ??= optAO(4000)
  const ar = new AR(arweave)
  await ar.gen("10")
  const src = new Src({ ar, dir })
  opt = { ar: { ...arweave }, jwk: ar.jwk }
  if (!auth && /localhost/.test(aoconnect?.CU_URL ?? "")) {
    auth = (await fetch(aoconnect.CU_URL).then(r => r.json())).address
  }

  // ao
  const wasm = await src.upload("sqlite", "wasm")
  const wasm2 = await src.upload("aos", "wasm")
  const wasm_aos2 = await src.upload("aos2_0_1", "wasm")

  const ao = new AO({ aoconnect, ar, authority: auth })
  const { id: module_aos2 } = await ao.postModule({
    data: await ar.data(wasm_aos2),
  })

  const { id: module_sqlite } = await ao.postModule({
    data: await ar.data(wasm),
    overwrite: true,
  })

  const { id: module } = await ao.postModule({
    data: await ar.data(wasm2),
    overwrite: true,
  })

  const { scheduler } = await ao.postScheduler({
    url: "http://su",
    overwrite: true,
  })

  opt.ao = {
    module: module_sqlite,
    scheduler,
    aoconnect,
    ar: opt.ar,
    authority: auth,
  }

  // ao2
  const ao2 = await new AO({
    aoconnect,
    ar,
    authority: auth,
    module: module_aos2,
    scheduler,
  }).init(ar.jwk)

  opt.ao2 = {
    module: module_aos2,
    scheduler,
    aoconnect,
    ar: opt.ar,
    authority: auth,
  }

  if (auth) opt.ao.authority = auth
  opt.authority = auth
  opt.targets = targets
  opt.modules = {
    aos2: module_aos2,
    aos1: module,
    sqlite: module_sqlite,
  }
  if (cache) {
    if (!existsSync(_cacheDir)) mkdirSync(_cacheDir)
    writeFileSync(optPath, JSON.stringify(opt))
  }
  return { opt, ao, ar, src, ao2 }
}

export const ok = obj => {
  if (obj.err) console.log(obj.err)
  assert.equal(obj.err, null)
  return obj
}

export const fail = obj => {
  if (!obj.err) console.log(obj.res)
  assert.notEqual(obj.err, null)
  return obj
}
