import { Profile, AR, AO, Collection, Notebook } from "./index.js"
import { expect } from "chai"
import { createDataItemSigner, connect } from "@permaweb/aoconnect"
import { resolve } from "path"
import { readFileSync } from "fs"
export class Src {
  constructor({ ar, base = "./lua", readFileSync, dir, resolve }) {
    this.ar = ar
    this.base = base
    this.dir = dir
  }
  async upload(file, ext = "lua") {
    const data = readFileSync(
      `${this.dir}/${this.base}/${file}.${ext}`,
      ext === "wasm" ? null : "utf8",
    )
    const res = await this.ar.post({ data })
    return res.err ? null : res.id
  }
}

export const setup = async ({ aoconnect, arweave } = {}) => {
  console.error = () => {}
  console.warn = () => {}
  arweave ??= { port: 4000 }
  aoconnect ??= {
    MU_URL: "http://localhost:4002",
    CU_URL: "http://localhost:4004",
    GATEWAY_URL: "http://localhost:4000",
  }
  const dir = resolve(import.meta.dirname)
  const thumbnail = readFileSync(`${dir}/assets/thumbnail.png`)
  const banner = readFileSync(resolve(`${dir}/assets/banner.png`))
  const ar = new AR(arweave)
  await ar.gen("10")
  const src = new Src({ ar, readFileSync, dir })
  const notelib_src = await src.upload("atomic-note-library")
  const registry_src = await src.upload("registry000")
  const profile_src = await src.upload("profile000")
  const collection_registry_src = await src.upload("collection-registry")
  const notebook_src = await src.upload("notebook")
  const note_src = await src.upload("atomic-note")
  const proxy = await src.upload("proxy")
  const wasm = await src.upload("aos-sqlite", "wasm")
  const wasm2 = await src.upload("aos", "wasm")
  const ao = new AO({ aoconnect, ar })

  const { id: module_sqlite } = await ao.postModule({
    data: await ar.data(wasm),
    overwrite: true,
  })

  const { scheduler } = await ao.postScheduler({
    url: "http://su",
    overwrite: true,
  })

  const profile = new Profile({ profile_src, registry_src, ao })
  await profile.createRegistry({})
  const notebook = new Notebook({
    notebook_src,
    registry_src: collection_registry_src,
    profile,
  })

  await notebook.createRegistry()
  const { id: module } = await ao.postModule({ data: await ar.data(wasm2) })
  const { pid: proxy_pid } = await ao.deploy({ src: proxy, module })

  let opt = {
    ar: { ...arweave },
    profile: { registry_src, registry: profile.registry, profile_src },
    ao: { module: module_sqlite, scheduler, aoconnect },
    note: { proxy: proxy_pid, note_src, notelib_src },
    notebook: {
      notebook_src,
      registry: notebook.registry,
      registry_src: collection_registry_src,
    },
  }
  return { opt, profile, ao, ar, thumbnail, banner }
}

export const ok = obj => {
  if (obj.err) console.log(obj.err)
  expect(obj.err).to.eql(null)
  return obj
}

export const fail = obj => {
  if (!obj.err) console.log(obj.res)
  expect(obj.err).to.not.eql(null)
  return obj
}
