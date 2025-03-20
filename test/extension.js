import assert from "assert"
import { resolve } from "path"
import { describe, it } from "node:test"
import { AO, acc } from "../src/test.js"
import { ok, fail, Src } from "../src/helpers.js"
import weavedb from "../src/weavedb.js"
import { encode, Encoder, Bundle } from "arjson"

let attestor = acc[0]
const { signer, jwk } = attestor

const handler = `
function get (id)
  local file = io.open(id)
  local data = nil
  if file then data = file:read(file:seek('end')) end
  file:close()
  return data
end

function data (id) return get('/data/' .. id) end

function block (height) return get('/block/' .. height) end

function tx (id) return get('/tx/' .. id) end

function tx2 (id) return get('/tx2/' .. id) end

Handlers.add("Data", "Data", function (msg)
  msg.reply({ Data = data(msg.id) })
end)

Handlers.add("Block", "Block", function (msg)
  msg.reply({ Data = block(msg.height) })
end)

Handlers.add("Tx", "Tx", function (msg)
  msg.reply({ Data = tx(msg.id) })
end)

Handlers.add("Tx2", "Tx2", function (msg)
  msg.reply({ Data = tx2(msg.id) })
end)
`

describe("WeaveDrive", function () {
  it.only("should load Arweave tx data", async () => {
    const ao = await new AO().init(attestor)
    const { p, err, mid } = ok(
      await ao.deploy({
        tags: { Extension: "WeaveDrive", Attestor: attestor.addr },
        loads: [handler],
      })
    )
    const { id } = await ao.ar.post({ data: "Hello" })
    await ao.attest({ id })
    assert.equal(await p.d("Data", { id }), "Hello")
    assert.equal((await p.d("Block", { height: "2" })).height, 2)
    assert.equal((await p.d("Tx", { id })).id, id)
    assert.equal((await p.d("Tx2", { id: mid })).id, mid)
  })
})

describe("WeaveDB", function () {
  it("should compact ARJSON", async () => {
    const db = await new DB().init()
    await db.q().c({ test: 1 }, 1, 123).c({ test: 2 }, 1, 120).send()
    await db.get(1, 123, { test: 1 })
    await db.get(1, 120, { test: 2 })
    await db.q().u({ test: 2 }, 1, 123).u({ test: 20 }, 1, 123).d(1, 120).send()
    await db.get(1, 123, { test: 20 })
    await db.get(1, 120, null)
  })
})

class DB {
  constructor() {
    this.ao = new AO({ extensions: { WeaveDB: weavedb } })
    this.data = {}
  }
  q() {
    const b = new Bundle(this.data, this)
    return b
  }
  async init() {
    const src = new Src({ dir: resolve(import.meta.dirname, "../src/lua") })
    const data = src.data("weavedb_mock")
    const { p } = await this.ao.deploy({
      tags: { Extension: "WeaveDB" },
      loads: [data],
    })
    this.p = p
    return this
  }
  async get(col, doc, eq) {
    const res = await this.p.d("Get", {
      col: col.toString(),
      doc: doc.toString(),
    })
    if (typeof eq !== "undefined") assert.deepEqual(res, eq)
    return res
  }
  async query(b) {
    const buf = Buffer.from(b.dump().buffer)
    const { mid } = await this.p.msg("Rollup", {
      check: "committed!",
      data: buf,
    })
    ok(
      await this.p.msg("Finalize", {
        check: "finalized!",
        tags: { TXID: mid },
      })
    )
  }
}
