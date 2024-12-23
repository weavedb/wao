import assert from "assert"
import { resolve } from "path"
import { describe, it } from "node:test"
import { AO, acc } from "../src/test.js"
import { ok, fail, Src } from "../src/helpers.js"
import weavedb from "../src/weavedb.js"

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
  it("should load Arweave tx data", async () => {
    const ao = await new AO().init(attestor)
    const { p, err, mid } = ok(
      await ao.deploy({
        tags: { Extension: "WeaveDrive", Attestor: attestor.addr },
        loads: [handler],
      }),
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
  it.only("should load Rollup queries", async () => {
    const ao = new AO({ extensions: { WeaveDB: weavedb } })
    const src = new Src({ dir: resolve(import.meta.dirname, "../src/lua") })
    const data = src.data("weavedb_mock")
    const { p } = await ao.deploy({
      tags: { Extension: "WeaveDB" },
      loads: [data],
    })
    const { mid } = ok(
      await p.msg("Rollup", {
        check: "committed!",
        data: JSON.stringify({
          diffs: [
            {
              op: "set",
              collection: "ppl",
              doc: "bob",
              data: { name: "Bob", age: 40 },
            },
          ],
        }),
      }),
    )

    ok(
      await p.msg("Finalize", {
        check: "finalized!",
        tags: { TXID: mid },
      }),
    )
    assert.deepEqual(await p.d("Get", { col: "ppl", doc: "bob" }), {
      name: "Bob",
      age: 40,
    })
  })
})
