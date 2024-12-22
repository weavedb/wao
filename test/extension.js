import assert from "assert"
import { resolve } from "path"
import { afterEach, after, describe, it, before, beforeEach } from "node:test"
import { blueprint, mu, AO, connect, acc, scheduler } from "../src/test.js"
import Server from "../src/server.js"
import MAO from "../src/ao.js"
import AR from "../src/ar.js"
import GQL from "../src/gql.js"
import ArMem from "../src/armem.js"
import { ok, fail, setup, Src, Testnet } from "../src/helpers.js"
import { optAO, tags, wait } from "../src/utils.js"

const { mem, spawn, message, dryrun } = connect()
let attestor = acc[0]
const { signer, jwk } = attestor

const handler = `
function data (id)
  local file = io.open('/data/' .. id)
  local data = nil
  if file then
    data = file:read(file:seek('end'))
   end
  file:close()
  return data
end

function block (height)
  local file = io.open('/block/' .. height)
  local data = nil
  if file then
    data = file:read(file:seek('end'))
   end
  file:close()
  return data
end

function tx (id)
  local file = io.open('/tx/' .. id)
  local data = nil
  if file then
    data = file:read(file:seek('end'))
   end
  file:close()
  return data
end

function tx2 (id)
  local file = io.open('/tx2/' .. id)
  local data = nil
  if file then
    data = file:read(file:seek('end'))
   end
  file:close()
  return data
end

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
  let ao, server, mem
  before(async () => {})
  after(async () => {
    if (server) await server.end()
  })

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
