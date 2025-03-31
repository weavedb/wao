import {
  results as r,
  connect as _connect,
  createDataItemSigner,
} from "@permaweb/aoconnect"
import base64url from "base64url"
import assert from "assert"
import { resolve } from "path"
import { after, describe, it, before, beforeEach } from "node:test"
import { blueprint, mu, connect, acc, scheduler } from "../src/test.js"
import AO from "../src/ao.js"
import TAO from "../src/tao.js"
import AR from "../src/ar.js"
import GQL from "../src/gql.js"
import ArMem from "../src/armem.js"
import { setup, Src } from "../src/helpers.js"
import { tags, wait, optAO } from "../src/utils.js"
import Server from "../src/server.js"
const { mem, spawn, message, dryrun } = connect()
const [{ signer, jwk }] = acc

const src_data = `
local count = 0
Handlers.add("Hello", "Hello", function (msg)
  count = count + 1
  msg.reply({ Data = "Hello, World: "..tostring(count) })
end)
`

const src_counter = `
local count = 0

Handlers.add("Add", "Add", function (msg)
  count = count + tonumber(msg.Plus)
end)

Handlers.add("Get", "Get", function (msg)
  msg.reply({ Data = tostring(count) })
end)
`

describe("SDK", function () {
  after(() => setTimeout(() => process.exit(), 100))

  it("should dump wasm memory", async () => {
    let ao = await new AO(4000).init(acc[0])
    const { p, pid } = await ao.deploy({ boot: true, src_data: src_counter })
    const memory = new Uint8Array(
      await fetch(`http://localhost:4004/state/${pid}`).then(r =>
        r.arrayBuffer()
      )
    )
    assert.notEqual(memory, null)
  })

  it("should work with aoconnect results", async () => {
    let ao = await new AO(4000).init(acc[0])
    const { p, pid } = await ao.deploy({ boot: true, src_data: src_counter })
    const { mid } = await p.msg("Add", { Plus: 3 })
    assert.equal(await p.d("Get"), "3")
    const { spawn, message, dryrun, assign, result, results } = _connect({
      MU_URL: `http://localhost:4002`,
      CU_URL: `http://localhost:4004`,
      GATEWAY_URL: `http://localhost:4000`,
    })
    const { edges } = await results({ process: pid, limit: 2 })
    const cursor = edges[1].node.cursor
    const { edges: edges2 } = await results({
      process: pid,
      limit: 1,
      start: cursor,
    })
    assert.equal(edges2[0].node.cursor, cursor)
  })
  it.only("should run server", async () => {
    let ao = await new AO({ ar: { port: 4000 }, aoconnect: optAO(4000) }).init(
      acc[0]
    )
    const data = `
Handlers.add("Hello", "Hello", function (msg)
  msg.reply({ Data = "Hello, World!" })
end)
`
    const { p, pid } = await ao.spwn({
      module: "JArYBF-D8q2OmZ4Mok00sD2Y_6SYEQ7Hjx-6VZ_jl3g",
    })
    console.log(await ao.eval({ pid, data }))
    console.log(await p.m("Hello"))
    return
  })
  it("should run server", async () => {
    let ao = await new AO({ ar: { port: 4000 }, aoconnect: optAO(4000) }).init(
      acc[0]
    )
    const { p, pid } = await ao.deploy({ boot: true, src_data: src_counter })
    console.log(await p.m("Get"))
    await p.m("Add", { Plus: 3 })
    assert.equal(await p.d("Get"), "3")
    let ao2 = await new AO({
      ar: { port: 4000 },
      aoconnect: optAO(4000),
    }).init(acc[0])
    const p2 = ao2.p(pid)
    assert.equal(await p2.d("Get"), "3")
    await p2.m("Add", { Plus: 2 })
    assert.equal(await p2.d("Get"), "5")
  })

  it("should connect with aoconnect", async () => {
    const { spawn, message, dryrun, assign, result } = _connect({
      MU_URL: `http://localhost:4002`,
      CU_URL: `http://localhost:4004`,
      GATEWAY_URL: `http://localhost:4000`,
    })
    const pid = await spawn({
      module: "Do_Uc2Sju_ffp6Ev0AnLVdPtot15rvMjP-a9VVaA5fM",
      scheduler: "_GQ33BkPtZrqxA84vM8Zk-N2aO0toNNu_C-l-rawrBA",
      tags: [
        {
          name: "Authority",
          value: "eNaLJLsMiWCSWvQKNbk_YT-9ydeWl9lrWwXxLVp9kcg",
        },
      ],
      signer: createDataItemSigner(acc[0].jwk),
    })
    const src_data = `
Handlers.add("Hello", "Hello", function (msg)
  msg.reply({ Data = "Hello, World!" })
end)
`
    await wait(1000)
    const mid = await message({
      process: pid,
      tags: [{ name: "Action", value: "Eval" }],
      data: src_data,
      signer: createDataItemSigner(acc[0].jwk),
    })

    await result({ process: pid, message: mid })
    const res = await dryrun({
      process: pid,
      data: "",
      tags: [{ name: "Action", value: "Hello" }],
      anchor: "1234",
    })
    assert.equal(res.Messages[0].Data, "Hello, World!")
  })

  it("should publish custom modules", async () => {
    const server = new Server({ port: 5000, log: true })
    let ao = new AO({ port: 5000 })
    const { pid: pid2 } = await ao.spwn({
      module: mem.modules.aos2_0_1,
      scheduler,
      tags: { Authority: mu.addr },
    })
    const p2 = ao.p(pid2)
    await ao.wait({ pid: pid2 })
    const { mid } = await ao.load({ pid: pid2, data: src_data })
    console.log("#1", await p2.d("Hello"))
    console.log("#2", await p2.m("Hello"))
    console.log("#3", await p2.d("Hello"))
    const res = await ao.ar.gql.txs({ fields: ["id"], first: 1, next: true })
    for (let v of res.data) console.log(v)
    console.log(await res.next())
    await server.end()
    return
  })
})

describe("ArMem", () => {
  after(() => setTimeout(() => process.exit(), 100))
  it("should upload data with the right format", async () => {
    const server = new Server({ port: 5000, log: true })
    let ao = new AO({ port: 5000 })
    const src = new Src({
      ar: ao.ar,
      dir: resolve(import.meta.dirname, "../src/lua"),
    })
    const wasm_aos2 = await src.upload("aos2_0_1", "wasm")
    console.log(await ao.ar.data(wasm_aos2))
    await server.end()
    return
  })
})
