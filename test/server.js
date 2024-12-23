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

describe("SDK", function () {
  after(() => setTimeout(() => process.exit(), 100))
  it("should publish custom modules", async () => {
    const server = new Server({ port: 5000, log: true })
    let ao = new AO({ port: 5000 })
    /*
    let ao2 = new AO({
      ar: { port: 4000 },
      aoconnect: {
        MU_URL: "http://localhost:4002",
        CU_URL: "http://localhost:4004",
        GATEWAY_URL: "http://localhost:4000",
      },
    })
    const gql = new GQL({ url: "http://localhost:4000/graphql" })
    const authority = "LbDrNpQjzGXGQWmHmWY9LdGJVqOGN0Sa5_1oXOVxrxY"
    const module = "oawtvR9WV7USv7F2Ls-U-6RmcDOAyKckDyWkef7O9Zo"
    const _scheduler = "DhJQ8cOlqSa_C4ZGj5bco2Zv-0xlQ3rVMGBV39E5BO0"

    const { pid: pid0 } = await ao2.spwn({
      module,
      scheduler: _scheduler,
      tags: { Authority: authority },
    })
    await ao2.wait({ pid: pid0 })
    const { mid: mid2 } = await ao2.load({ pid: pid0, data: src_data })
    console.log(mid2)
    const p2 = ao2.p(pid0)
    console.log(await p2.d("Hello"))
    return
    */
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
    //const txs = await fetch(`http://localhost:5003/${pid2}`).then(v => v.json())
    //for (let v of txs.edges) console.log(v.node.message.tags)
    const res = await ao.ar.gql.txs({
      /*block: [1, 3],
      first: 1,
      tags: { Type: "Message" },
      fields: {
        tags: true,
        bundledIn: true,
        id: true,
        owner: ["address"],
        block: ["height"],
        },*/
      fields: ["id"],
      first: 1,
      next: true,
    })
    for (let v of res.data) {
      console.log(v)
    }
    console.log(await res.next())
    return
  })
})

describe("ArMem", () => {
  after(() => setTimeout(() => process.exit(), 100))
  it.only("should upload data with the right format", async () => {
    const server = new Server({ port: 5000, log: true })
    let ao = new AO({ port: 5000 })
    const src = new Src({
      ar: ao.ar,
      dir: resolve(import.meta.dirname, "../src/lua"),
    })
    const wasm_aos2 = await src.upload("aos2_0_1", "wasm")
    console.log(await ao.ar.data(wasm_aos2))
    return
  })
})
