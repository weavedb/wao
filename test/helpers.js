import assert from "assert"
import { resolve } from "path"
import { after, describe, it, before, beforeEach } from "node:test"
import { blueprint, mu, connect, acc, scheduler } from "../src/test.js"
import AO from "../src/ao.js"
import TAO from "../src/tao.js"
import AR from "../src/ar.js"
import GQL from "../src/gql.js"
import ArMem from "../src/armem.js"
import { setup, Src, Testnet } from "../src/test.js"
import { tags, wait } from "../src/utils.js"
import Server from "../src/server.js"
import { Bundle, DataItem } from "arbundles"
const { mem, spawn, message, dryrun } = connect()
const [{ signer, jwk }] = acc

const src_data = `
Handlers.add("Hello", "Hello", function (msg)
  msg.reply({ Data = "Hello, World!" })
end)
`

describe("SDK", function () {
  after(() => setTimeout(() => process.exit(), 100))
  it("should execute helper functions", async () => {
    const tn = await new Testnet({ docker: true }).init()
    const { jwk, addr, src, gql, ao, ar } = tn
    const { pid, p } = await ao.deploy({ src_data })
    console.log(await p.m("Hello"))
    /*
    const txs = await gql.txs()
    for (let v of txs) {
      console.log(v.id, "....................")
      console.log(v.tags)
    }*/

    const txs = await gql.txs({ tags: { "App-Name": "ao-localnet bundler" } })
    for (let v of txs) {
      console.log(v.id)
      const di = new Bundle(
        Buffer.from(await ar.arweave.transactions.getData(v.id), "base64"),
      )
      console.log(v.block)
      for (let v2 of di.items) {
        console.log(v2.tags)
      }
    }
    return
  })
})
