import assert from "assert"
import { resolve } from "path"
import { readFileSync, unlinkSync } from "fs"
import { afterEach, after, describe, it, before, beforeEach } from "node:test"
import { blueprint, mu, AO, connect, acc, scheduler } from "../../src/test.js"
import Server from "../../src/server.js"
import MAO from "../../src/ao.js"
import AR from "../../src/ar.js"
import GQL from "../../src/gql.js"
import ArMem from "../../src/armem.js"
import { setup, Src } from "../../src/helpers.js"
import { optAO, optServer, tags, wait } from "../../src/utils.js"

const { mem, spawn, message, dryrun } = connect()
const [{ signer, jwk }] = acc

const src_data = `
Llama = require(".Llama")
Llama.logLevel = 4

Handlers.add("Hello", "Hello", function (msg)
  io.stderr:write("Loaded! Setting prompt")
  Llama.load("/data/" .. msg.ModelID)
  msg.reply({ Data = "true" })
end)

Handlers.add("Ask", "Ask", function (msg)
  Llama.setPrompt(msg.Q)
  io.stderr:write("prompt set!")
  local str = Llama.run(30)
  msg.reply({ Data = str })
end)
`

describe("LLM", function () {
  it("should wait reply from another process", async () => {
    const ao = await new AO().init(acc[0])
    const model = readFileSync(
      resolve(import.meta.dirname, "../../tinyllama.gguf")
    )
    const { id } = await ao.ar.post({ data: model })
    const src = new Src({ dir: resolve(import.meta.dirname, "../../src/lua") })
    const data = src.data("llama", "wasm")
    const { id: modid } = await ao.postModule({ data })
    const { p, pid, err } = await ao.deploy({
      tags: { Extension: "WeaveDrive", Attestor: ao.ar.addr },
      module: modid,
      src_data,
    })
    await ao.attest({ id })
    await p.m("Hello", { ModelID: id })
    console.log(await p.d("Ask", { Q: "How are you?" }, false))
    return
  })
})
