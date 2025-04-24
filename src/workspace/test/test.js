import assert from "assert"
import { describe, it } from "node:test"
import { readFileSync } from "fs"
import { resolve } from "path"

import { AO, acc } from "wao/test" // in-memory test
import { AO as WAO } from "wao" // browser test

const src_data = readFileSync(
  resolve(import.meta.dirname, "../src/counter.lua"),
  "utf8"
)

describe("WAO", function () {
  // lightning-fast testing in memory
  it("should spawn a process and send messages in memory", async () => {
    const ao = await new AO().init(acc[0])
    const { p, pid } = await ao.deploy({ src_data, scheduler: acc[0].addr })

    assert.equal(await p.d("Hello", false), "Hello, World!")
    assert.equal(await p.m("Inc", false), "Incremented!")
    assert.equal(await p.d("Get", false), "1")
  })

  // WAO Proxy must be running on your computer => "npx wao proxy"
  it("should spawn a process and send messages to the browser", async () => {
    // connect with 4000 - 4004 ports
    const ao = await new WAO(4000).init(acc[0])

    // post Scheduler-Location (acc[0])
    const { res, id } = await ao.postScheduler({ url: "http://localhost:4003" })

    const { p, pid } = await ao.deploy({
      src_data,
      scheduler: acc[0].addr, // specify the scheduler locaster address
      module: "WASM32-D8q2OmZ4Mok00sD2Y_6SYEQ7Hjx-6VZ_jl3g", // use wasm32 module
    })

    assert.equal(await p.d("Hello", false), "Hello, World!")
    assert.equal(await p.m("Inc", false), "Incremented!")
    assert.equal(await p.d("Get", false), "1")
  })
})
