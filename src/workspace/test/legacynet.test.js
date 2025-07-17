import assert from "assert"
import { describe, it } from "node:test"
import { readFileSync } from "fs"
import { resolve } from "path"

import { AO, acc } from "wao/test" // in-memory test

const src_data = readFileSync(
  resolve(import.meta.dirname, "../src/counter.lua"),
  "utf8"
)

describe("WAO Legacynet", function () {
  // lightning-fast testing in memory
  it("should spawn a process and send messages in memory", async () => {
    const ao = await new AO().init(acc[0])
    const { p, pid } = await ao.deploy({ src_data, scheduler: acc[0].addr })

    assert.equal(await p.d("Hello", false), "Hello, World!")
    assert.equal(await p.m("Inc", false), "Incremented!")
    assert.equal(await p.d("Get", false), "1")
  })
})
