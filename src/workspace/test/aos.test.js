import assert from "assert"
import { describe, it } from "node:test"
import { readFileSync } from "fs"
import { resolve } from "path"

import { AO, acc } from "wao/test"

const src_data = readFileSync(
  resolve(import.meta.dirname, "../src/counter.lua"),
  "utf8"
)

describe("AOS In-Memory", function () {
  it("should deploy and send messages", async () => {
    const ao = await new AO().init(acc[0])
    const { p } = await ao.deploy({ src_data })

    // dry-run (read-only)
    assert.equal(await p.d("Hello", false), "Hello, World!")

    // message (mutates state)
    assert.equal(await p.m("Inc", false), "Incremented!")
    assert.equal(await p.m("Inc", false), "Incremented!")

    // verify state changed
    assert.equal(await p.d("Get", false), "2")
  })

  it("should support multiple users", async () => {
    const ao1 = await new AO().init(acc[0])
    const { p, pid } = await ao1.deploy({ src_data })

    // user 1 increments
    await p.m("Inc", false)
    assert.equal(await p.d("Get", false), "1")

    // user 2 shares the same memory and connects to the same process
    const ao2 = await new AO({ mem: ao1.mem }).init(acc[1])
    const p2 = ao2.p(pid)

    // user 2 increments the same counter
    await p2.m("Inc", false)
    assert.equal(await p2.d("Get", false), "2")
  })

  it("should deploy with template fills", async () => {
    // Create a counter with a configurable greeting
    const template_src = `
local greeting = "<GREETING>"
Handlers.add("Hello", "Hello", function(msg)
  msg.reply({ Data = greeting })
end)
`
    const ao = await new AO().init(acc[0])
    const { p } = await ao.deploy({
      src_data: template_src,
      fills: { GREETING: "Howdy!" }
    })

    const result = await p.d("Hello", false)
    assert.equal(result, "Howdy!")
  })

  it("should verify state isolation between deploys", async () => {
    const ao = await new AO().init(acc[0])

    // Deploy two separate instances
    const { p: p1 } = await ao.deploy({ src_data })
    const { p: p2 } = await ao.deploy({ src_data })

    // Increment only the first
    await p1.m("Inc", false)
    await p1.m("Inc", false)

    // First process should have count 2
    assert.equal(await p1.d("Get", false), "2")

    // Second process should still have count 0
    assert.equal(await p2.d("Get", false), "0")
  })

  it("should use get/check patterns", async () => {
    const ao = await new AO().init(acc[0])
    const { p } = await ao.deploy({ src_data })

    // Use dry-run with direct data return
    const data = await p.d("Hello", false)
    assert.equal(data, "Hello, World!")

    // Mutate and read
    await p.m("Inc", false)
    const count = await p.d("Get", false)
    assert.equal(count, "1")

    // Verify count is a numeric string
    assert.ok(!isNaN(parseInt(count)))
  })
})
