import assert from "assert"
import { describe, it } from "node:test"
import { AO, acc } from "../src/test.js"

const src_data = `
local json = require('json')

Handlers.add("Hello", "Hello", function (msg)
  msg.reply({ Data = json.encode({ Name = "Bob" }) })
end)

Handlers.add("Hello2", "Hello2", function (msg)
  msg.reply({ Data = "Hello, World!", Name = "Bob", Age = "30" })
end)
`

describe("WAO", function () {
  it("should handle data extraction cases", async () => {
    const ao = await new AO().init(acc[0])
    const { p } = await ao.deploy({ src_data })

    // by default it extracts JSON decoded Data
    const out = await p.d("Hello")
    assert.deepEqual(out, { Name: "Bob" })

    // equivalent
    const out2 = await p.d("Hello", { get: true })
    assert.deepEqual(out2, { Name: "Bob" })

    // get string Data
    const out3 = await p.d("Hello2", { get: false })
    assert.equal(out3, "Hello, World!")

    // get a tag
    const out4 = await p.d("Hello2", { get: "Age" })
    assert.equal(out4, "30")

    // get multiple tags
    const out5 = await p.d("Hello2", { get: { name: "Name", age: "Age" } })
    assert.deepEqual(out5, { name: "Bob", age: "30" })
  })
})
