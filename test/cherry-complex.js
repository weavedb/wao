import assert from "assert"
import { describe, it, before } from "node:test" // Add before here
import { AO, acc } from "../src/test.js"

const src_data = `
local json = require('json')

Handlers.add("Hello", "Hello", function (msg)
  msg.reply({ Data = json.encode({ Name = "Bob" }) })
end)

Handlers.add("Hello2", "Hello2", function (msg)
  msg.reply({ Data = "Hello, World!", Name = "Bob", Age = "30" })
end)

Handlers.add("MultiTags", "MultiTags", function (msg)
  msg.reply({ 
    Data = "Main data", 
    FirstName = "John", 
    LastName = "Doe", 
    Age = "42",
    Score = "95.5",
    JSON = json.encode({ complex = "data", nested = { value = true } })
  })
end)

Handlers.add("NestedJSON", "NestedJSON", function (msg)
  msg.reply({ 
    Data = json.encode({ 
      user = { 
        name = "Alice", 
        details = { age = 28, country = "Wonderland" } 
      },
      settings = { theme = "dark" }
    }) 
  })
end)
`

describe("WAO Cherry-Picking Tests", function () {
  let ao, p

  // Setup - run before all tests in this describe block
  it("should set up the test environment", async () => {
    ao = await new AO().init(acc[0])
    const result = await ao.deploy({ src_data })
    p = result.p
  })

  describe("Basic Data Extraction", () => {
    it("should extract JSON-decoded Data by default", async () => {
      const out = await p.d("Hello")
      assert.deepEqual(out, { Name: "Bob" })
    })

    it("should extract JSON-decoded Data with explicit get:true", async () => {
      const out = await p.d("Hello", { get: true })
      assert.deepEqual(out, { Name: "Bob" })
    })

    it("should extract raw string Data with get:false", async () => {
      const out = await p.d("Hello2", { get: false })
      assert.equal(out, "Hello, World!")
    })
  })

  describe("Tag Extraction", () => {
    it("should extract a single tag value by name", async () => {
      const out = await p.d("Hello2", { get: "Age" })
      assert.equal(out, "30")
    })

    it("should extract multiple tags using an object mapping", async () => {
      const out = await p.d("Hello2", { get: { name: "Name", age: "Age" } })
      assert.deepEqual(out, { name: "Bob", age: "30" })
    })

    it("should handle multiple tags with complex data", async () => {
      const out = await p.d("MultiTags", {
        get: {
          first: "FirstName",
          last: "LastName",
          age: "Age",
          score: "Score",
        },
      })
      assert.deepEqual(out, {
        first: "John",
        last: "Doe",
        age: "42",
        score: "95.5",
      })
    })
  })

  describe("Advanced Scenarios", () => {
    it("should parse nested JSON data", async () => {
      const out = await p.d("NestedJSON")
      assert.deepEqual(out.user.details, { age: 28, country: "Wonderland" })
      assert.equal(out.settings.theme, "dark")
    })

    it("should handle JSON in tags", async () => {
      const out = await p.d("MultiTags", {
        get: { complexData: "JSON" },
        json: true,
      })
      assert.deepEqual(out, {
        complexData: { complex: "data", nested: { value: true } },
      })
    })

    it("should return null for non-existent tags", async () => {
      const out = await p.d("Hello2", { get: "NonExistentTag" })
      assert.strictEqual(out, null)
    })
  })
})
