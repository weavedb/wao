import assert from "assert"
import { after, describe, it, before, beforeEach } from "node:test"
import { pick } from "ramda"
import HyperBEAM from "../../src/hyperbeam.js"

describe("Hyperbeam Device", function () {
  let hb, hbeam
  before(async () => (hbeam = await new HyperBEAM({ reset: true }).ready()))
  beforeEach(async () => (hb = hbeam.hb))
  after(async () => hbeam.kill())

  it("should test json@1.0", async () => {
    const obj = { key: 1, key2: "2", key3: [1, { a: [2, 3] }], key4: { a: 3 } }
    const res = await hb.p("/~json@1.0/serialize", { ...obj })
    const parsed = JSON.parse(res.body)
    assert.equal(parsed.key, 1)
    assert.equal(parsed.key2, "2")
    // With accept-bundle: true, nested objects are returned inline (not linkified)
    assert.ok(parsed.key3 || parsed["key3+link"], "key3 should exist")
    assert.ok(parsed.key4 || parsed["key4+link"], "key4 should exist")

    // Deserialize JSON body into HyperBEAM message format.
    // With accept-bundle: true, complex values are returned inline in the response.
    const result1 = await hb.post({
      path: "/~json@1.0/deserialize",
      "ao-body-key": "body",
      body: JSON.stringify({ a: 1, b: [1, 2], c: { d: 4 } }),
    })
    // Primitive value in headers or out
    assert.equal(result1.out.a, 1)
    // Complex values are inline in out (not linkified when accept-bundle is true)
    assert.ok(result1.out.b || result1.headers?.["b+link"], "b should exist")
    assert.ok(result1.out.c || result1.headers?.["c+link"], "c should exist")

    const result2 = await hb.post({
      path: "/~json@1.0/deserialize",
      target: "json",
      "ao-body-key": "json",
      json: JSON.stringify({ a: 1, b: [1, 2], c: { d: 4 } }),
    })
    assert.equal(result2.out.a, 1)
    assert.ok(result2.out.b || result2.headers?.["b+link"], "b should exist")
    assert.ok(result2.out.c || result2.headers?.["c+link"], "c should exist")
  })
})
