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
    assert.deepEqual(
      pick(["key", "key2", "key3", "key4"])(JSON.parse(res.body)),
      {
        key: "1",
        key2: "2",
        key3: { 1: 1, 2: { a: [2, 3] } },
        key4: { a: 3 },
      }
    )

    const { headers: h } = await hb.post({
      path: "/~json@1.0/deserialize",
      body: JSON.stringify({ a: 1, b: [1, 2], c: { d: 4 } }),
    })
    assert.deepEqual(h.a, "1")
    assert.deepEqual(h.b, '"(ao-type-integer) 1", "(ao-type-integer) 2"')
    assert.deepEqual(h["body-keys"], '"c"')

    const { headers: h2 } = await hb.post({
      path: "/~json@1.0/deserialize",
      target: "json",
      json: JSON.stringify({ a: 1, b: [1, 2], c: { d: 4 } }),
    })

    assert.deepEqual(h2.a, "1")
    assert.deepEqual(h2.b, '"(ao-type-integer) 1", "(ao-type-integer) 2"')
    assert.deepEqual(h2["body-keys"], '"c"')
  })
})
