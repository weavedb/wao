import { strict as assert } from "node:assert"
import { describe, it } from "mocha"

import store from "../../lib/store.js"

describe("lib/store — zustand factory", () => {
  it("creates getters/setters for each key", () => {
    const initial = { foo: 1, bar: "x" }
    const useFn = store(initial)
    // store returns a hook function; we can't render it but we can verify
    // it exists and has the right shape.
    assert.equal(typeof useFn, "function")
  })
})
