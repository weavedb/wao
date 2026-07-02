import { strict as assert } from "node:assert"
import { describe, it } from "mocha"

import { bfiles, bps, default_projects } from "../../lib/guide.js"

describe("lib/guide — bundled docs structure", () => {
  it("bfiles is a non-empty array", () => {
    assert.ok(Array.isArray(bfiles))
    assert.ok(bfiles.length > 0)
  })

  it("each bfile has dir/name/id/path/pid", () => {
    for (const f of bfiles) {
      assert.equal(typeof f.name, "string")
      assert.equal(typeof f.id, "string")
      assert.equal(typeof f.path, "string")
      assert.equal(typeof f.pid, "string")
    }
  })

  it("guide directories include 'tutorials' and 'api'", () => {
    const dirs = bfiles.filter(f => f.dir).map(f => f.name)
    assert.ok(dirs.includes("tutorials"))
    assert.ok(dirs.includes("api"))
  })

  it("default_projects is an array of project descriptors", () => {
    assert.ok(Array.isArray(default_projects))
  })

  it("bps (built-in process scripts) is an array", () => {
    assert.ok(Array.isArray(bps))
  })
})
