---
name: create-module
description: "Scaffold a new custom module (WASM64 Rust or standalone Lua) with its test file. Use when user says 'create a module', 'new custom module', 'scaffold a WASM module', or 'scaffold a Lua module'. Not for AOS scripts — use /create-aos."
argument-hint: "<module-name> [--lua|--wasm]"
disable-model-invocation: true
allowed-tools: Read, Write, Bash, Grep, Glob
---

Scaffold a new custom execution module with its HyperBEAM integration test.

The argument `$ARGUMENTS` should include the module name and optionally `--lua` or `--wasm` to specify the type. If no type is given, ask the user.

## Steps

0. Check for conflicts:
```bash
test -f "custom-lua/$ARGUMENTS.lua" && echo "WARNING: custom-lua/$ARGUMENTS.lua already exists" || echo "OK"
test -d "custom-wasm/" && test -f "custom-wasm/src/lib.rs" && echo "WARNING: custom-wasm/src/lib.rs already exists" || echo "OK"
```
If the file exists, ask the user: overwrite or choose a different name?

### For Lua modules (`--lua` or default)

1. Create `custom-lua/` directory if it doesn't exist:

```bash
mkdir -p custom-lua
```

2. Create `custom-lua/{name}.lua`:

```lua
-- {name}.lua - Custom Lua module for lua@5.3a device
-- Standalone: no AOS boot code, no Handlers framework.
-- Implements a {Name} with basic actions.

local state = {}

local function intstr(n)
  if n == math.floor(n) then return string.format("%d", n) end
  return tostring(n)
end

function compute(base, req, opts)
  local action = nil
  if type(req) == "table" then
    if type(req.body) == "table" then
      action = req.body.Action or req.body.action
    end
    if not action then
      action = req.Action or req.action
    end
  end

  local result = "ok"

  if action == "Get" then
    -- Return current state
    result = "state"
  end

  base.results = {
    outbox = {
      ["1"] = { data = result }
    },
    output = ""
  }
  return base
end
```

3. Create `test/{name}-module.test.js`:

```js
import { describe, it, before, after } from "node:test"
import assert from "node:assert"
import { readFileSync } from "node:fs"
import { resolve } from "node:path"
import { HyperBEAM } from "wao/test"
import HB from "wao/hb"

const luaPath = resolve(import.meta.dirname, "../custom-lua/{name}.lua")

async function spawnCustomLua(hb, moduleId) {
  return hb.spawn({
    "data-protocol": "ao",
    variant: "ao.TN.1",
    module: moduleId,
    "execution-device": "lua@5.3a",
    "push-device": "push@1.0",
    "patch-from": "/results/outbox",
  })
}

describe("{Name} Custom Lua Module", function () {
  let hbeam, hb, moduleId

  before(async () => {
    hbeam = await new HyperBEAM({ reset: true }).ready()
    hb = new HB({ url: hbeam.url })
    await hb.init(hbeam.jwk)
    const luaSrc = readFileSync(luaPath, "utf-8")
    moduleId = await hb.cacheScript(luaSrc, "application/lua")
  })

  after(async () => { if (hbeam) hbeam.kill() })

  it("should spawn and get initial state", async () => {
    const { pid } = await spawnCustomLua(hb, moduleId)
    assert.ok(pid, "should get process ID")

    const get = await hb.scheduleLua({ pid, action: "Get" })
    const result = await hb.computeLua({ pid, slot: get.slot })
    assert.ok(result)
  })
})
```

### For WASM64 modules (`--wasm`)

1. Create the Rust project if it doesn't exist:

```bash
test -d custom-wasm || cargo new custom-wasm --lib
```

2. Write `custom-wasm/Cargo.toml`:

```toml
[package]
name = "custom-wasm"
version = "0.1.0"
edition = "2021"

[lib]
crate-type = ["cdylib"]

[profile.release]
opt-level = "z"
lto = true
strip = true
```

3. Write `custom-wasm/src/lib.rs` with the `#![no_std]` counter template from `docs/docs/pages/tutorials/rust-wasm64.mdx`.

4. Build:

```bash
cd custom-wasm && cargo +nightly build --target wasm64-unknown-unknown --release -Zbuild-std=core,panic_abort -Zbuild-std-features=panic_immediate_abort
```

5. Create `test/{name}-module.test.js` using the WASM test template (with `cacheBinary` + `spawnAOS` + `scheduleAOS` + `computeAOS`).

### Final steps

6. Run the test:

```bash
yarn test test/{name}-module.test.js
```

7. Report whether the test passes. If it fails, fix and re-run.
