---
name: build-module
description: "Build custom WASM64 (Rust) or standalone Lua modules with HyperBEAM integration tests. Iterates until all tests pass 100%. Use when user says 'build the module', 'write the WASM', 'custom Lua module', or 'implement the module'. Not for AOS scripts — use /build-aos."
argument-hint: "<task-id>"
disable-model-invocation: true
allowed-tools: Read, Write, Edit, Bash, Grep, Glob
---

Build custom execution modules (WASM64 Rust or standalone Lua) and HyperBEAM integration tests for a task. Iterates until all tests pass 100%.

## Resolve HyperBEAM path

Read `.env.hyperbeam` and extract the `CWD` value:

```bash
grep '^CWD=' .env.hyperbeam 2>/dev/null | cut -d= -f2-
```

If `.env.hyperbeam` doesn't exist or has no `CWD`, **STOP immediately** and tell the user:

> No HyperBEAM configured. Custom module tests require a HyperBEAM installation.
>
> Set it up by adding a `CWD` entry to `.env.hyperbeam`:
> ```bash
> echo "CWD=/path/to/your/HyperBEAM" >> .env.hyperbeam
> ```
>
> Or re-run `npx wao create` and choose option 1 (clone) or 2 (link).

## Steps

1. Read `tasks.json` and find the task matching `$ARGUMENTS` (task id). Update its status to `"in_progress"`.

2. Read `plan.md` for the module specifications:
   - Module type: WASM64 (Rust) or standalone Lua
   - Actions to handle
   - Response format
   - State management

3. Read the relevant docs:
   - Standalone Lua → `docs/aos-lua.md` (custom Lua section) and the tutorial at `docs/docs/pages/tutorials/custom-lua.mdx`
   - WASM64 Rust → the tutorial at `docs/docs/pages/tutorials/rust-wasm64.mdx`
   - SDK patterns → `docs/wao-sdk.md` (HB class: `cacheBinary`, `cacheScript`, `spawnAOS`, `scheduleAOS`, `computeAOS`, `scheduleLua`, `computeLua`)

4. **If the module type is `module-lua`** — write a standalone Lua module:

   Write the module in `custom-lua/{name}.lua`:

   ```lua
   -- {name}.lua - Custom Lua module for lua@5.3a device
   -- Standalone: no AOS boot code, no Handlers framework.

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

     -- Handle actions
     if action == "MyAction" then
       -- Implementation here
     end

     base.results = {
       outbox = {
         ["1"] = { data = "result" }
       },
       output = ""
     }
     return base
   end
   ```

   Key patterns:
   - Entry point is `compute(base, req, opts)` — NOT `Handlers.add`
   - Action is in `req.body.Action` or `req.body.action` (HTTP lowercases headers)
   - Response goes in `base.results.outbox` — string-keyed table
   - Module-level variables persist between compute calls (Lua VM state is saved)
   - No AOS framework available — no `Handlers`, `ao.send`, `msg.reply`

5. **If the module type is `module-wasm`** — write a Rust WASM64 module:

   Write the module in `custom-wasm/src/lib.rs`:

   ```rust
   #![no_std]
   #![no_main]

   use core::panic::PanicInfo;

   #[panic_handler]
   fn panic(_info: &PanicInfo) -> ! { loop {} }

   static mut BUMP: usize = 4096;

   #[no_mangle]
   pub extern "C" fn malloc(size: usize) -> usize {
       unsafe { let ptr = BUMP; BUMP += size; ptr }
   }

   #[no_mangle]
   pub extern "C" fn free(_ptr: usize) -> usize { 0 }

   // Manual byte copy (WAMR rejects memory.copy)
   unsafe fn copy_bytes(dst: *mut u8, src: *const u8, len: usize) {
       let mut i = 0;
       while i < len { *dst.add(i) = *src.add(i); i += 1; }
   }

   #[no_mangle]
   pub extern "C" fn handle(msg_ptr: usize, _proc_ptr: usize) -> usize {
       // Parse JSON message, handle actions, return JSON response
       // Response format: {"ok":true,"response":{"Output":{"data":""},"Messages":[{"Data":"..."}]}}
       unsafe { /* implementation */ 0 }
   }
   ```

   Key constraints:
   - `#![no_std]` required — WAMR rejects `memory.copy` from std
   - All pointers are `i64` (memory64 WASM)
   - Must export `malloc`, `free`, `handle`
   - Response is JSON with `Messages` array for outbox
   - Use manual byte loops, not memcpy

   Build with:
   ```bash
   cd custom-wasm && cargo +nightly build --target wasm64-unknown-unknown --release -Zbuild-std=core,panic_abort -Zbuild-std-features=panic_immediate_abort
   ```

6. **If the task type is `module-test`** — write HyperBEAM integration tests:

   **For Lua modules** — write `test/{name}-module.test.js`:

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

   describe("{name} custom Lua module", () => {
     let hbeam, hb, moduleId

     before(async () => {
       hbeam = await new HyperBEAM({ reset: true }).ready()
       hb = new HB({ url: hbeam.url })
       await hb.init(hbeam.jwk)
       const luaSrc = readFileSync(luaPath, "utf-8")
       moduleId = await hb.cacheScript(luaSrc, "application/lua")
     })

     after(async () => { if (hbeam) hbeam.kill() })

     it("should spawn process with module", async () => {
       const { pid } = await spawnCustomLua(hb, moduleId)
       assert.ok(pid)
     })

     // Test all actions, state persistence, error handling
   })
   ```

   **For WASM modules** — write `test/{name}-module.test.js`:

   ```js
   import { describe, it, before, after } from "node:test"
   import assert from "node:assert"
   import { readFileSync } from "node:fs"
   import { resolve } from "node:path"
   import { HyperBEAM } from "wao/test"
   import HB from "wao/hb"

   const wasmPath = resolve(import.meta.dirname,
     "../custom-wasm/target/wasm64-unknown-unknown/release/custom_wasm.wasm")

   describe("{name} custom WASM64 module", () => {
     let hbeam, hb

     before(async () => {
       hbeam = await new HyperBEAM({ reset: true }).ready()
       hb = new HB({ url: hbeam.url })
       await hb.init(hbeam.jwk)
     })

     after(async () => { if (hbeam) hbeam.kill() })

     it("should cache and spawn", async () => {
       const wasm = readFileSync(wasmPath)
       const imageId = await hb.cacheBinary(wasm, "application/wasm")
       const { pid } = await hb.spawnAOS({ image: imageId })
       assert.ok(pid)
     })

     // Test all actions via scheduleAOS/computeAOS
   })
   ```

7. Run the tests:

   ```bash
   yarn test test/{name}-module.test.js
   ```

8. If any tests fail:
   - Read the error output carefully
   - For WASM: check compilation output, verify no `memory.copy` instructions
   - For Lua: check `compute` function returns `base` with `results` set
   - Fix the code and re-run
   - Repeat until **100% pass**

9. Update the task status to `"done"` in `tasks.json`.

## Iteration Protocol

For each test-fix cycle:
1. Run tests and capture FULL output
2. Identify the FIRST failing test (fix failures in order)
3. Classify: is the bug in module code or test code?
4. Fix ONE issue per iteration (don't change multiple things at once)
5. Re-run only the failing test first, then full suite
6. If the same error persists after 3 different fix attempts, step back:
   - Re-read the relevant docs (custom module patterns, WASM64/Lua conventions, etc.)
   - Check if the plan itself has a wrong assumption
   - Try a completely different approach
7. Continue iterating until all tests pass — there is no retry limit

Only escalate to the user if:
- The failure is environmental (cargo missing, WASM build tools, port conflict)
- The test requires user input (wallet, external service URL)
- You've tried 10+ iterations with no progress on the same error

## Troubleshooting

### Lua module: "compute not found"
- Ensure the function is named `compute` (global, not local)
- Must take exactly 3 arguments: `(base, req, opts)`

### Lua module: action is nil
- HTTP signatures lowercase headers: check both `req.body.Action` and `req.body.action`
- Also check `req.Action` as a fallback

### WASM: "memory.copy not supported"
- Ensure `#![no_std]` is set in lib.rs
- Use manual byte loops, not any std function that might use memory.copy
- Verify Cargo.toml has `crate-type = ["cdylib"]`

### WASM: compilation fails with wasm64
- Install nightly: `rustup toolchain install nightly`
- Must use `-Zbuild-std=core,panic_abort`
- Target is `wasm64-unknown-unknown` (not wasm32)

### Tests hang on HyperBEAM spawn
- Kill stale beam.smp processes: `pkill -f beam.smp`
- Check `.env.hyperbeam` CWD path is valid
- Requires Erlang/OTP 27+
