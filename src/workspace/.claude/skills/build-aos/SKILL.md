---
name: build-aos
description: "Write AOS scripts and comprehensive in-memory tests. Iterates until all tests pass 100%. Use when user says 'build the AOS script', 'write the Lua code', or 'implement the handler'. Not for scaffolding — use /create-aos."
argument-hint: "<task-id>"
disable-model-invocation: true
allowed-tools: Read, Write, Edit, Bash, Grep, Glob
---

Build AOS scripts and/or in-memory tests for a task. Iterates until all tests pass 100%.

## Steps

1. Read `tasks.json` and find the task matching `$ARGUMENTS` (task id). Update its status to `"in_progress"`.

2. Read `plan.md` for the AOS script specifications:
   - Script names and actions
   - Input tags and expected values
   - State management (what gets stored, how)
   - Reply format (JSON, plain text, tags)
   - Edge cases to handle

3. Read `docs/aos-lua.md` for AOS Lua patterns and conventions.

3.5. Verify prerequisites:
- Check `src/` directory exists: `ls src/ 2>/dev/null`
- If `src/` doesn't exist, create it: `mkdir -p src`

4. **If the task type is `aos`** — write AOS scripts:

   Write script files in `src/` following the plan specs. For each script:
   - Use `Handlers.add(name, pattern, callback)` pattern
   - Action tags must be **uppercase** (e.g., `"Transfer"`, not `"transfer"`)
   - Validate all input: nil checks on `msg.Tags.*`, type validation, boundary values
   - Handle error cases: missing fields → reply with error, unauthorized → reject
   - Use `ao.send()` for fire-and-forget (NOT `Send().receive()` — broken on genesis-wasm)
   - Require dependencies at top: `local json = require('json')`, `local bint = require('.bint')(256)`
   - State stored in module-level locals

5. **If the task type is `aos-test`** — write in-memory tests:

   Write test files in `test/` using this pattern:

   ```js
   import { describe, it, before, after } from "node:test"
   import assert from "node:assert"
   import { AO, acc } from "wao/test"
   import { readFileSync } from "fs"

   const src_data = readFileSync("src/{name}.lua", "utf8")

   describe("{feature} tests", () => {
     let ao, p

     before(async () => {
       ao = await new AO().init(acc[0])
       const result = await ao.deploy({ src_data })
       p = result.p
     })

     it("should handle happy path", async () => {
       const { res } = await p.msg("Action", { ... })
       assert.equal(res.Output.data, expected)
     })

     it("should reject missing fields", async () => {
       // test error paths
     })

     // Minimum 10+ test cases per script:
     // - Happy path (valid input, expected output)
     // - Error paths (missing tags, invalid values)
     // - Boundary values (zero, max, empty string)
     // - Multi-user (different acc[0], acc[1], acc[2])
     // - State isolation between deploys
   })
   ```

   For multi-user tests, share `ao.mem` between AO instances:
   ```js
   const ao2 = await new AO({ mem: ao.mem }).init(acc[1])
   ```

6. Run the tests:

   ```bash
   yarn test test/{name}.test.js
   ```

7. If any tests fail:
   - Read the error output carefully
   - Identify whether the bug is in the AOS script or the test
   - Fix the code
   - Re-run the tests
   - Repeat until **100% pass**

8. Update the task status to `"done"` in `tasks.json`.

## Troubleshooting

### Tests fail with "Action not found"
- Action tags must be UPPERCASE: `"Transfer"` not `"transfer"`
- Check that `Handlers.add` name matches the Action tag exactly

### State resets between test cases
- Each `ao.deploy()` creates a fresh process — state doesn't carry over
- Use `before()` to deploy once, then test sequentially

### bint arithmetic errors
- Always wrap in `tostring()` when replying: `tostring(bint(x) + bint(y))`
- Use `pcall(bint, value)` for safe parsing of user input

## Iteration Protocol

For each test-fix cycle:
1. Run tests and capture FULL output
2. Identify the FIRST failing test (fix failures in order)
3. Classify: is the bug in source code or test code?
4. Fix ONE issue per iteration (don't change multiple things at once)
5. Re-run only the failing test first, then full suite
6. If the same error persists after 3 different fix attempts, step back:
   - Re-read the relevant docs (AOS patterns, Lua conventions, etc.)
   - Check if the plan itself has a wrong assumption
   - Try a completely different approach
7. Continue iterating until all tests pass — there is no retry limit

Only escalate to the user if:
- The failure is environmental (missing binary, permissions, port conflict)
- The test requires user input (wallet, external service URL)
- You've tried 10+ iterations with no progress on the same error
