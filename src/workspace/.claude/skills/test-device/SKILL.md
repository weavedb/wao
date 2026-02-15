---
name: test-device
description: "Test HyperBEAM Erlang devices with WAO SDK integration tests. Launches real HyperBEAM and tests device via HTTP. Use when user says 'test the device', 'run device tests', or 'verify the Erlang module'. Not for eunit — use /build-device."
argument-hint: "<task-id>"
disable-model-invocation: true
allowed-tools: Read, Write, Edit, Bash, Grep, Glob
---

Write and run WAO SDK integration tests for HyperBEAM Erlang devices. Launches a real HyperBEAM node and tests devices via HTTP.

## Resolve HyperBEAM path

Read `.env.hyperbeam` and extract the `CWD` value:

```bash
grep '^CWD=' .env.hyperbeam 2>/dev/null | cut -d= -f2-
```

If `.env.hyperbeam` doesn't exist or has no `CWD`, **STOP immediately** and tell the user:

> No HyperBEAM configured. Device integration tests require a HyperBEAM installation.
>
> Set it up by adding a `CWD` entry to `.env.hyperbeam`:
> ```bash
> echo "CWD=/path/to/your/HyperBEAM" >> .env.hyperbeam
> ```

Then verify the directory exists:

```bash
test -d "$HB_DIR/src" && echo "HyperBEAM: OK" || echo "HyperBEAM: MISSING"
```

If `MISSING`, stop and tell the user the path in `.env.hyperbeam` is invalid.

Do NOT mark the task as done. Set the task status to `"pending"` and stop.

## Steps

1. Read `tasks.json` and find the task matching `$ARGUMENTS` (task id). Update its status to `"in_progress"`.

2. Kill stale HyperBEAM processes:

   ```bash
   pkill -f beam.smp 2>/dev/null || true
   ```

3. Read `docs/wao-sdk.md` for HB class patterns (spawn, schedule, compute).

4. **Write integration tests** in `test/{name}-device.test.js`:

   The `HyperBEAM` class reads `.env.hyperbeam` automatically via `dotenv`, so it will find the correct `CWD`. No need to pass it manually.

   ```js
   import { describe, it, before, after } from "node:test"
   import assert from "node:assert"
   import { HB, HyperBEAM, acc } from "wao/test"

   describe("{name} device integration", () => {
     let hbeam, hb

     before(async () => {
       hbeam = await new HyperBEAM({ reset: true }).ready()
       hb = hbeam.hb
     })

     after(async () => {
       if (hbeam) await hbeam.kill()
     })

     it("should spawn process with device", async () => {
       const { id } = await hb.spawn({
         "execution-device": "dev_{name}@1.0",
       })
       assert.ok(id)
     })

     it("should handle action via device", async () => {
       const { id } = await hb.spawn({
         "execution-device": "dev_{name}@1.0",
       })
       const result = await hb.schedule({
         process: id,
         action: "MyAction",
         tags: { /* ... */ },
       })
       assert.ok(result)
     })

     // Test all device operations:
     // - Spawn with device
     // - Each exported action
     // - Error handling (invalid action, bad input)
     // - State persistence across messages
     // - Device stacks (if applicable)
   })
   ```

5. Run the tests:

   ```bash
   yarn test test/{name}-device.test.js
   ```

6. If any tests fail:
   - Read the error output
   - Check if HyperBEAM started correctly
   - Fix the test or device code
   - Kill stale processes and re-run
   - Repeat until **100% pass**

7. Always ensure HyperBEAM is killed after tests (check `after()` hook).

8. Update the task status to `"done"` in `tasks.json`.
