# Troubleshooting Guide

Known issues and fixes for WAO development.

## Critical Issues

### Send().receive() Doesn't Work

**Problem:** `Send({Target=pid, Action="X"}).receive()` hangs or fails in AOS handlers.

**Cause:** The external CU (genesis-wasm-server) is a single-pass evaluator. It uses `@permaweb/ao-loader` which returns immediately — no coroutine/yield support for blocking `receive()`.

**Fix:** Use fire-and-forget `Send()` and handle responses in separate Handlers:

```lua
-- WRONG: Will hang/fail
Handlers.add("Bad", "Bad", function(msg)
  local res = Send({ Target = pid, Action = "Query" }).receive()
  msg.reply({ Data = res.Data })
end)

-- CORRECT: Fire-and-forget + separate handler
Handlers.add("Query", "Query", function(msg)
  Send({ Target = pid, Action = "GetData", ["X-Reply-To"] = msg.From })
end)

Handlers.add("DataResponse", "DataResponse", function(msg)
  -- Handle the response in a separate handler
  State.lastData = msg.Data
end)
```

### Action Tag Case Sensitivity

**Problem:** Handlers don't match incoming messages.

**Cause:** AOS requires uppercase `Action` tag, not lowercase `action`.

**Fix:** Always use uppercase `Action` in JS:

```js
// WRONG
await p.msg("inc")  // lowercase action won't match Handlers.add("Inc", ...)

// CORRECT
await p.msg("Inc")  // matches Handlers.add("Inc", "Inc", ...)
```

### Authority Field Conflict

**Problem:** Spawn fails or signatures break when including `authority` in tags.

**Cause:** `@authority` is an RFC 9421 derived component in HTTP Message Signatures. Including `authority` as a tag conflicts with the signature system.

**Fix:** Don't include `authority` in spawn tags. It's handled automatically.

---

## Port & Process Issues

### Port Conflicts

**Problem:** HyperBEAM fails to start or tests hang.

**Fix:** Kill stale processes:

```bash
# Kill stale beam.smp (Erlang) processes
pkill -f beam.smp 2>/dev/null || true

# Kill processes on HyperBEAM ports
lsof -ti :10000-10010 | xargs -r kill -9 2>/dev/null || true

# Kill genesis-wasm CU
lsof -ti :6363 | xargs -r kill -9 2>/dev/null || true
```

### Multiple HyperBEAM Instances

**Problem:** Second HyperBEAM instance fails to start.

**Cause:** Each instance needs a unique Erlang node name and port.

**Fix:** Use different ports — node names are auto-assigned (`hb_{port}`):

```js
const hbeam1 = await new HyperBEAM({ port: 10001 }).ready()
const hbeam2 = await new HyperBEAM({ port: 10002 }).ready()
```

### HyperBEAM Timeout

**Problem:** `hbeam.ready()` times out.

**Causes:**
- Stale beam.smp processes blocking the port
- rebar3 compilation issues
- Missing HyperBEAM directory

**Fix:**
1. Kill stale processes (see Port Conflicts above)
2. Verify `./HyperBEAM` directory exists
3. Increase timeout: `hbeam.ready(120000)`
4. Always call `hbeam.kill()` in `after()`:

```js
after(async () => {
  if (hbeam) hbeam.kill()
})
```

---

## Signature & Encoding Issues

### Linkification Breaks Signatures

**Problem:** Signature verification fails on nested messages.

**Cause:** Without `linkify_mode: false`, nested maps get hash-linked. The verifier sees different data than what was signed.

**Fix:** Set `linkify_mode: false` in test configurations.

### Multiple Matches Error

**Problem:** `multiple_matches` error when scheduling messages.

**Cause:** Two messages with identical content produce the same hash/ID, creating a conflict.

**Fix:** Add a nonce or timestamp to make messages unique:

```js
await hb.schedule({
  pid,
  tags: { Action: "Inc", Nonce: Date.now().toString() },
})
```

---

## WASM Issues

### WASM Memory Error

**Problem:** `WebAssembly.Memory` errors or WASM module fails to load.

**Fix:** Node 24+ enables wasm-memory64 by default (and rejects the experimental flag at startup with "bad option"). On Node 22 or older, prefix with `--experimental-wasm-memory64`:

```bash
# Node 24+: no flag needed
node --test test/aos.test.js

# Node 22:
node --experimental-wasm-memory64 --test test/aos.test.js
```

The `yarn test` script ships without the flag (Node 24+ default); set `NODE_OPTIONS=--experimental-wasm-memory64` if you're on older Node.

### Genesis-WASM Server

**Problem:** Process spawns but compute returns nothing.

**Cause:** Genesis-wasm CU server not running at port 6363.

**Fix:**
1. Enable genesis_wasm: `new HyperBEAM({ genesis_wasm: true })`
2. Check server: `curl http://localhost:6363`
3. Check port: `lsof -i :6363`

---

## Process Issues

### Process Not Found

**Problem:** Messages to process fail with "not found".

**Causes:**
- `src_data` path incorrect in `readFileSync`
- Process didn't deploy successfully

**Fix:**
1. Check path resolution: `resolve(import.meta.dirname, "../src/file.lua")`
2. Check deploy result: `const { err, pid } = await ao.deploy({ src_data })`
3. Ensure `err` is null/undefined

### Atom Registration Error

**Problem:** `badarg in list_to_atom` error in HyperBEAM.

**Cause:** Erlang atom not pre-registered. Atoms created at runtime from untrusted input can crash.

**Fix:** Add the atom to `preRegisterAtoms` in HyperBEAM configuration, or use binary strings instead of atoms in device code.

---

## Common Error Messages

| Error | Cause | Fix |
|-------|-------|-----|
| `Message is not valid` | HTTPSig verification failed | Check `authority` field, remove from tags |
| `invalid_commitment` | Signed field name conflicts with RFC 9421 | Don't use reserved field names in tags |
| `fetch failed` | HyperBEAM crashed or not started | Kill stale beam.smp, restart |
| `ECONNREFUSED :10001` | HyperBEAM not running | Start with `new HyperBEAM().ready()` |
| `ECONNREFUSED :6363` | Genesis-wasm CU not running | Enable `genesis_wasm: true` |
| `badarg in list_to_atom` | Atom not pre-registered | Add to preRegisterAtoms |
| `multiple_matches` | Duplicate message content | Add nonce to messages |
| `timeout` | Process or server hung | Kill stale processes, increase timeout |
| `WASM memory error` | Missing memory64 flag | Add `--experimental-wasm-memory64` |
| `Process not found` | Bad pid or deploy failed | Check deploy result, verify pid |
| `Insufficient balance` | Payment balance too low | Top up via simple-pay or p4 |
| `Not signed` | Missing wallet/JWK | Call `.init(jwk)` before operations |

---

## Debug Checklist

When tests fail, check in order:

1. **Ports clear?** `lsof -ti :10000-10010 | xargs -r kill -9`
2. **beam.smp killed?** `pkill -f beam.smp`
3. **WASM flag?** Node 24+ default-on; on Node 22 add `--experimental-wasm-memory64` (and on Node 24+ remove it — it's rejected)
4. **HyperBEAM dir?** `ls ./HyperBEAM` exists
5. **Wallet exists?** `.wallet.json` present (auto-generated)
6. **hbeam.kill() in after()?** Always clean up
7. **Action uppercase?** `"Inc"` not `"inc"`
8. **No Send().receive()?** Use fire-and-forget instead
9. **src_data path correct?** `resolve(import.meta.dirname, "../src/file.lua")`
10. **Deploy succeeded?** Check `err` field in deploy result
