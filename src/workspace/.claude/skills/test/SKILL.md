---
name: test
description: "Run in-memory AOS tests (legacynet units). Fast unit testing — no server needed. Use when testing AOS scripts or after code changes. Not for HyperBEAM integration tests — use /test-hb instead."
argument-hint: "[test-file]"
disable-model-invocation: true
allowed-tools: Bash, Read, Grep
---

Run in-memory AOS tests. For HyperBEAM integration tests, use `/test-hb` instead.

## Examples

```
/test                          # run all tests
/test test/token.test.js       # run specific file
```

## Steps

1. Kill any stale `beam.smp` or HyperBEAM processes that may be lingering on ports 10000-10010:

```bash
lsof -ti :10000-10010 | xargs -r kill -9 2>/dev/null || true
pkill -f beam.smp 2>/dev/null || true
```

2. Run the tests:

```bash
# Specific file (if argument provided)
yarn test $ARGUMENTS

# All tests (if no argument)
yarn test
```

3. Report results:
   - Show pass/fail count
   - If failures, show the failing test names and error messages
   - Suggest fixes if the errors are obvious

## Troubleshooting

### Tests hang indefinitely
- Likely cause: `Send().receive()` in Lua source — does NOT work on genesis-wasm
- Fix: Replace with fire-and-forget `Send()` + separate handler
- Also check for infinite loops in Lua state

### "Cannot find module wao/test"
- Run `yarn install` to ensure dependencies are installed
- Check `package.json` has `"wao"` in dependencies

### WASM memory error
- Node 24+ enables wasm-memory64 by default — DO NOT pass `--experimental-wasm-memory64` (it's rejected with "bad option")
- Node 22: set `NODE_OPTIONS=--experimental-wasm-memory64` before running
- Node.js 22+ required
