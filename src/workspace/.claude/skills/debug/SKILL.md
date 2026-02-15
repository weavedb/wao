---
name: debug
description: "Troubleshoot WAO development issues. Diagnoses port conflicts, stale processes, missing dependencies, and common errors. Use when builds stall, tests hang, ports conflict, or user says 'something is broken', 'tests are stuck', or 'help debug'. Not for test failures — use /test or /test-hb to re-run tests."
argument-hint: "[issue-type]"
disable-model-invocation: true
allowed-tools: Bash, Read, Grep
---

Troubleshoot WAO development issues.

If `$ARGUMENTS` is provided, focus on that specific issue type (`timeout`, `port`, `wasm`, `signature`).

## Steps

1. Read `docs/debug.md` for known issues and their fixes.

2. Check port conflicts:

```bash
lsof -ti :10000-10010 2>/dev/null
lsof -ti :6363 2>/dev/null
```

3. Check for stale beam.smp processes:

```bash
ps aux | grep beam.smp | grep -v grep
```

4. Check HyperBEAM directory exists:

```bash
ls ./HyperBEAM/rebar.config 2>/dev/null && echo "HyperBEAM: OK" || echo "HyperBEAM: NOT FOUND"
```

5. Verify wallet exists:

```bash
ls .wallet.json 2>/dev/null && echo "Wallet: OK" || echo "Wallet: NOT FOUND"
```

6. If argument provided, focus on that specific issue type:
   - `timeout` — check ports, kill stale processes, verify HyperBEAM
   - `port` — show all processes on relevant ports
   - `wasm` — check Node.js flags and WASM modules
   - `signature` — check for authority field conflicts, linkify_mode

7. If no specific issue, run a minimal connectivity test:

```bash
pkill -f beam.smp 2>/dev/null || true
lsof -ti :10000-10010 | xargs -r kill -9 2>/dev/null || true
```

8. Cross-reference any error messages with the Common Error Messages table in `docs/debug.md`.

9. Report findings with specific fix recommendations.
