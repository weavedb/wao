---
name: test-hb
description: "Run HyperBEAM integration tests. Starts a local Erlang HyperBEAM node and tests AOS processes on the full production stack. Use when user says 'test on HyperBEAM', 'run integration tests', or 'test the full stack'. Not for in-memory tests — use /test."
argument-hint: "[test-file]"
disable-model-invocation: true
allowed-tools: Bash, Read, Grep
---

Run HyperBEAM integration tests. These spawn a real Erlang node — different from in-memory AOS tests (`/test`).

## Steps

1. Kill any stale HyperBEAM / beam.smp processes:

```bash
lsof -ti :10000-10010 | xargs -r kill -9 2>/dev/null || true
pkill -f beam.smp 2>/dev/null || true
```

2. Check that HyperBEAM is configured via `.env.hyperbeam`:

```bash
grep '^CWD=' .env.hyperbeam 2>/dev/null | cut -d= -f2-
```

If `.env.hyperbeam` doesn't exist or has no `CWD`, **STOP** and tell the user:

> No HyperBEAM configured. Integration tests require a HyperBEAM installation.
>
> During project creation (`npx wao create`), choose option 1 (clone) or 2 (link).
> Or configure it now:
> ```bash
> echo "CWD=/path/to/your/HyperBEAM" >> .env.hyperbeam
> ```

Then verify the path exists:

```bash
HB_DIR=$(grep '^CWD=' .env.hyperbeam | cut -d= -f2-)
test -d "$HB_DIR/src" && echo "HyperBEAM: OK" || echo "HyperBEAM: MISSING"
```

If `MISSING`, stop and tell the user the CWD path in `.env.hyperbeam` is invalid.

3. Run the tests:

```bash
# Specific file (if argument provided)
yarn test $ARGUMENTS

# Default HyperBEAM tests
yarn test test/hyperbeam.test.js
```

The `HyperBEAM` class reads `.env.hyperbeam` automatically via `dotenv`, so it picks up the correct `CWD`.

4. Report results:
   - Show pass/fail count
   - If HyperBEAM fails to start, check Erlang/OTP is installed (`erl -eval 'halt().'`)
   - If port conflicts, suggest running `/test-hb` again (it cleans up first)
   - Always ensure HyperBEAM process was killed after tests

## Troubleshooting

### HyperBEAM fails to start
- Check Erlang is installed: `erl -eval 'halt().'`
- Requires Erlang/OTP 27+
- Check `.env.hyperbeam` CWD path is valid

### Port already in use after cleanup
- Wait 2-3 seconds for port release, then retry
- Check for zombie processes: `ps aux | grep beam`

### Tests pass locally but fail in CI
- CI may lack Erlang — skip HB tests with env flag
- Ensure HyperBEAM binary is cached in CI
