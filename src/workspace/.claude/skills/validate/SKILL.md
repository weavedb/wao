---
name: validate
description: "Post-build validation. Runs tests, checks Lua pitfalls, verifies AOS script coverage, and reports results. Use after building, before deploying, or when user says 'check everything', 'are tests passing', or 'validate'."
argument-hint: "[test-file]"
disable-model-invocation: true
allowed-tools: Bash, Read, Grep, Glob
---

Run post-build validation to verify code quality before declaring a task done.

## Steps

### Gate 1: Unit tests

Run in-memory AOS tests (mainnet WASM device):

```bash
yarn test $ARGUMENTS 2>&1
```

If `$ARGUMENTS` is empty, run the full suite: `yarn test`

If any tests fail, report the failures and stop. Do not proceed until all tests pass.

### Gate 2: HyperBEAM integration

If `HyperBEAM/` directory exists and Erlang is available:

```bash
yarn test test/hyperbeam.test.js 2>&1
```

Skip this gate if HyperBEAM is not set up. Report "skipped (no HyperBEAM)" in the summary.

### Gate 3: Custom module tests

If `custom-lua/` or `custom-wasm/` directory exists and has module files:

```bash
ls test/*-module.test.js 2>/dev/null && yarn test test/*-module.test.js 2>&1
```

Skip this gate if no custom modules. Report "skipped (no custom modules)" in the summary.

### Gate 4: Frontend tests

If `frontend/` directory exists:

```bash
cd frontend && npm run test:unit 2>&1
```

Skip this gate if no frontend. Report "skipped (no frontend)" in the summary.

### Lua pitfalls

Run the validation script:
```bash
bash .claude/skills/validate/scripts/check-lua.sh
```
If the script reports issues, fix them before proceeding.

The script checks for:
- `Send().receive()` — broken on genesis-wasm
- Lowercase Action tags in `Handlers.add`
- `bint()` without `require('.bint')`
- `json.encode`/`json.decode` without `require('json')`

4. Verify AOS script coverage:

   - List all `Handlers.add` calls in `src/*.lua`
   - List all test files in `test/*.test.js`
   - For each script, check that at least one test sends a message with that action
   - Report any AOS scripts without test coverage

5. Check for uncaught error paths:

   - Scripts that read `msg.Tags.*` without nil checks
   - Scripts that use `bint()` on potentially nil values
   - Scripts that modify state without validation

6. Report a summary:

```
## Validation Results

Gate 1 — Unit tests:        X passed, Y failed
Gate 2 — HyperBEAM:         PASS / FAIL / skipped (no HyperBEAM)
Gate 3 — Custom modules:    PASS / FAIL / skipped (no custom modules)
Gate 4 — Frontend tests:    PASS / FAIL / skipped (no frontend)
Lua pitfalls:                none found (or list issues)
AOS script coverage:         X/Y AOS scripts have tests
Uncaught errors:             none found (or list issues)

Overall: PASS / FAIL
```
