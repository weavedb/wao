---
name: team
description: "Set up an agent team for parallel WAO development. Creates coordinated teammates for building features, researching patterns, debugging, or cross-layer work. Use when user says 'work in parallel', 'split the work', 'team up', or 'use multiple agents'."
argument-hint: "<workflow>"
disable-model-invocation: true
---

Set up an agent team for parallel WAO development.

The argument `$ARGUMENTS` describes the workflow. Common workflows:

## Examples

```
/team build      # parallel feature development
/team debug      # competing hypothesis debugging
/team device     # cross-layer Erlang + JS development
/team research   # one researches docs, another implements
/team module     # custom module development (WASM + Lua)
```

## Workflows

### `build` — Parallel Feature Development

Create an agent team with teammates that own separate files:
- **Lua teammate**: builds AOS scripts in `src/`, reads `docs/aos-lua.md` first
- **Test teammate**: writes tests in `test/`, reads `docs/wao-sdk.md` first
- **Integration teammate**: verifies everything works together, runs `yarn test`

Each teammate should own distinct files to avoid conflicts. The Lua teammate never edits test files, the test teammate never edits Lua files.

Require plan approval before teammates start implementing.

**File ownership per teammate:**
- Lua: `src/*.lua` only
- Test: `test/*.test.js` only
- Integration: runs `yarn test`, reports results, doesn't edit files

**Acceptance criteria per task:**
- Lua: AOS script compiles, follows patterns from `docs/aos-lua.md`
- Test: all assertions pass when run with `yarn test test/{file}.test.js`
- Integration: full `yarn test` passes with no regressions

### `research` — Research and Build

Create an agent team:
- **Researcher**: reads the relevant docs (`docs/aos-lua.md`, `docs/wao-sdk.md`, `docs/hyperbeam-devices.md`) and reports patterns, gotchas, and examples back to the team
- **Builder**: implements the feature based on the researcher's findings
- **Reviewer**: reviews the implementation for correctness and WAO best practices

### `debug` — Competing Hypotheses

Create an agent team with 3-4 teammates that each investigate a different hypothesis:
- One checks port conflicts and stale processes (`lsof`, `pkill`)
- One reads `docs/debug.md` and matches error messages against known issues
- One examines the test code and Lua source for logic errors
- One checks HyperBEAM configuration and `.env.hyperbeam`

Have teammates challenge each other's findings. The theory that survives debate is most likely the root cause.

### `device` — Cross-Layer Device Development

Create an agent team for building a HyperBEAM device:
- **Erlang teammate**: writes the device module in `HyperBEAM/src/`, reads `docs/hyperbeam-dev.md`
- **JS teammate**: writes the integration test in `test/`, reads `docs/wao-sdk.md`
- **Compile teammate**: handles `cd HyperBEAM && rebar3 compile`, fixes compilation errors

**File ownership:**
- Erlang: `HyperBEAM/src/dev_*.erl` only
- JS: `test/*.device.test.js` only
- Compile: `HyperBEAM/` (compile and fix only)

### `review` — Parallel Code Review

Create an agent team with 3 reviewers:
- **Security reviewer**: checks for wallet exposure, injection, auth bypass
- **Correctness reviewer**: validates AOS script logic, state management, edge cases
- **Test coverage reviewer**: ensures all AOS scripts have tests, edge cases covered

### `module` — Custom Module Development

Create an agent team for building custom WASM64 or Lua modules:
- **Module teammate**: writes the custom module source (custom-wasm/src/lib.rs or custom-lua/{name}.lua)
- **Test teammate**: writes HyperBEAM integration tests in `test/`, reads SDK docs for cacheBinary/cacheScript patterns
- **Build teammate**: handles compilation (cargo build for WASM, HyperBEAM startup for Lua)

**File ownership:**
- Module: `custom-wasm/` or `custom-lua/` only
- Test: `test/*-module.test.js` only
- Build: runs compilation and HB processes, doesn't edit source

## Guidelines

- Assign 5-6 tasks per teammate to keep everyone productive
- Each teammate should own separate files — never have two teammates edit the same file
- Workers report back test results, not just "done"
- Use delegate mode (Shift+Tab) if you want the lead to only coordinate, not code
- Teammates load CLAUDE.md and docs/ automatically — give task-specific context in spawn prompts
- Use Shift+Up/Down to cycle between teammates and message them directly
- Always clean up the team when done (tell the lead to clean up)
