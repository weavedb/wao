---
name: readme
description: "Generate a comprehensive README.md from the project's plan, code, and tests. Use when user says 'write the readme', 'document this', or 'generate docs'. Not for plan.md — use /plan."
disable-model-invocation: true
allowed-tools: Read, Write, Glob, Grep
---

Generate a comprehensive README.md that explains the project thoroughly — what it does, why it exists, how it works, and complete API references for every script, device, and module.

## Performance Notes
- Read every source file before writing — the README must be accurate, not guessed
- Include real code examples extracted from test files
- Document every handler/action, not just the main ones

## Steps

1. Read `plan.md` for the feature overview, architecture decisions, and edge cases.

2. Read **every** AOS script in `src/*.lua`. For each script, extract:
   - Purpose and what problem it solves
   - Every `Handlers.add()` — action name, expected tags, return data, error cases
   - State shape (what globals/tables the script maintains)
   - Interactions with other scripts (cross-process messages)

3. Read **every** test file in `test/*.test.js`. Extract:
   - Real usage examples (deploy, message, dry-run patterns)
   - Edge cases being tested (what fails, what's validated)
   - Multi-user scenarios

4. If `custom-lua/` or `custom-wasm/` exist, read the module source:
   - What the module does (compute function, state handling)
   - Input/output format
   - How it differs from standard AOS

5. If `HyperBEAM/src/dev_*.erl` files exist, read each device:
   - Device purpose and when to use it
   - Exported functions and their signatures
   - State structure and lifecycle
   - HTTP endpoints it exposes

6. If `frontend/src/` exists, read key components and hooks:
   - What the app does from a user perspective
   - How it connects to AOS (which hooks, which actions)
   - Wallet integration (ArConnect)

7. Read `package.json` for available scripts and dependencies.

8. Read `scripts/deploy.js` for deployment details.

9. **Write `README.md`** at the project root with ALL of these sections:

```markdown
# {Project Name}

{2-3 sentence description: what it does, what platform it runs on, who it's for}

## Overview

{Expanded explanation — what problem does this solve? What are the key features?
List each major capability as a bullet point with a brief explanation.}

## How It Works

{Explain the architecture in plain language:
- What happens when a user interacts with the app
- How AOS scripts process messages
- How state is maintained
- If HyperBEAM: how devices fit in the pipeline
- If frontend: how the browser connects to AOS}

## Project Structure

```
{file tree with inline comments for every important file}
```

## Prerequisites

- Node.js 20+
- Arweave wallet (`yarn keygen` to generate)
- {Erlang/OTP 27+ if devices}
- {ArConnect browser extension if frontend}

## Setup

```bash
yarn install
yarn keygen
```

## AOS Script Reference

### {script_name}.lua

{What this script does and why it exists}

**State**: {describe the tables/globals maintained}

| Action | Tags | Returns | Description |
|--------|------|---------|-------------|
| {action} | {required tags} | {return data} | {what it does} |

**Example** (from tests):
```js
{real code example extracted from test file}
```

**Edge Cases**:
- {what happens on invalid input}
- {what happens on insufficient balance, etc.}

{repeat for every script}

## Device Reference (if applicable)

### dev_{name}.erl

{What this device does}

| Function | Input | Output | Description |
|----------|-------|--------|-------------|
| {function} | {params} | {return} | {what it does} |

{repeat for every device}

## Custom Module Reference (if applicable)

### {module_name}

{What this module does, how it differs from standard AOS}

## Frontend (if applicable)

{What the app looks like, what users can do}

### Components
- {component} — {what it renders}

### Hooks
- {hook} — {what state it manages}

## Testing

```bash
yarn test test/aos.test.js          # in-memory AOS (fast)
yarn test test/{script}.test.js     # specific script tests
yarn test test/hyperbeam.test.js    # HyperBEAM integration
cd frontend && npm run test:unit    # frontend vitest
cd frontend && npm run test:e2e     # Playwright E2E
```

## Deploy

```bash
yarn keygen                        # generate wallet (first time only)
yarn deploy                        # all scripts to AO testnet
yarn deploy src/{script}.lua       # single script
yarn deploy --local-hb             # local HyperBEAM
yarn deploy --mainnet              # remote HyperBEAM (production)
```

### How Deploy Works

{Explain: reads src/*.lua, spawns a process per file.
On testnet: Eval message. On HyperBEAM: ao.deploy().
Each script gets its own process ID printed to console.}

### Verify

- [aolink](https://aolink.ar.io/#/entity/{PROCESS_ID}) — inspect AOS process
- [lunar](https://lunar.ar.io/#/process/{PROCESS_ID}) — inspect HyperBEAM process

## Built With

- [WAO](https://docs.wao.eco) — SDK for AO and HyperBEAM
- [AOS](https://ao.arweave.dev) — Lua processes on the AO computer
{add others as relevant}
```

10. After writing, re-read the README and verify:
    - Every AOS action from every script is documented
    - Every device function is documented
    - Code examples are real (from test files), not made up
    - Edge cases are mentioned

11. Update the task status to `"done"` in `tasks.json`.
