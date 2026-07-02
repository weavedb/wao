# WAO App

This is a WAO application — a decentralized app built on AOS (ao Standard) and HyperBEAM.

See @package.json for available commands.

## Auto-Start

On session start, the SessionStart hook checks for `tasks.json` and `plan.md`:
- **If they exist**: Resume the build — read `tasks.json`, find the first pending/in_progress task, and continue the `/build` workflow automatically. Read `.claude/skills/build/SKILL.md` and follow its resume instructions.
- **If they don't exist**: Wait for the user to run `/build <feature>` to start a new build.

## Stack

- **AOS scripts** in `src/` — Lua process logic
- **Custom modules** in `custom-lua/` or `custom-wasm/` — standalone Lua or WASM64 (Rust) execution modules
- **JS tests** in `test/` — Node.js built-in test runner with `wao/test`
- **Deploy script** in `scripts/deploy.js` — deploys Lua source to testnet, local HB, or remote HB
- **HyperBEAM** — local HyperBEAM node (path configured in `.env.hyperbeam` CWD)
- **Frontend** in `frontend/` — Vite + React SPA with `wao/web` (optional)

## Two Ways to Test AOS

### In-Memory AOS (legacynet units)
Runs AOS WASM directly in Node.js. No server, no Erlang. Instant. Use for fast unit testing of AOS script logic.

```js
import { AO, acc } from "wao/test"
const ao = await new AO().init(acc[0])
const { p } = await ao.deploy({ src_data })
```

### HyperBEAM AOS
Spawns a real Erlang HyperBEAM node. AOS processes run on the HyperBEAM stack via `genesis-wasm` device. Messages go through HTTP with slot-based scheduling. Use for integration testing the full production stack.

```js
import { HyperBEAM } from "wao/test"
const hbeam = await new HyperBEAM({ reset: true }).ready()
const hb = hbeam.hb  // HB HTTP client
```

You can also run AOS through HyperBEAM by passing `hb` to AO:
```js
import { AO } from "wao"
const ao = await new AO({ hb: hbeam.url }).init(jwk)
```

## Persistent Workflow

The build workflow is file-based so any session can pick up where another left off:

- **`plan.md`** — feature plan with AOS scripts, edge cases, test scenarios, validation gates
- **`tasks.json`** — ordered task list with status, files, details, and done criteria

On session start, check if `plan.md` and `tasks.json` exist. If they do, read them and continue from the first pending task. Update task status as you work.

## Validation Gates

All gates must pass before completing a task:

1. **Unit tests** — `yarn test` — in-memory AOS using mainnet WASM device (sub-second)
2. **HyperBEAM integration** — `yarn test test/hyperbeam.test.js` — full Erlang stack (skip if no HyperBEAM)
3. **Frontend tests** — `cd frontend && npm run test:unit` — vitest components (skip if no frontend)

The `TaskCompleted` hook enforces these gates automatically. Use `/validate` to run them manually.

## Dashboard

Real-time build progress dashboard with SSE updates.

```bash
yarn start          # API server (:3333) + Vite dev server (:5174)
yarn start:api      # API server only (:3333)
```

- **API**: `http://localhost:3333` — serves `/api/progress` (JSON) and `/api/events` (SSE)
- **UI**: `http://localhost:5174` — Vite dev server with proxy to API
- **MCP**: `get_progress` and `open_dashboard` tools via @.mcp.json (auto-discovered by Claude Code)

The API server watches `tasks.json` and `plan.md` and pushes changes to connected dashboards via SSE. The dashboard falls back to 3s polling if SSE is unavailable.

## Commands

```bash
yarn start                        # dashboard (API + Vite)
yarn test                         # all unit tests
yarn test test/aos.test.js        # in-memory AOS (fast)
yarn test test/hyperbeam.test.js  # HyperBEAM integration
cd frontend && npm run test:unit  # frontend vitest
cd frontend && npm run test:e2e   # frontend Playwright
yarn deploy src/<name>.lua              # testnet (default)
yarn deploy --local-hb                   # local HyperBEAM (genesis-wasm)
yarn deploy --local-hb --lua             # local HyperBEAM (Lua mode)
yarn deploy --mainnet                    # remote HyperBEAM (push-1)
yarn deploy --mainnet --lua              # remote HyperBEAM (Lua mode)
```

Runs: `node --test --test-concurrency=1` (Node 24+; on Node 22 prefix with `--experimental-wasm-memory64`)

## Deployment Targets

| Target | Command | Config |
|--------|---------|--------|
| AO Testnet | `yarn deploy` | `new AO().init(jwk)` via aoconnect |
| Local HB (Legacynet) | `yarn deploy --local-hb` | `new AO({ hb: "http://localhost:10001" }).init(jwk)` |
| Local HB (Lua) | `yarn deploy --local-hb --lua` | `new AO({ hb: url, mode: "lua" }).init(jwk)` |
| Remote HB (Production) | `yarn deploy --mainnet` | `new AO({ hb: "https://push-1.forward.computer" }).init(jwk)` |

- **Remote nodes**: Use `push-1` through `push-10` for full compute. `push.forward.computer` is push-only (no compute).
- **Lua mode**: Faster but no `receive()` — use `msg.reply()` pattern instead.
- **Wallet**: Run `yarn keygen` to generate `.wallet.json`.
- **HyperBEAM fork**: `git clone -b wao-final https://github.com/weavedb/HyperBEAM.git`

### Frontend Commands

```bash
cd frontend && npm run dev         # Vite dev server (port 5173)
cd frontend && npm run test:unit   # vitest component tests
cd frontend && npm run test:e2e    # Playwright E2E tests
cd frontend && npm run build       # production build
```

## Key Imports

### Testing (`wao/test`)
```js
import { AO, acc } from "wao/test"           // in-memory AOS
import { HB, HyperBEAM, acc } from "wao/test" // HyperBEAM
```

### Production (`wao`)
```js
import { AO, AR, GQL, HB } from "wao"
```

### Browser (`wao/web`)
```js
import { AO, AR } from "wao/web"  // browser only — NOT wao/test
```

Use `wao/web` for frontend code. Use `wao/test` for Node.js tests only.

## Wallet

Admin wallet auto-generated at `.wallet.json` (gitignored). This is an Arweave JWK used for deploy and HyperBEAM signing.

## HyperBEAM

- Default port: **10001**
- Local node path: configured via `CWD` in `.env.hyperbeam`
- `HyperBEAM` class manages the Erlang server lifecycle
- `HB` class is the HTTP client that talks to it

## Reference Docs

Read these on-demand when building features — they contain complete API references. Don't load all at once; read the one relevant to your current task:

| When you're... | Read this |
|----------------|-----------|
| Writing JS code (tests, deploy) | `docs/wao-sdk.md` — AO, HB, AR, GQL, Process handle APIs |
| Writing AOS scripts | `docs/aos-lua.md` — msg object, ao globals, patterns, blueprints |
| Building custom Lua modules | `docs/docs/pages/tutorials/custom-lua.mdx` — lua@5.3a device, compute function |
| Building custom WASM64 modules | `docs/docs/pages/tutorials/rust-wasm64.mdx` — wasm-64@1.0 device, Rust no_std |
| Working with HyperBEAM | `docs/hyperbeam-devices.md` — device catalog, endpoints, config |
| Building Erlang devices | `docs/hyperbeam-dev.md` — device protocol, templates, state |
| Debugging issues | `docs/debug.md` — known issues, error table, fixes |
| Building frontend | `docs/wao-sdk.md` (browser section) — wao/web, ArConnect |

## Agent Teams

For complex tasks that benefit from parallel work, create an agent team. Teammates run as separate Claude Code sessions with their own context windows, coordinating through a shared task list.

**When to use teams** (instead of working solo or using subagents):
- Building multiple independent features in parallel (each teammate owns separate files)
- Research + build: one teammate researches patterns in docs/, another implements
- Debug with competing hypotheses: teammates test different theories simultaneously
- Cross-layer work: one teammate on AOS scripts, one on JS tests, one on Erlang devices

**When NOT to use teams:**
- Sequential tasks or same-file edits (coordination overhead > benefit)
- Simple features that one session can handle
- Quick fixes or single-script changes (use subagents instead)

**Team patterns for WAO:**
```
# Parallel feature development
Create a team: one teammate builds the AOS script in src/,
another writes tests in test/, a third handles deployment config.

# Research and build
Create a team: one teammate researches the AOS patterns in docs/aos-lua.md,
another implements the AOS script based on findings.

# Debug investigation
Create a team with 3 teammates to investigate the timeout from different angles:
port conflicts, WASM memory, and HyperBEAM configuration.
```

Use `/team` to set up common team configurations.

## Skills

Slash commands in `.claude/skills/`:

### Build workflow
- `/build` — full build workflow (plan -> build -> test -> validate -> README). Orchestrates all steps, manages tasks.json throughout. Supports resume — picks up from `current_step` if interrupted.
- `/plan` — plan a feature (writes plan.md + tasks.json for persistent workflow)
- `/validate` — post-build validation (tests, Lua pitfalls, coverage)
- `/readme` — generate comprehensive README.md from plan, code, and tests

### Build steps (called by `/build` or standalone)
- `/build-aos` — build AOS scripts + in-memory tests, iterate until 100% pass
- `/build-module` — build custom WASM64 (Rust) or standalone Lua modules + HyperBEAM integration tests
- `/build-device` — build Erlang device + eunit tests, iterate until 100% pass
- `/build-frontend` — build Vite components + vitest tests, iterate until 100% pass

### Test steps (called by `/build` or standalone)
- `/test` — run in-memory AOS tests
- `/test-hb` — run HyperBEAM integration tests
- `/test-device` — WAO SDK integration tests for Erlang devices
- `/test-e2e` — Playwright E2E with live HyperBEAM backend

### Other
- `/report` — show progress on the current plan (task status, test results)
- `/deploy` — deploy Lua source to testnet, local HB, or remote HB (with pre-deploy validation)
- `/create-aos` — scaffold new AOS script + test
- `/create-module` — scaffold new custom module (WASM64 or Lua) + test
- `/create-device` — scaffold new HyperBEAM device + test
- `/debug` — troubleshoot issues
- `/team` — set up an agent team for parallel development
- `/dev` — start Vite dev server for frontend

## Subagents

Specialized agents in `.claude/agents/`:
- `builder` — general-purpose feature builder (Lua + tests + iterate)
- `tester` — test specialist (failures, debugging, verification)
- `device-builder` — Erlang device specialist (protocol, compilation, eunit)

## Rules

Path-specific rules in `.claude/rules/` auto-inject when editing matching files:
- `lua.md` — AOS script patterns (triggers on `src/**/*.lua`)
- `testing.md` — test patterns (triggers on `test/**/*.js`)
- `hyperbeam.md` — HyperBEAM device patterns (triggers on `HyperBEAM/**/*.erl`)
- `deploy.md` — deployment conventions (triggers on `scripts/**/*.js`)
- `frontend.md` — Frontend patterns (triggers on `frontend/**/*.{jsx,tsx,js}`)

## Personal Preferences

Create `CLAUDE.local.md` for personal project-specific preferences (auto-gitignored):
- Your sandbox URLs
- Your preferred test files
- Local HyperBEAM overrides

## Auto Memory

Claude automatically saves useful learnings as you work (build patterns, debug insights, architecture notes) to `~/.claude/projects/<project>/memory/MEMORY.md` (per-machine, not committed). Use `/memory` to view and edit saved memories.
