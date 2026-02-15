---
name: plan
description: "Plan a feature before building. Writes plan.md and tasks.json so any agent can pick up the work. Use when starting a new feature, user says 'plan this', 'design this', or 'what should we build'. Not for modifying existing plans — edit plan.md directly."
argument-hint: "<feature-description>"
disable-model-invocation: true
allowed-tools: Read, Write, Grep, Glob
---

Plan a feature before building it. The plan and tasks are written to files so any Claude session can pick up and execute them.

## Performance Notes
- Take your time researching patterns and edge cases
- Quality of the plan determines quality of the build — a thorough plan saves iteration cycles
- Consider all edge cases before writing tasks.json — it's cheaper to plan than to rebuild

## Steps

1. Read the user's feature description from `$ARGUMENTS`.

2. **Prompt the user** to choose which components to build. Ask them to select one or more:
   - **AOS Lua scripts** — Lua scripts running on AOS processes (src/*.lua)
   - **Custom Modules** — WASM64 (Rust) or standalone Lua modules (custom-wasm/ or custom-lua/)
   - **HyperBEAM devices** — custom Erlang device modules (HyperBEAM/src/*.erl)
   - **Frontend** — Vite + React browser app with wao/web (frontend/)

   Wait for the user's answer before proceeding. Only generate tasks for the selected components.

3. Read the relevant docs for the selected components:
   - AOS scripts → `docs/aos-lua.md`
   - Custom Modules (Lua) → `docs/docs/pages/tutorials/custom-lua.mdx`
   - Custom Modules (WASM64) → `docs/docs/pages/tutorials/rust-wasm64.mdx`
   - HyperBEAM devices → `docs/hyperbeam-dev.md` and `docs/hyperbeam-devices.md`
   - JS/SDK → `docs/wao-sdk.md`
   - Frontend → `docs/wao-sdk.md` (browser section)

4. Design the feature: files, AOS scripts/devices, edge cases, test scenarios.

5. Write `plan.md` at the project root:

```markdown
# Plan: {feature name}

## Overview
{1-2 sentence summary}

## Components
- [ ] AOS: {yes/no — list AOS scripts}
- [ ] Custom Modules: {yes/no — list modules, specify Lua or WASM64}
- [ ] Device: {yes/no — list devices}
- [ ] Frontend: {yes/no — list pages/components}

## AOS Scripts
| Script | Action | Input Tags | State | Reply |
|---------|--------|------------|-------|-------|

## Devices
| Device | Module | Exports | Description |
|--------|--------|---------|-------------|

## Edge Cases
1. ...

## Status: PENDING APPROVAL
```

6. Write `tasks.json` at the project root. This is a JSON file so any agent can precisely parse task status. Tasks follow a strict build order based on what components are included. Pick the applicable task templates:

```json
{
  "feature": "{feature name}",
  "tracks": ["aos"],
  "current_step": 0,
  "tasks": [
    {
      "id": 1,
      "name": "Build AOS scripts",
      "type": "aos",
      "skill": "/build-aos",
      "status": "pending",
      "files": ["src/{name}.lua"],
      "details": "{script names, actions, input tags, state, reply format, input validation}",
      "done_when": "AOS scripts written with all edge cases handled"
    },
    {
      "id": 2,
      "name": "Write in-memory AOS tests",
      "type": "aos-test",
      "skill": "/build-aos",
      "status": "pending",
      "files": ["test/{name}.test.js"],
      "details": "{test scenarios — happy path, error paths, multi-user, boundaries}",
      "done_when": "yarn test test/{name}.test.js — all pass"
    },
    {
      "id": 3,
      "name": "Write AOS HyperBEAM integration tests",
      "type": "aos-integration",
      "skill": "/test-hb",
      "status": "pending",
      "files": ["test/hyperbeam.test.js"],
      "details": "{deploy AOS scripts on real HyperBEAM via genesis-wasm, test through HTTP}",
      "done_when": "yarn test test/hyperbeam.test.js — all pass"
    }
  ]
}
```

**Root-level fields**:
- `"tracks"` — array of selected build tracks: `"aos"`, `"module"`, `"device"`, `"frontend"` (only the ones the user chose)
- `"current_step"` — set to `0` initially; `/build` updates this as it progresses through tasks

**Every task must include `"skill"`** — the slash command that handles it. This is how `/build` dispatches work:

| Task type | Skill |
|-----------|-------|
| `aos` | `/build-aos` |
| `aos-test` | `/build-aos` |
| `aos-integration` | `/test-hb` |
| `module-lua` | `/build-module` |
| `module-wasm` | `/build-module` |
| `module-test` | `/build-module` |
| `device` | `/build-device` (eunit tests inline — same file) |
| `device-integration` | `/test-device` |
| `frontend` | `/build-frontend` |
| `frontend-test` | `/build-frontend` |
| `frontend-integration` | `/test-e2e` |
| `readme` | `/readme` |
| `validate` | `/validate` |

**Custom Module tasks** (if building custom WASM64 or Lua modules) — add `"module"` to `tracks` array and add these to `tasks`:
- For Lua: `{ "type": "module-lua", "skill": "/build-module", "files": ["custom-lua/{name}.lua"], "done_when": "module written with compute function" }`
- For WASM64: `{ "type": "module-wasm", "skill": "/build-module", "files": ["custom-wasm/src/lib.rs"], "done_when": "cargo build succeeds, .wasm file produced" }`
- Tests: `{ "type": "module-test", "skill": "/build-module", "files": ["test/{name}-module.test.js"], "done_when": "yarn test test/{name}-module.test.js — all pass" }`

**Device tasks** (if building Erlang devices) — add `"device"` to `tracks` array and add these to `tasks`:
- `{ "type": "device", "skill": "/build-device", "files": ["HyperBEAM/src/dev_{name}.erl"], "done_when": "rebar3 compile + eunit — all pass" }`
  - **HyperBEAM convention**: device source and eunit tests live in the same `.erl` file using `-ifdef(TEST).` guards. Do NOT create separate test files in `HyperBEAM/test/`.
- `{ "type": "device-integration", "skill": "/test-device", "files": ["test/{name}-device.test.js"], "done_when": "yarn test test/{name}-device.test.js — all pass" }`

**Frontend tasks** (if building frontend) — add `"frontend"` to `tracks` array and add these to `tasks`:
- `{ "type": "frontend", "skill": "/build-frontend", "files": ["frontend/src/{components}"], "done_when": "components render, connect to AO processes" }`
- `{ "type": "frontend-test", "skill": "/build-frontend", "files": ["frontend/test/{name}.test.jsx"], "done_when": "cd frontend && npm run test:unit — all pass" }`
- `{ "type": "frontend-integration", "skill": "/test-e2e", "files": ["frontend/e2e/{name}.spec.js"], "done_when": "cd frontend && npm run test:e2e — all pass" }`

**README task** — add after all build/test tasks, before validate:
- `{ "type": "readme", "skill": "/readme", "done_when": "README.md written with architecture, setup, and usage" }`

**Final validation** — always the last task:
- `{ "type": "validate", "skill": "/validate", "done_when": "overall PASS" }`

### IMPORTANT: Replace all placeholders with actual names

The templates above use `{name}` as a placeholder. When writing `tasks.json`, replace ALL placeholders with the actual file names from the plan. For example:
- `"files": ["src/{name}.lua"]` → `"files": ["src/token.lua", "src/registry.lua"]`
- `"files": ["test/{name}.test.js"]` → `"files": ["test/token.test.js", "test/registry.test.js"]`
- `"done_when": "yarn test test/{name}.test.js — all pass"` → `"done_when": "yarn test test/token.test.js test/registry.test.js — all pass"`

The `files` array must list every actual file the task will create. The `done_when` must reference the actual test commands. Never leave `{name}` as a literal string in the output.

### Task ordering rules

The build order matters. Within each component:
- **AOS**: AOS scripts → unit tests → integration tests
- **Custom Modules**: module source → HyperBEAM integration tests
- **Device**: Erlang module → eunit → integration tests
- **Frontend**: components → vitest → Playwright

Cross-component: AOS, Custom Modules, and Device can be built in parallel. Frontend depends on AOS/Device/Module being done (it calls them). Final validation is always last.

### Task numbering

Number task `id` sequentially starting from 1. Only include tasks for selected components. A pure AOS project might have 3 tasks + validate. A full project with all four components might have 12+ tasks.

7. Each task must be self-contained — a new agent reading only `plan.md` and `tasks.json` should be able to execute any task without additional context.

8. Show the plan to the user and wait for approval. After approval, update `plan.md` status to `APPROVED`.
