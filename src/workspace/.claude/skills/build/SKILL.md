---
name: build
description: "Full build workflow — plan, build, test, validate. Manages tasks.json throughout every step. Use when user says 'build this', 'start building', 'implement this feature', or 'make this'. Not for single-file fixes — use /build-aos, /build-device, or /build-module directly."
argument-hint: "<feature-description>"
disable-model-invocation: true
allowed-tools: Read, Write, Edit, Bash, Grep, Glob
---

Full build orchestrator. Sequences through plan → build → test → validate → README, managing `tasks.json` throughout.

## Performance Notes
- Take your time on each task — quality is more important than speed
- Read the relevant docs thoroughly before writing code
- Do not skip validation steps even if they seem redundant
- When iterating on test failures, identify the root cause before changing code

## Examples

### Start a new build
```
/build A counter app that tracks clicks per user, with a leaderboard showing top 10
```

### Resume an interrupted build
```
/build
```
(Automatically detects tasks.json and resumes)

## Steps

### 1. Check for existing plan (resume support)

Read `plan.md` and `tasks.json` at the project root. If both exist and tasks.json has `"current_step"`, resume:

1. Find the first task with `"status": "in_progress"` — resume debugging that task immediately.
2. If no `in_progress` task, find the first task with `"status": "pending"` — start it.
3. If ALL tasks are `"done"`, **re-verify every test task** by running its tests:
   - `aos-test` → `yarn test <files>`
   - `aos-integration` → `yarn test <files>`
   - `device` → `cd $HB_DIR && rebar3 eunit --module=<module>`
   - `device-integration` → `yarn test <files>`
   - `frontend-test` → `cd frontend && npm run test:unit`
   - `frontend-integration` → `cd frontend && npm run test:e2e`
   - `validate` → run all validation gates

   If any test task fails, set that task back to `"in_progress"` in tasks.json and resume debugging it. Do NOT skip over failing tests just because the task was previously marked done.

If no plan exists, proceed to step 2.

### 2. Plan

**Enter plan mode using the `EnterPlanMode` tool immediately.** Do not skip this step.

If `$ARGUMENTS` is provided, use it as the feature description. If `$ARGUMENTS` is empty, ask the user what they want to build — but only this one question, then proceed immediately.

While in plan mode, read `.claude/skills/plan/SKILL.md` and execute ALL of its steps:
1. Ask the user which tracks to build (AOS / Custom Modules / Device / Frontend)
2. Read the relevant docs for the selected tracks
3. Design the feature
4. Write `plan.md` and `tasks.json` at the project root

Once `plan.md` and `tasks.json` are written, **use `ExitPlanMode` to present the plan for user approval.** This exits plan mode and asks the user to approve before building begins.

After the user approves, update `plan.md` status to `APPROVED` and proceed to step 3.

### 3. Clean up sample files

Remove the starter reference files that ship with the template. These exist so users can explore the project before building, but should be cleared before a real build:

```bash
rm -f src/counter.lua test/aos.test.js test/hyperbeam.test.js
```

Also remove any other sample files (`src/registry.lua`, `src/token.lua`, `test/registry.test.js`, `test/token.test.js`) if they exist:

```bash
rm -f src/registry.lua src/token.lua test/registry.test.js test/token.test.js
```

### 4. Verify prerequisites

Before starting the build loop, check which tracks are selected and verify their prerequisites.

**Device track or integration tests** — require HyperBEAM configured via `.env.hyperbeam`:

```bash
grep '^CWD=' .env.hyperbeam 2>/dev/null | cut -d= -f2-
```

If the plan includes `device`, `device-integration`, `aos-integration`, `module-lua`, `module-wasm`, or `module-test` tasks, verify:

1. `.env.hyperbeam` exists and has a `CWD` value
2. The `CWD` path points to a valid HyperBEAM directory (`$CWD/src` exists)
3. `rebar3` is available (for device compilation)

If any check fails, **stop and tell the user**:

> Device/integration tasks require HyperBEAM. Configure `.env.hyperbeam`:
> ```bash
> echo "CWD=/path/to/your/HyperBEAM" >> .env.hyperbeam
> ```
>
> Or re-run `npx wao create` and choose option 1 (clone) or 2 (link).

Only proceed once all prerequisites are met.

### 5. Build loop

For each task in `tasks.json` (in `id` order):

a. Update the task status to `"in_progress"` in `tasks.json` and update `current_step`.

b. Read the skill file for this task's type and follow its instructions. The mapping from task type to skill file:

| Task type | Read and follow |
|-----------|-----------------|
| `aos` | `.claude/skills/build-aos/SKILL.md` |
| `aos-test` | `.claude/skills/build-aos/SKILL.md` |
| `aos-integration` | `.claude/skills/test-hb/SKILL.md` |
| `module-lua` | `.claude/skills/build-module/SKILL.md` |
| `module-wasm` | `.claude/skills/build-module/SKILL.md` |
| `module-test` | `.claude/skills/build-module/SKILL.md` |
| `device` | `.claude/skills/build-device/SKILL.md` (eunit tests are inline — same file) |
| `device-integration` | `.claude/skills/test-device/SKILL.md` |
| `frontend` | `.claude/skills/build-frontend/SKILL.md` |
| `frontend-test` | `.claude/skills/build-frontend/SKILL.md` |
| `frontend-integration` | `.claude/skills/test-e2e/SKILL.md` |
| `readme` | `.claude/skills/readme/SKILL.md` |
| `validate` | `.claude/skills/validate/SKILL.md` |

c. Follow the skill's instructions for this task, using the task `id` as context.

d. After completing, verify the task's `done_when` criteria are met:
   - For test tasks: tests must pass 100%
   - For build tasks: files must exist and compile/load without errors

e. If the criteria are met, update the task status to `"done"` in `tasks.json`.

f. **If tests fail, keep debugging.** Do not stop. On each iteration:
   1. Read the full error output — stack traces, assertion messages, log lines
   2. Identify the root cause (wrong logic, missing handler, bad import, timing issue, etc.)
   3. Fix the code — not the test, unless the test itself has a bug
   4. Re-run the failing test file specifically (faster feedback than full suite)
   5. If the same error persists after 3 different fix attempts, step back:
      - Re-read the relevant docs (AOS patterns, HyperBEAM device protocol, etc.)
      - Check if the plan itself has a wrong assumption
      - Try a completely different approach
   6. Continue iterating until all tests pass — there is no retry limit

g. Only escalate to the user if:
   - The failure is environmental (port conflict, missing binary, permissions)
   - The test requires user input (wallet, external service URL)
   - You've tried 10+ iterations with no progress on the same error

h. Move to the next task.

### 6. README

After all build and test tasks are done, read `.claude/skills/readme/SKILL.md` and follow its instructions to generate README.md.

### 7. Final validation

Read `.claude/skills/validate/SKILL.md` and follow its instructions to run all validation gates.

### 8. Summary

Report final results:

```
## Build Complete

Feature: {feature name}
Tracks: {AOS, Custom Modules, Device, Frontend}

| Task | Status |
|------|--------|
| ... | done/skipped/failed |

Overall: PASS / FAIL
```

## Failure handling

- **Test failures are expected.** The build loop is: write code → run tests → read failures → fix → re-run. This is the core loop — never skip it, never give up early.
- If a skill exits with test failures, read the full output, diagnose the root cause, fix the code (not the test), and re-run. Keep iterating until 100% pass.
- If a skill cannot be dispatched (unknown type), skip and warn.
- If the user interrupts, save current state to tasks.json so the next session can resume.

## Rollback

If a task corrupts working code:
1. Check git status for what changed
2. Restore the last working version: `git checkout -- <file>` for specific files
3. Re-run tests to confirm the rollback worked
4. Update the task status back to `pending` in tasks.json
5. Retry with a different approach

Never force a task to `done` status with failing tests. The TaskCompleted hook will block it anyway.

## Troubleshooting

### Build stalls on a single task
- Check if it's a test failure loop — read the last test output
- If the same error persists after 3 different fix attempts, step back and reconsider — the plan may have a wrong assumption
- Run `/debug` to diagnose port/process/WASM issues

### Resume doesn't pick up the right task
- Read `tasks.json` directly and check `current_step` value
- Verify task statuses — a task stuck as `in_progress` blocks progression

### Permission denied on yarn test
- Check `node --version` is 22+ (24+ recommended)
- On Node 22, set `NODE_OPTIONS=--experimental-wasm-memory64`; on Node 24+ the flag is default-on and is rejected if passed explicitly
