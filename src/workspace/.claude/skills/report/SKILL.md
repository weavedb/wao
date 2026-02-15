---
name: report
description: "Report progress on the current plan. Reads plan.md and tasks.json, shows status of each task and overall progress. Use when user says 'show progress', 'where are we', 'status', or 'what's done'. Not for running tests — use /validate."
disable-model-invocation: true
allowed-tools: Read, Bash, Grep, Glob
---

Report the current progress of the build plan.

## Steps

1. Check if `plan.md` and `tasks.json` exist. If not, report "No active plan. Run /plan first."

2. Read `plan.md` for the feature name and components.

3. Read `tasks.json` and count tasks by status:
   - **done**: completed tasks
   - **in_progress**: currently being worked on
   - **pending**: not started yet

4. For each task, show a one-line summary:

```
## Progress: {feature name}

  ✔ Task 1: Build AOS Lua handlers
  ✔ Task 2: Write in-memory AOS tests
  ▶ Task 3: Write AOS HyperBEAM integration tests
  · Task 4: Build frontend components
  · Task 5: Write frontend vitest tests
  · Task 6: Write frontend Playwright integration tests
  · Task 7: Validate all gates

Progress: 2/7 done, 1 in progress

Components:
  AOS:      ██████░░░░ tests passing
  Device:   ░░░░░░░░░░ not started
  Frontend: ░░░░░░░░░░ not started
```

5. If any task is `in_progress`, run a quick check on its "done when" condition:
   - aos-test: run `yarn test` and report pass/fail count
   - device: run `rebar3 eunit --module=dev_{name}` and report (eunit tests are inline)
   - module-test: run `yarn test test/{name}-module.test.js` and report
   - frontend-test: run `cd frontend && npm run test:unit` and report
   - For other types, just report the status

6. Report any blockers or issues found.
