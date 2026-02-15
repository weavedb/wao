---
name: build-frontend
description: "Write Vite + React frontend components with comprehensive vitest tests. 100% pass required. Use when user says 'build the frontend', 'write the UI', or 'implement the components'. Not for dev server — use /dev."
argument-hint: "<task-id>"
disable-model-invocation: true
allowed-tools: Read, Write, Edit, Bash, Grep, Glob
---

Build Vite + React frontend components and/or vitest tests for a task. Iterates until all tests pass 100%.

## Steps

0. Check prerequisites:
```bash
test -d frontend && echo "OK" || echo "ERROR: No frontend/ directory"
test -f frontend/package.json && echo "OK" || echo "ERROR: No package.json"
```
If missing, stop and tell the user: "No frontend scaffolded. Re-run `npx wao create` with frontend option."

Check Playwright if task is frontend-integration:
```bash
cd frontend && npx playwright --version 2>/dev/null || echo "Playwright not installed — run: cd frontend && npx playwright install"
```

1. Read `tasks.json` and find the task matching `$ARGUMENTS` (task id). Update its status to `"in_progress"`.

2. Read `plan.md` for UI specifications:
   - Components to build
   - User interactions
   - AO process connections
   - State management

3. Read `docs/wao-sdk.md` (browser section) for `wao/web` patterns.

4. **If the task type is `frontend`** — write React components:

   Write components in `frontend/src/`:

   ```jsx
   import { AO, AR } from "wao/web"  // browser only — NOT wao/test

   // ArConnect wallet integration
   async function connectWallet() {
     await window.arweaveWallet.connect(["ACCESS_ADDRESS", "SIGN_TRANSACTION"])
     const addr = await window.arweaveWallet.getActiveAddress()
     return addr
   }

   // AO process interaction
   async function sendMessage(processId, action, tags) {
     const ao = new AO()
     const { p } = ao.p(processId)
     const result = await p.msg(action, tags)
     return result
   }
   ```

   Key patterns:
   - Import from `wao/web` (NOT `wao/test` — that's Node.js only)
   - ArConnect wallet via `window.arweaveWallet`
   - Process interaction via `ao.p(processId).msg()`
   - Use React hooks for state management
   - Handle loading, error, and success states

5. **If the task type is `frontend-test`** — write vitest tests:

   Write tests in `frontend/test/`:

   ```jsx
   import { describe, it, expect, vi } from "vitest"
   import { render, screen, fireEvent, waitFor } from "@testing-library/react"
   import { MyComponent } from "../src/MyComponent"

   // Mock wao/web
   vi.mock("wao/web", () => ({
     AO: vi.fn(() => ({
       p: vi.fn(() => ({
         msg: vi.fn().mockResolvedValue({ res: { Output: { data: "ok" } } })
       }))
     })),
   }))

   // Mock ArConnect
   Object.defineProperty(window, "arweaveWallet", {
     value: {
       connect: vi.fn(),
       getActiveAddress: vi.fn().mockResolvedValue("test-addr"),
     },
   })

   describe("MyComponent", () => {
     it("renders correctly", () => {
       render(<MyComponent />)
       expect(screen.getByText("expected")).toBeTruthy()
     })

     it("handles wallet connection", async () => {
       render(<MyComponent />)
       fireEvent.click(screen.getByRole("button", { name: /connect/i }))
       await waitFor(() => {
         expect(window.arweaveWallet.connect).toHaveBeenCalled()
       })
     })

     // Test: render, interaction, state updates, error states, loading states
   })
   ```

6. Run tests:

   ```bash
   cd frontend && npm run test:unit
   ```

7. **If tests fail, keep debugging.** Do not stop. On each iteration:
   1. Read the full error output — stack traces, assertion messages, log lines
   2. Identify the FIRST failing test (fix failures in order)
   3. Classify: is the bug in component code or test code?
   4. Fix ONE issue per iteration (don't change multiple things at once)
   5. Re-run the failing test file specifically (faster feedback than full suite)
   6. If the same error persists after 3 different fix attempts, step back:
      - Re-read the relevant docs (`wao/web` patterns, React testing library, vitest mocking)
      - Check if the plan itself has a wrong assumption
      - Try a completely different approach
   7. Continue iterating until all tests pass — there is no retry limit

   Only escalate to the user if:
   - The failure is environmental (missing dependency, port conflict, permissions)
   - The test requires user input (wallet, external service URL)
   - You've tried 10+ iterations with no progress on the same error

8. Update the task status to `"done"` in `tasks.json`.

    **IMPORTANT**: Only mark done if all tests pass 100%.
