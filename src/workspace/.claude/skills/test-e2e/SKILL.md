---
name: test-e2e
description: "Playwright E2E tests with live HyperBEAM backend. Launches HB via WAO SDK, runs browser tests. Use when user says 'run E2E tests', 'test the frontend', or 'browser tests'. Not for vitest — use /build-frontend."
argument-hint: "<task-id>"
disable-model-invocation: true
allowed-tools: Read, Write, Edit, Bash, Grep, Glob
---

Write and run Playwright E2E tests with a live HyperBEAM backend. Tests full user flows in the browser.

## Steps

0. Check prerequisites:
```bash
test -d frontend/e2e && echo "OK" || echo "ERROR: No e2e/ directory"
cd frontend && npx playwright --version 2>/dev/null || echo "ERROR: Playwright not installed"
```
If Playwright is missing: `cd frontend && npx playwright install`

1. Read `tasks.json` and find the task matching `$ARGUMENTS` (task id). Update its status to `"in_progress"`.

2. Kill stale HyperBEAM processes:

   ```bash
   pkill -f beam.smp 2>/dev/null || true
   ```

3. **Write Playwright tests** in `frontend/e2e/`:

   ```js
   import { test, expect } from "@playwright/test"

   test.describe("{feature} E2E", () => {
     test("full user flow", async ({ page }) => {
       await page.goto("http://localhost:5173")

       // Wait for app to load
       await expect(page.locator("h1")).toBeVisible()

       // Test wallet connection flow
       // (mock ArConnect via page.addInitScript if needed)

       // Test process interaction
       // - Fill form inputs
       // - Click submit
       // - Wait for response from AO process
       // - Verify UI updates

       // Test error states
       // - Network failure
       // - Invalid input
     })
   })
   ```

4. **Write global setup** if needed for HyperBEAM lifecycle:

   ```js
   // frontend/e2e/global-setup.js
   import { HyperBEAM } from "wao/test"

   let hbeam

   export default async function globalSetup() {
     hbeam = await new HyperBEAM({ reset: true }).ready()
     process.env.HYPERBEAM_URL = hbeam.url
   }

   export async function globalTeardown() {
     if (hbeam) await hbeam.kill()
   }
   ```

   Reference in `frontend/playwright.config.js`:
   ```js
   export default {
     globalSetup: "./e2e/global-setup.js",
     globalTeardown: "./e2e/global-setup.js",
     use: { baseURL: "http://localhost:5173" },
   }
   ```

5. Ensure the Vite dev server is running (or start it):

   ```bash
   cd frontend && npm run dev &
   ```

6. Run E2E tests:

   ```bash
   cd frontend && npx playwright test
   ```

7. If any tests fail:
   - Read the error output and screenshots (if any)
   - Fix the test or frontend code
   - Re-run
   - Repeat until **100% pass**

8. Kill the Vite dev server and HyperBEAM after tests.

9. Update the task status to `"done"` in `tasks.json`.
