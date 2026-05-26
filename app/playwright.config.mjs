// @ts-check
import { defineConfig, devices } from "@playwright/test"
import { resolve, dirname } from "node:path"
import { fileURLToPath } from "node:url"

const __dirname = dirname(fileURLToPath(import.meta.url))
const APP_ROOT = __dirname

export default defineConfig({
  testDir: resolve(APP_ROOT, "test/e2e"),
  testIgnore: ["**/global-setup.mjs", "**/global-teardown.mjs", "**/run-from-root.mjs"],
  fullyParallel: false,
  forbidOnly: !!process.env.CI,
  retries: process.env.CI ? 2 : 0,
  workers: 1,
  reporter: process.env.CI ? "github" : "list",
  globalSetup: resolve(APP_ROOT, "test/e2e/global-setup.mjs"),
  globalTeardown: resolve(APP_ROOT, "test/e2e/global-teardown.mjs"),
  timeout: 90000,
  use: {
    baseURL: "http://localhost:3000",
    trace: "on-first-retry",
    actionTimeout: 15000,
    navigationTimeout: 30000,
  },
  projects: [
    {
      name: "chromium",
      use: { ...devices["Desktop Chrome"] },
    },
  ],
  webServer: {
    command: "npm run dev",
    url: "http://localhost:3000",
    reuseExistingServer: !process.env.CI,
    timeout: 120000,
    cwd: APP_ROOT,
    stdout: "ignore",
    stderr: "pipe",
  },
})
