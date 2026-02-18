import { defineConfig } from "@playwright/test"
export default defineConfig({
  testDir: "./test",
  testMatch: "explorer.test.js",
  timeout: 60_000,
  use: { baseURL: "http://localhost:8788", headless: true },
})
