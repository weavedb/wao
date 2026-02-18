import { defineConfig } from "vitest/config"
export default defineConfig({
  test: {
    testTimeout: 120_000,
    hookTimeout: 120_000,
    include: ["test/**/*.test.js"],
    exclude: ["test/explorer.test.js"],
    fileParallelism: false,
  },
})
