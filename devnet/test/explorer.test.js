import { test, expect } from "@playwright/test"
import { startWrangler } from "./helpers.js"
import { resolve } from "node:path"

let server

test.beforeAll(async () => {
  server = await startWrangler({ port: 8788, cwd: resolve(import.meta.dirname, "..") })

  // Seed data so tests have content
  const { execSync } = await import("node:child_process")
  execSync("node devnet/test/seed.js", {
    cwd: resolve(import.meta.dirname, "../.."),
    env: { ...process.env, PORT: "8788" },
    stdio: "pipe",
  })
})

test.afterAll(async () => {
  if (server) await server.kill()
})

test("Dashboard renders 5 stat cards and section titles", async ({ page }) => {
  await page.goto("/")
  await page.waitForSelector(".stats")
  const cards = await page.locator(".stat-card").count()
  expect(cards).toBe(5)

  await expect(page.locator(".section-title", { hasText: "Latest Blocks" })).toBeVisible()
  await expect(page.locator(".section-title", { hasText: "Latest Transactions" })).toBeVisible()
  await expect(page.locator(".section-title", { hasText: "Latest Messages" })).toBeVisible()
})

test("Dashboard shows type badges in tables", async ({ page }) => {
  await page.goto("/")
  await page.waitForSelector(".stats")
  // Transactions table should have type badges
  const badges = await page.locator("[class^='type-badge-']").count()
  expect(badges).toBeGreaterThan(0)
})

test("Nav has Arweave and AO section labels", async ({ page }) => {
  await page.goto("/")
  await expect(page.locator("nav .nav-section", { hasText: "Arweave" })).toBeVisible()
  await expect(page.locator("nav .nav-section", { hasText: "AO" })).toBeVisible()

  const navLinks = await page.locator("nav a").count()
  expect(navLinks).toBe(6)
})

test("Theme toggle works", async ({ page }) => {
  await page.goto("/")
  const toggle = page.locator(".theme-toggle")
  await expect(toggle).toBeVisible()

  // Click to switch to light
  await toggle.click()
  const theme = await page.evaluate(() => document.documentElement.getAttribute("data-theme"))
  expect(theme).toBe("light")

  // Click back to dark
  await toggle.click()
  const theme2 = await page.evaluate(() => document.documentElement.getAttribute("data-theme"))
  expect(theme2).toBeNull()
})

test("Theme persists in localStorage", async ({ page }) => {
  await page.goto("/")
  await page.locator(".theme-toggle").click()
  const stored = await page.evaluate(() => localStorage.getItem("theme"))
  expect(stored).toBe("light")

  // Toggle back
  await page.locator(".theme-toggle").click()
  const stored2 = await page.evaluate(() => localStorage.getItem("theme"))
  expect(stored2).toBe("dark")
})

test("Blocks page renders", async ({ page }) => {
  await page.goto("/#/blocks")
  await expect(page.locator(".section-title", { hasText: "Blocks" })).toBeVisible({ timeout: 15000 })
})

test("Transactions page renders with type badges", async ({ page }) => {
  await page.goto("/#/transactions")
  await expect(page.locator(".section-title", { hasText: "Transactions" })).toBeVisible({ timeout: 15000 })
  // Should have type badges in rows
  const badges = await page.locator("[class^='type-badge-']").count()
  expect(badges).toBeGreaterThan(0)
})

test("Processes page renders with seeded data", async ({ page }) => {
  await page.goto("/#/processes")
  await expect(page.locator(".section-title", { hasText: "Processes" })).toBeVisible({ timeout: 15000 })
  // Should have process type badges
  const badges = await page.locator(".type-badge-process").count()
  expect(badges).toBeGreaterThan(0)
})

test("Messages page renders with seeded data", async ({ page }) => {
  await page.goto("/#/messages")
  await expect(page.locator(".section-title", { hasText: "Messages" })).toBeVisible({ timeout: 15000 })
  // Should have message type badges (action badges)
  const badges = await page.locator(".type-badge-message").count()
  expect(badges).toBeGreaterThan(0)
})

test("Modules page renders with seeded data", async ({ page }) => {
  await page.goto("/#/modules")
  await expect(page.locator(".section-title", { hasText: "Modules" })).toBeVisible({ timeout: 15000 })
  const badges = await page.locator(".type-badge-module").count()
  expect(badges).toBeGreaterThan(0)
})

test("Clicking a process shows detail page", async ({ page }) => {
  await page.goto("/#/processes")
  await expect(page.locator(".section-title", { hasText: "Processes" })).toBeVisible({ timeout: 15000 })
  // Click the first process row link
  const firstLink = page.locator("tbody .link").first()
  await firstLink.click()
  // Wait for navigation to entity page
  await page.waitForURL(/#\/entity\//, { timeout: 15000 })
  // Should show detail fields
  await expect(page.locator(".detail-fields")).toBeVisible({ timeout: 15000 })
  // Should show tags section
  await expect(page.locator(".section-title", { hasText: "Tags" })).toBeVisible()
})

test("Clicking a message shows detail page", async ({ page }) => {
  await page.goto("/#/messages")
  await expect(page.locator(".section-title", { hasText: "Messages" })).toBeVisible({ timeout: 15000 })
  // Click the first message ID link
  const firstLink = page.locator("tbody .link").first()
  await firstLink.click()
  // Wait for navigation to entity page
  await page.waitForURL(/#\/entity\//, { timeout: 15000 })
  // Should show detail fields
  await expect(page.locator(".detail-fields")).toBeVisible({ timeout: 15000 })
  // Should show tags section
  await expect(page.locator(".section-title", { hasText: "Tags" })).toBeVisible()
})

test("Message with target auto-loads compute result", async ({ page }) => {
  await page.goto("/#/messages")
  await expect(page.locator(".section-title", { hasText: "Messages" })).toBeVisible({ timeout: 15000 })
  // Find a message row that has a non-dash "To" value (has a target)
  const rows = page.locator("tbody tr")
  const count = await rows.count()
  let found = false
  for (let i = 0; i < count; i++) {
    const toCells = rows.nth(i).locator("td").nth(3)
    const toText = await toCells.textContent()
    if (toText && toText.trim() !== "\u2014" && toText.trim() !== "") {
      // Click the ID link (2nd column) to avoid hitting From/To address links
      await rows.nth(i).locator("td").nth(1).locator(".link").click()
      found = true
      break
    }
  }
  if (found) {
    await page.waitForURL(/#\/entity\//, { timeout: 15000 })
    await expect(page.locator(".detail-fields")).toBeVisible({ timeout: 15000 })
    // Compute result auto-loads — check for section title
    await expect(page.locator(".section-title", { hasText: "Compute Result" })).toBeVisible({ timeout: 15000 })
  }
})

test("Search navigates to entity page", async ({ page }) => {
  await page.goto("/")
  const input = page.locator(".search-bar input")
  await input.fill("test-query-123")
  await page.locator(".search-bar button").click()
  await page.waitForURL(/#\/entity\/test-query-123/)
})

test("Nav click navigation works", async ({ page }) => {
  await page.goto("/")
  await page.locator("nav a", { hasText: "Blocks" }).click()
  await page.waitForURL(/#\/blocks/)
  await expect(page.locator(".section-title", { hasText: "Blocks" })).toBeVisible({ timeout: 15000 })
})

test("Entity page handles unknown ID", async ({ page }) => {
  await page.goto("/#/entity/nonexistent")
  await page.waitForTimeout(2000)
  const content = await page.locator("#content").textContent()
  expect(content.length).toBeGreaterThan(0)
})

test("All table IDs are clickable links", async ({ page }) => {
  await page.goto("/#/transactions")
  await expect(page.locator(".section-title", { hasText: "Transactions" })).toBeVisible({ timeout: 15000 })
  // Check that table cells contain .link spans
  const links = await page.locator("tbody .link").count()
  expect(links).toBeGreaterThan(0)
})

test("Logo shows WAO SCAN", async ({ page }) => {
  await page.goto("/")
  await expect(page.locator(".logo")).toContainText("WAO SCAN")
  await expect(page.locator(".logo > span:first-of-type")).toContainText("devnet alpha")
})
