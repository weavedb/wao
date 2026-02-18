import { createSearch } from "./search.js"

function initTheme() {
  const saved = localStorage.getItem("theme")
  if (saved) {
    document.documentElement.setAttribute("data-theme", saved)
  }
  // no saved = dark (default via CSS :root)
}

function toggleTheme() {
  const current = document.documentElement.getAttribute("data-theme")
  const next = current === "light" ? "dark" : "light"
  if (next === "dark") {
    document.documentElement.removeAttribute("data-theme")
  } else {
    document.documentElement.setAttribute("data-theme", next)
  }
  localStorage.setItem("theme", next)
  return next
}

function getCurrentTheme() {
  return document.documentElement.getAttribute("data-theme") === "light" ? "light" : "dark"
}

export function createLayout(app) {
  initTheme()

  const header = document.createElement("header")
  const logo = document.createElement("a")
  logo.className = "logo"
  logo.href = "#/"
  logo.innerHTML = '<div class="logo-text">WAO SCAN</div><span>DEVNET ALPHA</span>'
  header.appendChild(logo)

  const headerRight = document.createElement("div")
  headerRight.className = "header-right"
  headerRight.appendChild(createSearch())

  const themeBtn = document.createElement("button")
  themeBtn.className = "theme-toggle"
  themeBtn.title = "Toggle dark/light mode"
  const updateIcon = () => {
    const theme = getCurrentTheme()
    themeBtn.innerHTML = theme === "light"
      ? '<svg width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><circle cx="12" cy="12" r="5"/><path d="M12 1v2M12 21v2M4.22 4.22l1.42 1.42M18.36 18.36l1.42 1.42M1 12h2M21 12h2M4.22 19.78l1.42-1.42M18.36 5.64l1.42-1.42"/></svg>'
      : '<svg width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><path d="M21 12.79A9 9 0 1111.21 3 7 7 0 0021 12.79z"/></svg>'
  }
  updateIcon()
  themeBtn.addEventListener("click", () => {
    toggleTheme()
    updateIcon()
  })
  headerRight.appendChild(themeBtn)

  header.appendChild(headerRight)

  const nav = document.createElement("nav")
  const items = [
    { href: "#/", label: "Dashboard", icon: '<svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><rect x="3" y="3" width="7" height="7"/><rect x="14" y="3" width="7" height="7"/><rect x="3" y="14" width="7" height="7"/><rect x="14" y="14" width="7" height="7"/></svg>' },
    { href: "#/blocks", label: "Blocks", icon: '<svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><rect x="3" y="3" width="18" height="18" rx="2"/><path d="M3 9h18M9 21V9"/></svg>' },
    { href: "#/transactions", label: "Transactions", icon: '<svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M17 1l4 4-4 4"/><path d="M3 11V9a4 4 0 014-4h14"/><path d="M7 23l-4-4 4-4"/><path d="M21 13v2a4 4 0 01-4 4H3"/></svg>' },
    { href: "#/modules", label: "Modules", icon: '<svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M21 16V8a2 2 0 00-1-1.73l-7-4a2 2 0 00-2 0l-7 4A2 2 0 003 8v8a2 2 0 001 1.73l7 4a2 2 0 002 0l7-4A2 2 0 0021 16z"/><path d="M3.27 6.96L12 12.01l8.73-5.05M12 22.08V12"/></svg>' },
    { href: "#/processes", label: "Processes", icon: '<svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><circle cx="12" cy="12" r="3"/><path d="M19.4 15a1.65 1.65 0 00.33 1.82l.06.06a2 2 0 010 2.83 2 2 0 01-2.83 0l-.06-.06a1.65 1.65 0 00-1.82-.33 1.65 1.65 0 00-1 1.51V21a2 2 0 01-4 0v-.09A1.65 1.65 0 009 19.4a1.65 1.65 0 00-1.82.33l-.06.06a2 2 0 01-2.83-2.83l.06-.06A1.65 1.65 0 004.68 15a1.65 1.65 0 00-1.51-1H3a2 2 0 010-4h.09A1.65 1.65 0 004.6 9a1.65 1.65 0 00-.33-1.82l-.06-.06a2 2 0 012.83-2.83l.06.06A1.65 1.65 0 009 4.68a1.65 1.65 0 001-1.51V3a2 2 0 014 0v.09a1.65 1.65 0 001 1.51 1.65 1.65 0 001.82-.33l.06-.06a2 2 0 012.83 2.83l-.06.06A1.65 1.65 0 0019.4 9a1.65 1.65 0 001.51 1H21a2 2 0 010 4h-.09a1.65 1.65 0 00-1.51 1z"/></svg>' },
    { href: "#/messages", label: "Messages", icon: '<svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M21 15a2 2 0 01-2 2H7l-4 4V5a2 2 0 012-2h14a2 2 0 012 2z"/></svg>' },
    { href: "#/schedulers", label: "Schedulers", icon: '<svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><circle cx="12" cy="12" r="10"/><path d="M12 6v6l4 2"/></svg>' },
  ]
  for (const item of items) {
    const a = document.createElement("a")
    a.href = item.href
    a.innerHTML = `${item.icon}<span>${item.label}</span>`
    nav.appendChild(a)
  }

  const content = document.createElement("main")
  content.id = "content"

  const footer = document.createElement("footer")
  footer.className = "site-footer"
  footer.innerHTML = `<a href="https://wao.eco" target="_blank" rel="noopener">WizardAO</a>`

  app.appendChild(header)
  app.appendChild(nav)
  app.appendChild(content)
  app.appendChild(footer)

  return content
}
