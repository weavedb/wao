const routes = []
let container = null

// Track blob URLs created during current view for cleanup
const _blobUrls = new Set()
export function trackBlobUrl(url) { _blobUrls.add(url) }
function cleanupBlobUrls() {
  for (const url of _blobUrls) {
    try { URL.revokeObjectURL(url) } catch {}
  }
  _blobUrls.clear()
}

export function route(pattern, handler) {
  const keys = []
  const re = pattern.replace(/:(\w+)/g, (_, k) => {
    keys.push(k)
    return "([^/]+)"
  })
  routes.push({ re: new RegExp("^" + re + "$"), keys, handler })
}

export function navigate(hash) {
  location.hash = hash
}

export function refresh() {
  dispatch()
}

export function start(el) {
  container = el
  window.addEventListener("hashchange", dispatch)
  dispatch()
}

function dispatch() {
  const hash = location.hash.slice(1) || "/"
  for (const r of routes) {
    const m = hash.match(r.re)
    if (m) {
      const params = {}
      r.keys.forEach((k, i) => (params[k] = m[i + 1]))
      cleanupBlobUrls()
      container.innerHTML = ""
      r.handler(container, params)
      updateNav()
      return
    }
  }
  container.innerHTML = "<p>Not found</p>"
}

function updateNav() {
  const hash = location.hash || "#/"
  document.querySelectorAll("nav a").forEach((a) => {
    a.classList.toggle("active", a.getAttribute("href") === hash)
  })
}
