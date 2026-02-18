export function short(id, n = 6) {
  if (!id || id.length <= n * 2 + 3) return id || ""
  return id.slice(0, n) + "..." + id.slice(-n)
}

function toMs(ts) {
  if (!ts) return 0
  // If ts > 1e12 it's already milliseconds, otherwise treat as seconds
  return ts > 1e12 ? ts : ts * 1000
}

export function fromNow(ts) {
  if (!ts) return ""
  const msTs = toMs(ts)
  const sec = Math.floor((Date.now() - msTs) / 1000)
  if (sec < 0) return "just now"
  if (sec < 60) return sec + "s ago"
  const min = Math.floor(sec / 60)
  if (min < 60) return min + "m ago"
  const hr = Math.floor(min / 60)
  if (hr < 24) return hr + "h ago"
  const d = Math.floor(hr / 24)
  return d + "d ago"
}

export function tagsMap(arr) {
  const m = {}
  if (arr) arr.forEach((t) => (m[t.name] = t.value))
  return m
}

export function isAddr(s) {
  return typeof s === "string" && /^[a-zA-Z0-9_-]{43}$/.test(s)
}

export function formatDate(ts) {
  if (!ts) return ""
  return new Date(toMs(ts)).toLocaleString()
}

export function typeBadgeClass(type) {
  const t = (type || "").toLowerCase()
  if (t === "process") return "type-badge-process"
  if (t === "message") return "type-badge-message"
  if (t === "module") return "type-badge-module"
  if (t === "block") return "type-badge-block"
  if (t === "bundle") return "type-badge-bundle"
  if (t === "scheduler-location" || t === "scheduler-transfer") return "type-badge-scheduler"
  if (t === "image") return "type-badge-image"
  if (t === "video") return "type-badge-video"
  if (t === "audio") return "type-badge-audio"
  if (t === "json") return "type-badge-json"
  if (t === "markdown" || t === "html" || t === "css" || t === "text" ||
      t === "javascript" || t === "lua" || t === "svg") return "type-badge-text"
  return "type-badge-tx"
}

export function typeBadgeHtml(type) {
  return `<span class="${typeBadgeClass(type)}">${type || "TX"}</span>`
}

export function contentTypeLabel(dataType) {
  if (!dataType) return null
  if (dataType.startsWith("image/")) return "Image"
  if (dataType.startsWith("video/")) return "Video"
  if (dataType.startsWith("audio/")) return "Audio"
  if (dataType.includes("json")) return "JSON"
  if (dataType === "text/markdown") return "Markdown"
  if (dataType === "text/html") return "HTML"
  if (dataType === "text/css") return "CSS"
  if (dataType.includes("javascript")) return "JavaScript"
  if (dataType.includes("svg")) return "SVG"
  if (dataType.includes("wasm")) return "WASM"
  if (dataType === "text/x-lua") return "Lua"
  if (dataType.startsWith("text/")) return "Text"
  return null
}
