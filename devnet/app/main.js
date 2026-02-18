import { route, start } from "./router.js"
import { createLayout } from "./components/layout.js"
import { dashboardView } from "./views/dashboard.js"
import { blocksView } from "./views/blocks.js"
import { transactionsView } from "./views/transactions.js"
import { processesView } from "./views/processes.js"
import { messagesView } from "./views/messages.js"
import { modulesView } from "./views/modules.js"
import { entityView } from "./views/entity.js"
import { schedulersView } from "./views/schedulers.js"
import { queryTxs } from "./api.js"
import { prependRow } from "./components/table.js"
import { short, fromNow, tagsMap, contentTypeLabel, typeBadgeHtml } from "./utils.js"
import { navigate } from "./router.js"

const app = document.getElementById("app")
const content = createLayout(app)

route("/", dashboardView)
route("/blocks", blocksView)
route("/transactions", transactionsView)
route("/modules", modulesView)
route("/processes", processesView)
route("/messages", messagesView)
route("/schedulers", schedulersView)
route("/entity/:id", entityView)

start(content)

// --- Column definitions (shared with dashboard/list views) ---
const txColumns = [
  { key: "type", label: "Type", render: (v) => typeBadgeHtml(v) },
  { key: "id", label: "ID", mono: true, render: (v) => `<span class="link">${v}</span>` },
  { key: "owner", label: "Owner", mono: true, render: (v, r) => `<span class="link" onclick="event.stopPropagation();location.hash='#/entity/${r._owner}'">${short(v)}</span>` },
  { key: "time", label: "Time" },
]

const msgColumns = [
  { key: "action", label: "Action", render: (v) => `<span class="type-badge-message">${v}</span>` },
  { key: "id", label: "ID", mono: true, render: (v) => `<span class="link">${v}</span>` },
  { key: "from", label: "From", mono: true, render: (v, r) => `<span class="link" onclick="event.stopPropagation();location.hash='#/entity/${r._from}'">${short(v)}</span>` },
  { key: "to", label: "To", mono: true, render: (v, r) => r._to ? `<span class="link" onclick="event.stopPropagation();location.hash='#/entity/${r._to}'">${short(v)}</span>` : v },
  { key: "time", label: "Time" },
]

const procColumns = [
  { key: "name", label: "Name", render: (v) => `<span class="type-badge-process">${v}</span>` },
  { key: "id", label: "ID", mono: true, render: (v) => `<span class="link">${v}</span>` },
  { key: "owner", label: "Owner", mono: true, render: (v, r) => `<span class="link" onclick="event.stopPropagation();location.hash='#/entity/${r._owner}'">${short(v)}</span>` },
  { key: "time", label: "Time" },
]

const schedColumns = [
  { key: "type", label: "Type", render: (v) => `<span class="type-badge-scheduler">${v}</span>` },
  { key: "id", label: "Scheduler", mono: true, render: (v) => `<span class="link">${short(v)}</span>` },
  { key: "url", label: "URL" },
  { key: "ttl", label: "TTL" },
  { key: "owner", label: "Owner", mono: true, render: (v, r) => `<span class="link" onclick="event.stopPropagation();location.hash='#/entity/${r._owner}'">${short(v)}</span>` },
  { key: "time", label: "Time" },
]

// Track seen IDs to avoid duplicate prepends
const seenIds = new Set()

// --- Find table by section title ---
function findTableByTitle(title) {
  const sections = document.querySelectorAll(".section")
  for (const sec of sections) {
    const t = sec.querySelector(".section-title")
    if (t && t.textContent.includes(title)) {
      return sec.querySelector(".table-wrap")
    }
  }
  return null
}

// --- Incremental stat update ---
function bumpStat(label, delta = 1) {
  const cards = document.querySelectorAll(".stat-card")
  for (const card of cards) {
    const lbl = card.querySelector(".label")
    if (lbl && lbl.textContent === label) {
      const valEl = card.querySelector(".value")
      const n = parseInt(valEl.textContent)
      if (!isNaN(n)) valEl.textContent = n + delta
      break
    }
  }
}

// --- Handle a single WS tx event ---
async function handleWsTx(msg) {
  if (!msg.id || seenIds.has(msg.id)) return
  seenIds.add(msg.id)
  // Keep set bounded
  if (seenIds.size > 500) {
    const iter = seenIds.values()
    for (let i = 0; i < 100; i++) seenIds.delete(iter.next().value)
  }

  // Fetch full tx details
  let tx
  try {
    const result = await queryTxs({ ids: [msg.id], first: 1 })
    const edge = result?.edges?.[0]
    if (!edge) return
    tx = edge.node
  } catch { return }

  const tags = tagsMap(tx.tags)
  const type = tags["Bundle-Format"] ? "Bundle" : tags["Type"] || contentTypeLabel(tx.data?.type || tags["Content-Type"]) || "TX"
  const now = fromNow(msg.timestamp || Date.now())
  const onClick = (row) => navigate("#/entity/" + row._id)

  // Prepend to "Latest Transactions" / "Transactions" table
  const txTable = findTableByTitle("Latest Transactions") || findTableByTitle("Transactions")
  if (txTable) {
    prependRow(txTable, txColumns, {
      type,
      id: tx.id,
      owner: tx.owner?.address,
      time: now,
      _id: tx.id,
      _owner: tx.owner?.address || "",
    }, onClick, 20)
  }

  // Prepend to "Latest Messages" / "Messages" table if it's a message
  if (type === "Message") {
    const msgTable = findTableByTitle("Latest Messages") || findTableByTitle("Messages")
    if (msgTable) {
      prependRow(msgTable, msgColumns, {
        action: tags["Action"] || "\u2014",
        id: tx.id,
        from: tx.owner?.address,
        to: tx.recipient || "\u2014",
        time: now,
        _id: tx.id,
        _from: tx.owner?.address || "",
        _to: tx.recipient || "",
      }, onClick, 20)
    }
  }

  // Prepend to "Processes" table if it's a process
  if (type === "Process") {
    const procTable = findTableByTitle("Processes")
    if (procTable) {
      prependRow(procTable, procColumns, {
        name: tags["Name"] || short(tx.id),
        id: tx.id,
        owner: tx.owner?.address,
        time: now,
        _id: tx.id,
        _owner: tx.owner?.address || "",
      }, onClick, 50)
    }
    bumpStat("Processes")
  }

  // Prepend to "Schedulers" table if it's a scheduler tx
  if (type === "Scheduler-Location" || type === "Scheduler-Transfer") {
    const schedTable = findTableByTitle("Schedulers")
    if (schedTable) {
      prependRow(schedTable, schedColumns, {
        type,
        id: tx.id,
        url: tags["Url"] || "\u2014",
        ttl: tags["Time-To-Live"] || "\u2014",
        owner: tx.owner?.address,
        time: now,
        _id: tx.id,
        _owner: tx.owner?.address || "",
      }, onClick, 50)
    }
  }

  // Bump Transactions counter
  bumpStat("Transactions")
}

// --- WebSocket real-time updates ---
function connectWS() {
  const proto = location.protocol === "https:" ? "wss:" : "ws:"
  const ws = new WebSocket(`${proto}//${location.host}/ws`)
  ws.onmessage = (e) => {
    try {
      const msg = JSON.parse(e.data)
      if (msg.type === "tx" && msg.id) {
        handleWsTx(msg)
      }
    } catch {}
    updateLiveIndicator(true)
  }
  ws.onopen = () => updateLiveIndicator(true)
  ws.onclose = () => {
    updateLiveIndicator(false)
    setTimeout(connectWS, 3000)
  }
  ws.onerror = () => ws.close()
}

function updateLiveIndicator(connected) {
  let dot = document.querySelector(".live-dot")
  if (!dot) {
    dot = document.createElement("span")
    dot.className = "live-dot"
    const logo = document.querySelector(".logo")
    if (logo) logo.appendChild(dot)
  }
  dot.classList.toggle("connected", connected)
  dot.title = connected ? "Live \u2014 real-time updates" : "Disconnected \u2014 reconnecting..."
}

connectWS()
