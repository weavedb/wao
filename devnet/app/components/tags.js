import { isAddr } from "../utils.js"
import { navigate } from "../router.js"

const SYSTEM_TAGS = new Set([
  "Data-Protocol", "Variant", "Type", "Module", "Scheduler", "SDK",
  "Content-Type", "Module-Format", "Input-Encoding", "Output-Encoding",
  "Memory-Limit", "Compute-Limit", "Authority", "On-Boot",
  "Cron-Interval", "Cron-Tag-Action", "From-Process", "From-Module",
  "Ref_", "Pushed-For", "Epoch", "Nonce", "Hash-Chain", "Timestamp",
  "Block-Height", "Assignments",
])

function isSystemTag(name) {
  if (SYSTEM_TAGS.has(name)) return true
  if (name.startsWith("Cron-Tag-")) return true
  if (name.startsWith("Ref_")) return true
  return false
}

function renderTagTable(tags) {
  const wrap = document.createElement("div")
  wrap.className = "tags-table-wrap"
  const t = document.createElement("table")
  t.className = "tags-table"
  const thead = document.createElement("thead")
  thead.innerHTML = "<tr><th>Name</th><th>Value</th></tr>"
  t.appendChild(thead)
  const tbody = document.createElement("tbody")
  for (const tag of tags) {
    const tr = document.createElement("tr")
    const tdName = document.createElement("td")
    tdName.className = "tag-key"
    tdName.textContent = tag.name
    tr.appendChild(tdName)

    const tdVal = document.createElement("td")
    tdVal.className = "tag-val mono"
    if (isAddr(tag.value)) {
      const link = document.createElement("span")
      link.className = "link"
      link.textContent = tag.value
      link.addEventListener("click", (e) => {
        e.stopPropagation()
        navigate("#/entity/" + tag.value)
      })
      tdVal.appendChild(link)
    } else {
      tdVal.textContent = tag.value
    }
    tr.appendChild(tdVal)
    tbody.appendChild(tr)
  }
  t.appendChild(tbody)
  wrap.appendChild(t)
  return wrap
}

export function renderTags(tags) {
  const container = document.createElement("div")
  container.className = "tags-container"
  if (!tags || !tags.length) return container

  const system = tags.filter((t) => isSystemTag(t.name))
  const regular = tags.filter((t) => !isSystemTag(t.name))

  if (system.length) {
    const group = document.createElement("div")
    group.className = "tag-group"
    const label = document.createElement("div")
    label.className = "tag-group-label"
    label.textContent = `System (${system.length})`
    group.appendChild(label)
    group.appendChild(renderTagTable(system))
    container.appendChild(group)
  }

  if (regular.length) {
    const group = document.createElement("div")
    group.className = "tag-group"
    const label = document.createElement("div")
    label.className = "tag-group-label"
    label.textContent = `User (${regular.length})`
    group.appendChild(label)
    group.appendChild(renderTagTable(regular))
    container.appendChild(group)
  }

  return container
}
