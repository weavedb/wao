import { navigate } from "../router.js"
import { queryTxs } from "../api.js"
import { short, tagsMap, contentTypeLabel, typeBadgeClass } from "../utils.js"

let debounceTimer = null

export function createSearch() {
  const wrapper = document.createElement("div")
  wrapper.className = "search-wrapper"

  const form = document.createElement("form")
  form.className = "search-bar"

  const input = document.createElement("input")
  input.type = "text"
  input.placeholder = "Search by ID, address, or height..."

  const btn = document.createElement("button")
  btn.type = "submit"
  btn.innerHTML = '<svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><circle cx="11" cy="11" r="8"/><path d="m21 21-4.3-4.3"/></svg>'

  form.appendChild(input)
  form.appendChild(btn)
  wrapper.appendChild(form)

  // Dropdown
  const dropdown = document.createElement("div")
  dropdown.className = "search-dropdown"
  dropdown.style.display = "none"
  wrapper.appendChild(dropdown)

  function hideDropdown() {
    dropdown.style.display = "none"
    dropdown.innerHTML = ""
  }

  form.addEventListener("submit", (e) => {
    e.preventDefault()
    const val = input.value.trim()
    if (val) {
      hideDropdown()
      navigate("#/entity/" + val)
      input.value = ""
    }
  })

  input.addEventListener("input", () => {
    const val = input.value.trim()
    if (val.length < 2) {
      hideDropdown()
      return
    }
    if (debounceTimer) clearTimeout(debounceTimer)
    debounceTimer = setTimeout(() => searchLive(val, dropdown), 250)
  })

  input.addEventListener("focus", () => {
    const val = input.value.trim()
    if (val.length >= 2 && dropdown.children.length) {
      dropdown.style.display = "block"
    }
  })

  document.addEventListener("click", (e) => {
    if (!wrapper.contains(e.target)) hideDropdown()
  })

  input.addEventListener("keydown", (e) => {
    if (e.key === "Escape") {
      hideDropdown()
      input.blur()
    }
  })

  return wrapper
}

async function searchLive(query, dropdown) {
  try {
    // Search by ID
    const [byId, byOwner] = await Promise.all([
      queryTxs({ ids: [query], first: 5 }).catch(() => null),
      query.length >= 10
        ? queryTxs({ owners: [query], first: 5, sort: "HEIGHT_DESC" }).catch(() => null)
        : null,
    ])

    const results = []
    const seen = new Set()

    for (const edge of byId?.edges || []) {
      if (!seen.has(edge.node.id)) {
        seen.add(edge.node.id)
        results.push(edge)
      }
    }
    for (const edge of byOwner?.edges || []) {
      if (!seen.has(edge.node.id)) {
        seen.add(edge.node.id)
        results.push(edge)
      }
    }

    dropdown.innerHTML = ""

    if (!results.length) {
      // Show "Go to" option for direct navigation
      const item = document.createElement("div")
      item.className = "search-item"
      item.innerHTML = `<span class="search-item-label">Go to</span><span class="mono">${short(query, 20)}</span>`
      item.addEventListener("click", () => {
        dropdown.style.display = "none"
        navigate("#/entity/" + query)
        dropdown.parentElement.querySelector("input").value = ""
      })
      dropdown.appendChild(item)
      dropdown.style.display = "block"
      return
    }

    for (const edge of results.slice(0, 8)) {
      const node = edge.node
      const tags = tagsMap(node.tags)
      const type = tags["Type"] || contentTypeLabel(node.data?.type || tags["Content-Type"]) || "TX"
      const name = tags["Name"] || tags["Action"] || ""

      const item = document.createElement("div")
      item.className = "search-item"
      item.innerHTML = `
        <span class="${typeBadgeClass(type)}">${type}</span>
        <span class="mono search-item-id">${short(node.id)}</span>
        ${name ? `<span class="search-item-name">${name}</span>` : ""}
      `
      item.addEventListener("click", () => {
        dropdown.style.display = "none"
        navigate("#/entity/" + node.id)
        dropdown.parentElement.querySelector("input").value = ""
      })
      dropdown.appendChild(item)
    }

    // Also add "View all" link
    if (byOwner?.edges?.length) {
      const viewAll = document.createElement("div")
      viewAll.className = "search-item search-item-viewall"
      viewAll.innerHTML = `<span>View address: ${short(query)}</span>`
      viewAll.addEventListener("click", () => {
        dropdown.style.display = "none"
        navigate("#/entity/" + query)
        dropdown.parentElement.querySelector("input").value = ""
      })
      dropdown.appendChild(viewAll)
    }

    dropdown.style.display = "block"
  } catch {
    // silently fail
  }
}
