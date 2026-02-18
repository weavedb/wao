/**
 * Etherscan-style numbered pagination with page-size selector.
 *
 * Usage:
 *   const pg = pagination({ onPage, pageSize })
 *   el.appendChild(pg.el)
 *   pg.update(page, hasNext, count)     // after each fetch
 *   pg.pageSize                          // current page size (read-only)
 */
export function pagination({ onPage, pageSize = 25 }) {
  let current = 1
  let maxKnown = 1
  let hasNext = false
  let knownLast = null
  let currentSize = pageSize
  let totalCount = null

  const wrap = document.createElement("div")
  wrap.className = "pagination"

  function render() {
    wrap.innerHTML = ""

    // Left side: page size + total count
    const left = document.createElement("div")
    left.className = "pg-left"

    const sizeWrap = document.createElement("span")
    sizeWrap.className = "pg-size"
    const sizeLabel = document.createElement("span")
    sizeLabel.textContent = "Show "
    sizeWrap.appendChild(sizeLabel)
    const sel = document.createElement("select")
    sel.className = "pg-select"
    for (const s of [10, 25, 50, 100]) {
      const opt = document.createElement("option")
      opt.value = s
      opt.textContent = s
      if (s === currentSize) opt.selected = true
      sel.appendChild(opt)
    }
    sel.addEventListener("change", () => {
      const newSize = Number(sel.value)
      if (newSize === currentSize) return
      currentSize = newSize
      current = 1
      maxKnown = 1
      knownLast = null
      hasNext = false
      render()
      onPage(1, newSize)
    })
    sizeWrap.appendChild(sel)
    sizeWrap.append(" Records")
    left.appendChild(sizeWrap)

    if (totalCount != null) {
      const countEl = document.createElement("span")
      countEl.className = "pg-count"
      countEl.textContent = `(${totalCount.toLocaleString()} total)`
      left.appendChild(countEl)
    }

    wrap.appendChild(left)

    // Right side: navigation buttons
    const right = document.createElement("div")
    right.className = "pg-right"

    const prev = btn("\u2039", () => go(current - 1), current === 1)
    right.appendChild(prev)

    const range = pageRange(current, maxKnown, hasNext)
    for (const p of range) {
      if (p === "...") {
        const dots = document.createElement("span")
        dots.className = "pg-dots"
        dots.textContent = "..."
        right.appendChild(dots)
      } else {
        const b = btn(String(p), () => go(p), false)
        if (p === current) b.classList.add("pg-active")
        right.appendChild(b)
      }
    }

    const next = btn("\u203A", () => go(current + 1), !hasNext)
    right.appendChild(next)

    wrap.appendChild(right)
  }

  function btn(label, onclick, disabled) {
    const b = document.createElement("button")
    b.className = "pg-btn"
    b.textContent = label
    b.disabled = disabled
    if (!disabled) b.addEventListener("click", onclick)
    return b
  }

  function go(page) {
    if (page < 1 || page === current) return
    if (page > maxKnown + 1) return
    current = page
    render()
    onPage(page)
  }

  function update(page, _hasNext, _count) {
    current = page
    hasNext = _hasNext
    if (_count != null) totalCount = _count
    if (page > maxKnown) maxKnown = page
    if (_hasNext && page + 1 > maxKnown) maxKnown = page
    if (!_hasNext && page > 0) knownLast = page
    render()
  }

  render()
  return { el: wrap, update, get pageSize() { return currentSize } }
}

/** Compute which page numbers to show. */
function pageRange(current, maxKnown, hasNext) {
  const last = hasNext ? maxKnown + 1 : maxKnown
  if (last <= 7) {
    return Array.from({ length: last }, (_, i) => i + 1)
  }
  const pages = new Set([1])
  for (let i = Math.max(2, current - 1); i <= Math.min(last, current + 1); i++) {
    pages.add(i)
  }
  pages.add(last)
  const sorted = [...pages].sort((a, b) => a - b)
  const result = []
  let prev = 0
  for (const p of sorted) {
    if (prev && p - prev > 1) result.push("...")
    result.push(p)
    prev = p
  }
  return result
}
