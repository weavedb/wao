export function table(columns, rows, onClick) {
  const wrap = document.createElement("div")
  wrap.className = "table-wrap"

  const t = document.createElement("table")
  const thead = document.createElement("thead")
  const hr = document.createElement("tr")
  for (const col of columns) {
    const th = document.createElement("th")
    th.textContent = col.label
    hr.appendChild(th)
  }
  thead.appendChild(hr)
  t.appendChild(thead)

  const tbody = document.createElement("tbody")
  for (const row of rows) {
    const tr = document.createElement("tr")
    if (onClick) tr.addEventListener("click", (e) => onClick(row, e))
    for (const col of columns) {
      const td = document.createElement("td")
      const val = row[col.key]
      if (col.render) {
        const rendered = col.render(val, row)
        if (typeof rendered === "string") {
          td.innerHTML = rendered
        } else if (rendered instanceof Node) {
          td.appendChild(rendered)
        }
      } else {
        td.textContent = val ?? ""
      }
      td.className = col.mono ? "mono" : ""
      tr.appendChild(td)
    }
    tbody.appendChild(tr)
  }
  t.appendChild(tbody)
  wrap.appendChild(t)
  return wrap
}

export function replaceRows(tableWrap, columns, rows, onClick) {
  const tbody = tableWrap.querySelector("tbody")
  if (!tbody) return
  tbody.innerHTML = ""
  for (const row of rows) {
    const tr = document.createElement("tr")
    if (onClick) tr.addEventListener("click", (e) => onClick(row, e))
    for (const col of columns) {
      const td = document.createElement("td")
      const val = row[col.key]
      if (col.render) {
        const rendered = col.render(val, row)
        if (typeof rendered === "string") {
          td.innerHTML = rendered
        } else if (rendered instanceof Node) {
          td.appendChild(rendered)
        }
      } else {
        td.textContent = val ?? ""
      }
      td.className = col.mono ? "mono" : ""
      tr.appendChild(td)
    }
    tbody.appendChild(tr)
  }
}

export function appendRows(tableWrap, columns, rows, onClick) {
  const tbody = tableWrap.querySelector("tbody")
  for (const row of rows) {
    const tr = document.createElement("tr")
    if (onClick) tr.addEventListener("click", (e) => onClick(row, e))
    for (const col of columns) {
      const td = document.createElement("td")
      const val = row[col.key]
      if (col.render) {
        const rendered = col.render(val, row)
        if (typeof rendered === "string") {
          td.innerHTML = rendered
        } else if (rendered instanceof Node) {
          td.appendChild(rendered)
        }
      } else {
        td.textContent = val ?? ""
      }
      td.className = col.mono ? "mono" : ""
      tr.appendChild(td)
    }
    tbody.appendChild(tr)
  }
}

export function prependRow(tableWrap, columns, row, onClick, maxRows = 0) {
  const tbody = tableWrap.querySelector("tbody")
  if (!tbody) return
  const tr = document.createElement("tr")
  tr.className = "new-row"
  if (onClick) tr.addEventListener("click", (e) => onClick(row, e))
  for (const col of columns) {
    const td = document.createElement("td")
    const val = row[col.key]
    if (col.render) {
      const rendered = col.render(val, row)
      if (typeof rendered === "string") {
        td.innerHTML = rendered
      } else if (rendered instanceof Node) {
        td.appendChild(rendered)
      }
    } else {
      td.textContent = val ?? ""
    }
    td.className = col.mono ? "mono" : ""
    tr.appendChild(td)
  }
  tbody.insertBefore(tr, tbody.firstChild)
  // Trim excess rows
  if (maxRows > 0) {
    while (tbody.children.length > maxRows) {
      tbody.removeChild(tbody.lastChild)
    }
  }
}
