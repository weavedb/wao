import { queryTxs } from "../api.js"
import { table, replaceRows } from "../components/table.js"
import { pagination } from "../components/pagination.js"
import { navigate } from "../router.js"
import { short, fromNow, tagsMap } from "../utils.js"

const columns = [
  { key: "type", label: "Type", render: (v) => `<span class="type-badge-module">${v}</span>` },
  { key: "name", label: "Name" },
  { key: "id", label: "ID", mono: true, render: (v) => `<span class="link">${short(v)}</span>` },
  { key: "format", label: "Format" },
  { key: "memLimit", label: "Memory" },
  { key: "processes", label: "Processes" },
  { key: "owner", label: "Owner", mono: true, render: (v, row) => v ? `<span class="link" onclick="event.stopPropagation();location.hash='#/entity/${row._owner}'">${short(v)}</span>` : "\u2014" },
  { key: "time", label: "Timestamp" },
]

function mapEdges(edges) {
  return edges.map((e) => {
    const tags = tagsMap(e.node.tags)
    return {
      type: "Module",
      name: tags["Name"] || tags["Module-Name"] || "\u2014",
      id: e.node.id,
      format: tags["Module-Format"] || tags["Content-Type"] || "\u2014",
      memLimit: tags["Memory-Limit"] || "\u2014",
      processes: "\u2026",
      owner: e.node.owner?.address,
      time: fromNow(e.node.block?.timestamp),
      _id: e.node.id,
      _owner: e.node.owner?.address || "",
    }
  })
}

async function loadProcessCounts(rows, tableEl) {
  for (const row of rows) {
    try {
      const procs = await queryTxs({
        first: 100,
        tags: [
          { name: "Type", values: ["Process"] },
          { name: "Module", values: [row._id] },
        ],
      })
      const count = procs?.edges?.length || 0
      row.processes = String(count)
    } catch {
      row.processes = "\u2014"
    }
  }
  const tbody = tableEl.querySelector("tbody")
  if (!tbody) return
  const procColIdx = columns.findIndex((c) => c.key === "processes")
  const trs = tbody.querySelectorAll("tr")
  trs.forEach((tr, i) => {
    if (rows[i] && procColIdx >= 0) {
      const td = tr.children[procColIdx]
      if (td) td.textContent = rows[i].processes
    }
  })
}

export async function modulesView(el) {
  el.innerHTML = '<div class="loading">Loading modules...</div>'

  let pageSize = 25
  const cursors = [null]
  const onClick = (row) => navigate("#/entity/" + row._id)
  let t = null
  let pg = null
  let currentRows = []

  async function loadPage(page) {
    const cursor = cursors[page - 1] ?? null
    const data = await queryTxs({
      first: pageSize,
      after: cursor,
      tags: [{ name: "Type", values: ["Module"] }],
      sort: "HEIGHT_DESC",
    })
    const edges = data?.edges || []
    let rows = mapEdges(edges)
    const count = data?.pageInfo?.count

    // On first page, discover modules referenced by processes but not in Module txs
    if (page === 1) {
      const processData = await queryTxs({
        first: 100,
        tags: [{ name: "Type", values: ["Process"] }],
      })
      const existingIds = new Set(rows.map((r) => r._id))
      const discoveredModules = new Map()
      for (const e of processData?.edges || []) {
        const modId = tagsMap(e.node.tags)["Module"]
        if (modId && !existingIds.has(modId) && !discoveredModules.has(modId)) {
          discoveredModules.set(modId, true)
        }
      }
      for (const modId of discoveredModules.keys()) {
        try {
          const modData = await queryTxs({ ids: [modId] })
          const modEdge = modData?.edges?.[0]
          if (modEdge) {
            rows.unshift(mapEdges([modEdge])[0])
          } else {
            rows.unshift({
              type: "Module",
              name: "Built-in",
              id: modId,
              format: "\u2014",
              memLimit: "\u2014",
              processes: "\u2026",
              owner: "\u2014",
              time: "\u2014",
              _id: modId,
              _owner: "",
            })
          }
        } catch {}
      }
    }

    if (edges.length && data?.pageInfo?.hasNextPage) {
      cursors[page] = edges[edges.length - 1].cursor
    }

    currentRows = rows

    if (t) {
      replaceRows(t, columns, rows, onClick)
    } else {
      t = table(columns, rows, onClick)
    }

    return { hasNext: !!data?.pageInfo?.hasNextPage, count }
  }

  try {
    const { hasNext, count } = await loadPage(1)

    el.innerHTML = '<div class="section-title">Modules</div>'
    el.appendChild(t)

    loadProcessCounts(currentRows, t)

    pg = pagination({
      pageSize,
      onPage: async (page, newSize) => {
        if (newSize && newSize !== pageSize) {
          pageSize = newSize
          cursors.length = 1
          cursors[0] = null
        }
        const res = await loadPage(page)
        loadProcessCounts(currentRows, t)
        pg.update(page, res.hasNext, res.count)
      },
    })
    pg.update(1, hasNext, count)
    el.appendChild(pg.el)
  } catch (err) {
    el.innerHTML = `<div class="loading">Error: ${err.message}</div>`
  }
}
