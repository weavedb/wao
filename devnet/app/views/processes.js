import { queryTxs } from "../api.js"
import { table, replaceRows } from "../components/table.js"
import { pagination } from "../components/pagination.js"
import { navigate } from "../router.js"
import { short, fromNow, tagsMap } from "../utils.js"

const columns = [
  { key: "type", label: "Type", render: (v) => `<span class="type-badge-process">${v}</span>` },
  { key: "name", label: "Name" },
  { key: "id", label: "ID", mono: true, render: (v) => `<span class="link">${short(v)}</span>` },
  { key: "module", label: "Module", mono: true, render: (v, row) => v !== "\u2014" ? `<span class="link" onclick="event.stopPropagation();location.hash='#/entity/${row._module}'">${short(v)}</span>` : v },
  { key: "owner", label: "Owner", mono: true, render: (v, row) => `<span class="link" onclick="event.stopPropagation();location.hash='#/entity/${row._owner}'">${short(v)}</span>` },
  { key: "time", label: "Timestamp" },
]

function mapEdges(edges) {
  return edges.map((e) => {
    const tags = tagsMap(e.node.tags)
    return {
      type: "Process",
      name: tags["Name"] || "\u2014",
      id: e.node.id,
      module: tags["Module"] || "\u2014",
      owner: e.node.owner?.address,
      time: fromNow(e.node.block?.timestamp),
      _id: e.node.id,
      _module: tags["Module"] || "",
      _owner: e.node.owner?.address || "",
    }
  })
}

export async function processesView(el) {
  el.innerHTML = '<div class="loading">Loading processes...</div>'

  let pageSize = 25
  const cursors = [null]
  const onClick = (row) => navigate("#/entity/" + row._id)
  let t = null
  let pg = null

  async function loadPage(page) {
    const cursor = cursors[page - 1] ?? null
    const data = await queryTxs({
      first: pageSize,
      after: cursor,
      tags: [{ name: "Type", values: ["Process"] }],
      sort: "HEIGHT_DESC",
    })
    const edges = data?.edges || []
    const rows = mapEdges(edges)
    const count = data?.pageInfo?.count

    if (edges.length && data?.pageInfo?.hasNextPage) {
      cursors[page] = edges[edges.length - 1].cursor
    }

    if (t) {
      replaceRows(t, columns, rows, onClick)
    } else {
      t = table(columns, rows, onClick)
    }

    return { hasNext: !!data?.pageInfo?.hasNextPage, count }
  }

  try {
    const { hasNext, count } = await loadPage(1)

    el.innerHTML = '<div class="section-title">Processes</div>'
    el.appendChild(t)

    pg = pagination({
      pageSize,
      onPage: async (page, newSize) => {
        if (newSize && newSize !== pageSize) {
          pageSize = newSize
          cursors.length = 1
          cursors[0] = null
        }
        const res = await loadPage(page)
        pg.update(page, res.hasNext, res.count)
      },
    })
    pg.update(1, hasNext, count)
    el.appendChild(pg.el)
  } catch (err) {
    el.innerHTML = `<div class="loading">Error: ${err.message}</div>`
  }
}
