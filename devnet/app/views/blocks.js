import { queryBlocks } from "../api.js"
import { table, replaceRows } from "../components/table.js"
import { pagination } from "../components/pagination.js"
import { navigate } from "../router.js"
import { fromNow } from "../utils.js"

const columns = [
  { key: "height", label: "Height" },
  { key: "id", label: "ID", mono: true, render: (v) => `<span class="link">${v}</span>` },
  { key: "time", label: "Timestamp" },
]

function mapEdges(edges) {
  return edges.map((e) => ({
    height: e.node.height,
    id: e.node.id,
    time: fromNow(e.node.timestamp),
    _height: e.node.height,
    _id: e.node.id,
  }))
}

export async function blocksView(el) {
  el.innerHTML = '<div class="loading">Loading blocks...</div>'

  let pageSize = 25
  const cursors = [null]
  const onClick = (row) => navigate("#/entity/" + (row._height ?? row._id))
  let t = null
  let pg = null

  async function loadPage(page) {
    const cursor = cursors[page - 1] ?? null
    const data = await queryBlocks({ first: pageSize, after: cursor })
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

    el.innerHTML = '<div class="section-title">Blocks</div>'
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
