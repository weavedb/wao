import { queryTxs } from "../api.js"
import { table, replaceRows } from "../components/table.js"
import { pagination } from "../components/pagination.js"
import { navigate } from "../router.js"
import { short, fromNow, tagsMap, contentTypeLabel, typeBadgeHtml } from "../utils.js"

const columns = [
  { key: "type", label: "Type", render: (v) => typeBadgeHtml(v) },
  { key: "id", label: "ID", mono: true, render: (v) => `<span class="link">${v}</span>` },
  { key: "owner", label: "Owner", mono: true, render: (v, row) => `<span class="link" onclick="event.stopPropagation();location.hash='#/entity/${row._owner}'">${short(v)}</span>` },
  { key: "recipient", label: "Recipient", mono: true, render: (v, row) => row._recipient ? `<span class="link" onclick="event.stopPropagation();location.hash='#/entity/${row._recipient}'">${short(v)}</span>` : v },
  { key: "block", label: "Block" },
  { key: "time", label: "Timestamp" },
]

function mapEdges(edges) {
  return edges.filter(e => !e.node.bundledIn?.id).map((e) => {
    const tags = tagsMap(e.node.tags)
    return {
      id: e.node.id,
      type: tags["Bundle-Format"] ? "Bundle" : tags["Type"] || contentTypeLabel(e.node.data?.type || tags["Content-Type"]) || "TX",
      owner: e.node.owner?.address,
      recipient: e.node.recipient || "\u2014",
      block: e.node.block?.height ?? "\u2014",
      time: fromNow(e.node.block?.timestamp),
      _id: e.node.id,
      _owner: e.node.owner?.address || "",
      _recipient: e.node.recipient || "",
    }
  })
}


export async function transactionsView(el) {
  el.innerHTML = '<div class="loading">Loading transactions...</div>'

  let pageSize = 25
  const cursors = [null]
  const onClick = (row) => {
    navigate("#/entity/" + row._id)
  }
  let t = null
  let pg = null

  async function loadPage(page) {
    const cursor = cursors[page - 1] ?? null
    const data = await queryTxs({ first: pageSize, after: cursor, sort: "HEIGHT_DESC" })
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

    el.innerHTML = '<div class="section-title">Transactions</div>'
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
