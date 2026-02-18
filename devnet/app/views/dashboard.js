import { getArInfo, queryBlocks, queryTxs } from "../api.js"
import { table } from "../components/table.js"
import { navigate } from "../router.js"
import { short, fromNow, tagsMap, contentTypeLabel, typeBadgeHtml } from "../utils.js"

export async function dashboardView(el) {
  el.innerHTML = '<div class="loading">Loading...</div>'

  try {
    const [info, blocks, txs, procs, msgs, mods, allTxs] = await Promise.all([
      getArInfo(),
      queryBlocks({ first: 5 }),
      queryTxs({ first: 5, sort: "HEIGHT_DESC" }),
      queryTxs({ first: 100, tags: [{ name: "Type", values: ["Process"] }] }),
      queryTxs({ first: 5, tags: [{ name: "Type", values: ["Message"] }], sort: "HEIGHT_DESC" }),
      queryTxs({ first: 100, tags: [{ name: "Type", values: ["Module"] }] }),
      queryTxs({ first: 100, sort: "HEIGHT_DESC" }),
    ])

    el.innerHTML = ""

    // Stats
    const stats = document.createElement("div")
    stats.className = "stats"
    const cards = [
      ["Block Height", info.height ?? "\u2014"],
      ["Network", info.network ?? "\u2014"],
      ["Transactions", (allTxs?.edges || []).length],
      ["Processes", (procs?.edges || []).length],
      ["Modules", (mods?.edges || []).length],
    ]
    for (const [label, value] of cards) {
      const card = document.createElement("div")
      card.className = "stat-card"
      card.innerHTML = `<div class="label">${label}</div><div class="value">${value}</div>`
      stats.appendChild(card)
    }
    el.appendChild(stats)

    // Latest Blocks
    const blockSection = document.createElement("div")
    blockSection.className = "section"
    blockSection.innerHTML = '<div class="section-title">Latest Blocks</div>'
    const blockRows = (blocks?.edges || []).map((e) => ({
      height: e.node.height,
      id: e.node.id,
      time: fromNow(e.node.timestamp),
      _id: e.node.id,
      _height: e.node.height,
    }))
    blockSection.appendChild(
      table(
        [
          { key: "height", label: "Height", render: (v) => `<span class="type-badge-block">#${v}</span>` },
          { key: "id", label: "ID", mono: true, render: (v) => `<span class="link">${v}</span>` },
          { key: "time", label: "Time" },
        ],
        blockRows,
        (row) => navigate("#/entity/" + (row._height ?? row._id)),
      ),
    )
    el.appendChild(blockSection)

    // Latest Transactions
    const txSection = document.createElement("div")
    txSection.className = "section"
    txSection.innerHTML = '<div class="section-title">Latest Transactions</div>'
    const txRows = (txs?.edges || []).map((e) => {
      const tags = tagsMap(e.node.tags)
      return {
        type: tags["Type"] || contentTypeLabel(e.node.data?.type || tags["Content-Type"]) || "TX",
        id: e.node.id,
        owner: e.node.owner?.address,
        time: fromNow(e.node.block?.timestamp),
        _id: e.node.id,
        _owner: e.node.owner?.address || "",
      }
    })
    txSection.appendChild(
      table(
        [
          { key: "type", label: "Type", render: (v) => typeBadgeHtml(v) },
          { key: "id", label: "ID", mono: true, render: (v) => `<span class="link">${v}</span>` },
          { key: "owner", label: "Owner", mono: true, render: (v, r) => `<span class="link" onclick="event.stopPropagation();location.hash='#/entity/${r._owner}'">${short(v)}</span>` },
          { key: "time", label: "Time" },
        ],
        txRows,
        (row) => navigate("#/entity/" + row._id),
      ),
    )
    el.appendChild(txSection)

    // Latest Messages (AO)
    const msgSection = document.createElement("div")
    msgSection.className = "section"
    msgSection.innerHTML = '<div class="section-title">Latest Messages</div>'
    const msgRows = (msgs?.edges || []).map((e) => {
      const tags = tagsMap(e.node.tags)
      return {
        action: tags["Action"] || "\u2014",
        id: e.node.id,
        from: e.node.owner?.address,
        to: e.node.recipient || "\u2014",
        time: fromNow(e.node.block?.timestamp),
        _id: e.node.id,
        _from: e.node.owner?.address || "",
        _to: e.node.recipient || "",
      }
    })
    msgSection.appendChild(
      table(
        [
          { key: "action", label: "Action", render: (v) => `<span class="type-badge-message">${v}</span>` },
          { key: "id", label: "ID", mono: true, render: (v) => `<span class="link">${v}</span>` },
          { key: "from", label: "From", mono: true, render: (v, r) => `<span class="link" onclick="event.stopPropagation();location.hash='#/entity/${r._from}'">${short(v)}</span>` },
          { key: "to", label: "To", mono: true, render: (v, r) => r._to ? `<span class="link" onclick="event.stopPropagation();location.hash='#/entity/${r._to}'">${short(v)}</span>` : v },
          { key: "time", label: "Time" },
        ],
        msgRows,
        (row) => navigate("#/entity/" + row._id),
      ),
    )
    el.appendChild(msgSection)
  } catch (err) {
    el.innerHTML = `<div class="loading">Error loading dashboard: ${err.message}</div>`
  }
}
