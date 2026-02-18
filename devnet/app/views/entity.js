import { queryTxs, queryBlocks, getResults, getResult, getTxData } from "../api.js"
import { table } from "../components/table.js"
import { renderTags } from "../components/tags.js"
import { navigate, trackBlobUrl } from "../router.js"
import { short, fromNow, tagsMap, formatDate, contentTypeLabel, typeBadgeClass, typeBadgeHtml } from "../utils.js"

export async function entityView(el, { id }) {
  el.innerHTML = '<div class="loading">Loading...</div>'

  try {
    if (/^\d+$/.test(id)) {
      await renderBlock(el, parseInt(id))
      return
    }

    let data = await queryTxs({ ids: [id] })
    let edge = data?.edges?.[0]

    // Retry once — GQL index may not have caught up yet
    if (!edge) {
      await new Promise(r => setTimeout(r, 500))
      data = await queryTxs({ ids: [id] })
      edge = data?.edges?.[0]
    }

    if (!edge) {
      const blocks = await queryBlocks({ first: 1 })
      const found = blocks?.edges?.find((e) => e.node.id === id)
      if (found) {
        await renderBlockNode(el, found.node)
        return
      }

      const ownerTxs = await queryTxs({ owners: [id], first: 50 })
      if (ownerTxs?.edges?.length) {
        await renderAddress(el, id, ownerTxs)
        return
      }

      el.innerHTML = `<div class="loading">Entity not found: ${short(id)}</div>`
      return
    }

    const node = edge.node
    const tags = tagsMap(node.tags)
    const type = tags["Type"] || "Transaction"

    if (type === "Process") {
      await renderProcess(el, node, tags)
    } else if (type === "Message") {
      await renderMessage(el, node, tags)
    } else if (type === "Module") {
      await renderModule(el, node, tags)
    } else {
      await renderGeneric(el, node, tags, type)
    }
  } catch (err) {
    el.innerHTML = `<div class="loading">Error: ${err.message}</div>`
  }
}

// ---- helpers ----

function detailHeader(type, id, name) {
  const div = document.createElement("div")
  div.className = "detail-header"
  let html = `<span class="${typeBadgeClass(type)}">${type}</span>`
  if (name) html += `<div class="entity-name">${name}</div>`
  html += `<div class="entity-id"><h2>${id}</h2><button class="copy-btn" title="Copy to clipboard"><svg width="14" height="14" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><rect x="9" y="9" width="13" height="13" rx="2"/><path d="M5 15H4a2 2 0 01-2-2V4a2 2 0 012-2h9a2 2 0 012 2v1"/></svg></button></div>`
  div.innerHTML = html
  const copyBtn = div.querySelector(".copy-btn")
  copyBtn.addEventListener("click", async () => {
    try {
      await navigator.clipboard.writeText(id)
      copyBtn.innerHTML = '<svg width="14" height="14" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><polyline points="20 6 9 17 4 12"/></svg>'
      setTimeout(() => {
        copyBtn.innerHTML = '<svg width="14" height="14" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><rect x="9" y="9" width="13" height="13" rx="2"/><path d="M5 15H4a2 2 0 01-2-2V4a2 2 0 012-2h9a2 2 0 012 2v1"/></svg>'
      }, 1500)
    } catch {}
  })
  return div
}

function detailFields(fields) {
  const div = document.createElement("div")
  div.className = "detail-fields"
  for (const [key, value] of fields) {
    const field = document.createElement("div")
    field.className = "field"
    field.innerHTML = `<div class="field-key">${key}</div><div class="field-value">${value ?? "\u2014"}</div>`
    div.appendChild(field)
  }
  return div
}

function link(text, target) {
  return `<span class="link mono" onclick="location.hash='#/entity/${target}'">${text}</span>`
}

function msgTable(edges) {
  const rows = edges.map((e) => {
    const mt = tagsMap(e.node.tags)
    return {
      action: mt["Action"] || "\u2014",
      id: e.node.id,
      from: e.node.owner?.address || "\u2014",
      to: e.node.recipient || "\u2014",
      block: e.node.block?.height ?? "\u2014",
      time: fromNow(e.node.block?.timestamp),
      _id: e.node.id,
      _from: e.node.owner?.address || "",
      _to: e.node.recipient || "",
    }
  })
  return table(
    [
      { key: "action", label: "Action", render: (v) => `<span class="type-badge-message">${v}</span>` },
      { key: "id", label: "ID", mono: true, render: (v) => `<span class="link">${short(v)}</span>` },
      { key: "from", label: "From", mono: true, render: (v, r) => r._from ? `<span class="link" onclick="event.stopPropagation();location.hash='#/entity/${r._from}'">${short(v)}</span>` : v },
      { key: "to", label: "To", mono: true, render: (v, r) => r._to ? `<span class="link" onclick="event.stopPropagation();location.hash='#/entity/${r._to}'">${short(v)}</span>` : v },
      { key: "block", label: "Block" },
      { key: "time", label: "Time" },
    ],
    rows,
    (row) => navigate("#/entity/" + row._id),
  )
}

function section(title, content) {
  const div = document.createElement("div")
  div.className = "section"
  div.innerHTML = `<div class="section-title">${title}</div>`
  if (typeof content === "string") {
    div.innerHTML += content
  } else if (content) {
    div.appendChild(content)
  }
  return div
}

function twoCol(header, fields, tagsEl) {
  const grid = document.createElement("div")
  grid.className = "detail-grid"
  const left = document.createElement("div")
  left.appendChild(header)
  left.appendChild(fields)
  grid.appendChild(left)
  grid.appendChild(tagsEl)
  return grid
}

// ---- tabs ----

function createTabs(tabs) {
  const wrap = document.createElement("div")
  wrap.className = "entity-tabs"

  const tabBar = document.createElement("div")
  tabBar.className = "tab-bar"

  const tabContent = document.createElement("div")
  tabContent.className = "tab-content"

  tabs.forEach((t, i) => {
    const btn = document.createElement("button")
    btn.className = "tab-btn" + (i === 0 ? " active" : "")
    btn.textContent = t.label + (t.count != null ? ` (${t.count})` : "")
    btn.addEventListener("click", () => {
      tabBar.querySelectorAll(".tab-btn").forEach((b) => b.classList.remove("active"))
      btn.classList.add("active")
      tabContent.innerHTML = ""
      if (typeof t.render === "function") {
        const result = t.render()
        if (result instanceof Node) tabContent.appendChild(result)
      }
    })
    tabBar.appendChild(btn)
  })

  wrap.appendChild(tabBar)
  wrap.appendChild(tabContent)

  // render first tab
  if (tabs.length && typeof tabs[0].render === "function") {
    const result = tabs[0].render()
    if (result instanceof Node) tabContent.appendChild(result)
  }

  return wrap
}

// ---- Message tree ----

function renderMessageTree(parentNode, parentTags, childMsgs, computeResult, pushedForId, parentAction) {
  const tree = document.createElement("div")
  tree.className = "msg-tree"

  const target = parentNode.recipient || parentTags["Target"] || ""
  const arrowSvg = '<svg width="14" height="14" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><path d="M5 12h14m-7-7 7 7-7 7"/></svg>'

  // When parent exists, all nodes (current + children) go in one continuous tree-children
  let childWrap

  if (pushedForId) {
    // Level 1: parent node
    const ancestor = document.createElement("div")
    ancestor.className = "tree-node tree-root"
    ancestor.innerHTML = `<div class="tree-node-header">
      <span class="tree-node-dot root"></span>
      ${parentAction ? `<span class="type-badge-message">${parentAction}</span>` : ""}
      <span class="mono link tree-node-id" onclick="location.hash='#/entity/${pushedForId}'">${pushedForId}</span>
    </div>`
    tree.appendChild(ancestor)

    // Level 2: current message nested under parent
    const level2 = document.createElement("div")
    level2.className = "tree-children"
    let selfHtml = `<div class="tree-child"><div class="tree-connector"></div><div class="tree-node-header">
      <span class="tree-node-dot child" style="background:#4ade80;box-shadow:0 0 0 3px #1a3a2a"></span>
      <span class="type-badge-message">${parentTags["Action"] || "Message"}</span>
      <span class="mono tree-node-id">${parentNode.id}</span>`
    if (target) {
      selfHtml += `<span class="tree-node-arrow">${arrowSvg}</span>
      <span class="mono link" onclick="location.hash='#/entity/${target}'">${target}</span>`
    }
    selfHtml += `</div></div>`
    level2.innerHTML = selfHtml
    tree.appendChild(level2)

    // Level 3: children of the current message, nested one level deeper
    childWrap = document.createElement("div")
    childWrap.className = "tree-children"
    childWrap.style.marginLeft = "24px"
    level2.appendChild(childWrap)
  } else {
    // No parent — this message is the root (self = green)
    const rootItem = document.createElement("div")
    rootItem.className = "tree-node tree-root"
    let rootHtml = `<div class="tree-node-header">
      <span class="tree-node-dot root" style="background:#4ade80;box-shadow:0 0 0 3px #1a3a2a"></span>
      <span class="type-badge-message">${parentTags["Action"] || "Message"}</span>
      <span class="mono tree-node-id">${parentNode.id}</span>`
    if (target) {
      rootHtml += `<span class="tree-node-arrow">${arrowSvg}</span>
      <span class="mono link" onclick="location.hash='#/entity/${target}'">${target}</span>`
    }
    rootHtml += `</div>`
    rootItem.innerHTML = rootHtml
    tree.appendChild(rootItem)

    // Children container
    childWrap = document.createElement("div")
    childWrap.className = "tree-children"
  }

  // Show compute result replies first, then recorded GraphQL children (deduplicated).
  const gqlChildren = childMsgs || []
  const resultMsgs = computeResult?.Messages || []

  // Track reply targets+actions to dedup GraphQL children
  const replyKeys = new Set()

  // 1. Compute result Messages — always show (replies/unrecorded)
  for (const m of resultMsgs) {
    const mt = tagsMap(m.Tags || m.tags)
    const t = m.Target || m.target || ""
    const action = mt["Action"] || "Message"
    replyKeys.add(`${action}:${t}`)
    const child = document.createElement("div")
    child.className = "tree-node tree-child"
    child.innerHTML = `
      <div class="tree-connector"></div>
      <div class="tree-node-header">
        <span class="tree-node-dot child"></span>
        <span class="type-badge-message">${action}</span>
        <span class="tree-note">(reply)</span>
        ${t ? `<span class="tree-node-arrow">${arrowSvg}</span>
        <span class="mono link" onclick="event.stopPropagation();location.hash='#/entity/${t}'">${t}</span>` : ""}
      </div>
    `
    childWrap.appendChild(child)
  }

  // 2. GraphQL children (Pushed-For) — skip if already shown as reply
  for (const edge of gqlChildren) {
    const mt = tagsMap(edge.node.tags)
    const action = mt["Action"] || "Message"
    const recipient = edge.node.recipient || ""
    if (replyKeys.has(`${action}:${recipient}`)) continue
    const child = document.createElement("div")
    child.className = "tree-node tree-child"
    child.innerHTML = `
      <div class="tree-connector"></div>
      <div class="tree-node-header">
        <span class="tree-node-dot child"></span>
        <span class="type-badge-message">${action}</span>
        <span class="mono link tree-node-id" onclick="event.stopPropagation();location.hash='#/entity/${edge.node.id}'">${edge.node.id}</span>
        ${recipient ? `<span class="tree-node-arrow">${arrowSvg}</span>
        <span class="mono link" onclick="event.stopPropagation();location.hash='#/entity/${recipient}'">${recipient}</span>` : ""}
      </div>
    `
    childWrap.appendChild(child)
  }

  // Spawns
  for (const s of computeResult?.Spawns || []) {
    const st = tagsMap(s.Tags || s.tags)
    const child = document.createElement("div")
    child.className = "tree-node tree-child"
    child.innerHTML = `
      <div class="tree-connector"></div>
      <div class="tree-node-header">
        <span class="tree-node-dot spawn"></span>
        <span class="type-badge-process">Spawn</span>
        <span class="tree-node-name">${st["Name"] || "Process"}</span>
      </div>
    `
    childWrap.appendChild(child)
  }

  if (!childWrap.children.length) {
    const leaf = document.createElement("div")
    leaf.className = "tree-leaf"
    leaf.textContent = "No child messages"
    childWrap.appendChild(leaf)
  }

  // For no-parent case, childWrap needs to be appended to the tree
  // For parent case, childWrap is already nested inside currentNode
  if (!pushedForId) {
    tree.appendChild(childWrap)
  }
  return tree
}

// ---- Process ----

async function renderProcess(el, node, tags) {
  el.innerHTML = ""
  const name = tags["Name"] || null
  const header = detailHeader("Process", node.id, name)
  const fields = detailFields([
    ["Owner", link(node.owner?.address, node.owner?.address)],
    ["Module", tags["Module"] ? link(tags["Module"], tags["Module"]) : "\u2014"],
    ["Name", tags["Name"] || "\u2014"],
    ["Scheduler", tags["Scheduler"] || "\u2014"],
    ["Block", node.block?.height ?? "\u2014"],
    ["Timestamp", formatDate(node.block?.timestamp)],
  ])
  el.appendChild(twoCol(header, fields, section("Tags", renderTags(node.tags))))

  // Source (Eval Lua script)
  if (parseInt(node.data?.size || "0") > 0) {
    const srcSection = document.createElement("div")
    srcSection.className = "section"
    srcSection.innerHTML = '<div class="section-title">Source</div>'
    el.appendChild(srcSection)
    renderDataViewer(srcSection, node.id, null, node)
  }

  // Fetch incoming and outgoing messages for this process
  const [incoming, outgoing] = await Promise.all([
    queryTxs({ first: 50, recipients: [node.id], tags: [{ name: "Type", values: ["Message"] }], sort: "HEIGHT_DESC" }).catch(() => null),
    queryTxs({ first: 50, tags: [{ name: "Type", values: ["Message"] }, { name: "From-Process", values: [node.id] }], sort: "HEIGHT_DESC" }).catch(() => null),
  ])

  const inEdges = incoming?.edges || []
  const outEdges = outgoing?.edges || []

  // All messages combined, deduped, sorted descending by block height
  const allIds = new Set()
  const allEdges = []
  for (const e of [...inEdges, ...outEdges]) {
    if (!allIds.has(e.node.id)) {
      allIds.add(e.node.id)
      allEdges.push(e)
    }
  }
  allEdges.sort((a, b) => (b.node.block?.height ?? 0) - (a.node.block?.height ?? 0))

  // Filter eval messages from incoming
  const evalEdges = inEdges.filter((e) => {
    const mt = tagsMap(e.node.tags)
    return mt["Action"] === "Eval"
  })

  if (allEdges.length || evalEdges.length) {
    const tabs = [
      { label: "All Messages", count: allEdges.length, render: () => msgTable(allEdges) },
      { label: "Incoming", count: inEdges.length, render: () => msgTable(inEdges) },
      { label: "Outgoing", count: outEdges.length, render: () => msgTable(outEdges) },
    ]
    if (evalEdges.length) {
      tabs.push({
        label: "Eval",
        count: evalEdges.length,
        render: () => renderEvalList(evalEdges),
      })
    }
    el.appendChild(createTabs(tabs))
  }
}

function renderEvalList(edges) {
  const wrap = document.createElement("div")
  for (const e of edges) {
    const item = document.createElement("div")
    item.className = "eval-item"
    const header = document.createElement("div")
    header.className = "eval-header"
    header.innerHTML = `
      <span class="mono link" onclick="location.hash='#/entity/${e.node.id}'">${short(e.node.id)}</span>
      <span class="eval-from">from ${short(e.node.owner?.address || "")}</span>
      <span class="eval-time">${fromNow(e.node.block?.timestamp)}</span>
    `
    item.appendChild(header)

    const codeWrap = document.createElement("div")
    codeWrap.innerHTML = '<pre class="code-block-body" style="margin-top:8px"><code>Loading...</code></pre>'
    item.appendChild(codeWrap)

    // Load the Lua source async with highlighting + copy btn on header
    getTxData(e.node.id).then((data) => {
      const src = data?.text || "-- (empty)"
      codeWrap.innerHTML = ""
      codeWrap.appendChild(buildCodeBody(src))
      header.appendChild(makeCopyBtn(src))
    }).catch(() => {
      codeWrap.innerHTML = '<pre class="code-block-body" style="margin-top:8px"><code>-- (failed to load)</code></pre>'
    })

    wrap.appendChild(item)
  }
  return wrap
}

// ---- Compute result with caching ----

function cacheKey(mid) { return `cu:${mid}` }

function getCachedResult(mid) {
  try {
    const raw = sessionStorage.getItem(cacheKey(mid))
    return raw ? JSON.parse(raw) : null
  } catch { return null }
}

function setCachedResult(mid, result) {
  try { sessionStorage.setItem(cacheKey(mid), JSON.stringify(result)) } catch {}
}

const ANSI_COLORS = {
  "30": "#666", "31": "#e06c75", "32": "#98c379", "33": "#e5c07b",
  "34": "#61afef", "35": "#c678dd", "36": "#56b6c2", "37": "#abb2bf",
  "90": "#5c6370", "91": "#e06c75", "92": "#98c379", "93": "#e5c07b",
  "94": "#61afef", "95": "#c678dd", "96": "#56b6c2", "97": "#ffffff",
}

function ansiToHtml(text) {
  const esc = (s) => s.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;")
  let html = ""
  let last = 0
  // eslint-disable-next-line no-control-regex
  const re = /\x1b\[([0-9;]*)m/g
  let m
  while ((m = re.exec(text)) !== null) {
    html += esc(text.slice(last, m.index))
    last = m.index + m[0].length
    const codes = m[1]
    if (!codes || codes === "0") {
      html += "</span>"
    } else {
      for (const c of codes.split(";")) {
        if (ANSI_COLORS[c]) html += `<span style="color:${ANSI_COLORS[c]}">`
        else if (c === "1") html += '<span style="font-weight:bold">'
        else if (c === "2") html += '<span style="opacity:0.7">'
        else if (c === "3") html += '<span style="font-style:italic">'
        else if (c === "4") html += '<span style="text-decoration:underline">'
      }
    }
  }
  html += esc(text.slice(last))
  return html
}

function renderAnsiText(text) {
  const div = document.createElement("div")
  // Check if text contains ANSI escape codes
  // eslint-disable-next-line no-control-regex
  if (/\x1b\[/.test(text)) {
    div.innerHTML = ansiToHtml(text)
  } else {
    div.textContent = text
  }
  return div
}

function renderComputeOutput(container, result) {
  if (!result || (!result.Output?.data && !result.Messages?.length && !result.Spawns?.length)) {
    const empty = document.createElement("div")
    empty.className = "loading"
    empty.style.padding = "16px"
    empty.textContent = "No compute result available"
    container.appendChild(empty)
    return
  }

  if (result.Output?.data) {
    const label = document.createElement("div")
    label.className = "result-label"
    label.textContent = "Output"
    container.appendChild(label)
    const out = document.createElement("div")
    out.className = "result-output"
    const raw = typeof result.Output.data === "string"
      ? result.Output.data
      : JSON.stringify(result.Output.data, null, 2)
    const rendered = renderAnsiText(raw)
    out.appendChild(rendered)
    container.appendChild(out)
  }

  if (result.Messages?.length) {
    const label = document.createElement("div")
    label.className = "section-title"
    label.textContent = `Spawned Messages (${result.Messages.length})`
    label.style.marginTop = "16px"
    container.appendChild(label)
    const rows = result.Messages.map((m, i) => {
      const mt = tagsMap(m.Tags || m.tags)
      return {
        idx: i + 1,
        action: mt["Action"] || "\u2014",
        target: m.Target || m.target || "\u2014",
        _target: m.Target || m.target || "",
      }
    })
    container.appendChild(table(
      [
        { key: "idx", label: "#" },
        { key: "action", label: "Action" },
        { key: "target", label: "Target", mono: true, render: (v, r) => r._target ? `<span class="link" onclick="event.stopPropagation();location.hash='#/entity/${r._target}'">${short(v)}</span>` : v },
      ],
      rows,
    ))
  }

  if (result.Spawns?.length) {
    const label = document.createElement("div")
    label.className = "section-title"
    label.textContent = `Spawns (${result.Spawns.length})`
    label.style.marginTop = "16px"
    container.appendChild(label)
  }
}

function highlightLua(code) {
  const keywords = new Set(["and","break","do","else","elseif","end","false","for","function","if","in","local","nil","not","or","repeat","return","then","true","until","while"])
  const builtins = new Set(["print","tostring","tonumber","type","pairs","ipairs","require","error","pcall","xpcall","assert","select","unpack","table","string","math","io","os","ao","Handlers","Send","Spawn","msg"])
  // Escape HTML
  const esc = s => s.replace(/&/g,"&amp;").replace(/</g,"&lt;").replace(/>/g,"&gt;")
  // Tokenize and highlight
  return code.replace(/--\[\[[\s\S]*?\]\]|--[^\n]*|"[^"\\]*(?:\\.[^"\\]*)*"|'[^'\\]*(?:\\.[^'\\]*)*'|\[\[[\s\S]*?\]\]|\b\d+\.?\d*\b|[A-Za-z_]\w*(?:\.[A-Za-z_]\w*)*|[^\s]/g, token => {
    if (token.startsWith("--")) return `<span class="lua-comment">${esc(token)}</span>`
    if (token.startsWith('"') || token.startsWith("'") || token.startsWith("[[")) return `<span class="lua-string">${esc(token)}</span>`
    if (/^\d/.test(token)) return `<span class="lua-number">${esc(token)}</span>`
    if (/^[A-Za-z_]/.test(token)) {
      // Handle dotted names like ao.send, Handlers.add
      const parts = token.split(".")
      return parts.map(p => {
        if (keywords.has(p)) return `<span class="lua-keyword">${esc(p)}</span>`
        if (builtins.has(p)) return `<span class="lua-builtin">${esc(p)}</span>`
        return esc(p)
      }).join(`<span class="lua-punct">.</span>`)
    }
    return esc(token)
  })
}

const clipSvg = '<svg width="14" height="14" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><rect x="9" y="9" width="13" height="13" rx="2"/><path d="M5 15H4a2 2 0 0 1-2-2V4a2 2 0 0 1 2-2h9a2 2 0 0 1 2 2v1"/></svg>'
const checkSvg = '<svg width="14" height="14" viewBox="0 0 24 24" fill="none" stroke="#4ade80" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M20 6 9 17l-5-5"/></svg>'

function makeCopyBtn(text) {
  const btn = document.createElement("button")
  btn.className = "code-copy-btn"
  btn.title = "Copy to clipboard"
  btn.innerHTML = clipSvg
  btn.onclick = () => {
    navigator.clipboard.writeText(text).then(() => {
      btn.innerHTML = checkSvg
      setTimeout(() => { btn.innerHTML = clipSvg }, 1500)
    })
  }
  return btn
}

function buildCodeBody(src) {
  const wrap = document.createElement("div")
  wrap.className = "code-block-body"
  const lines = src.split("\n")
  const gutter = document.createElement("pre")
  gutter.className = "code-gutter"
  gutter.textContent = lines.map((_, i) => i + 1).join("\n")
  const code = document.createElement("pre")
  code.className = "code-content"
  code.innerHTML = highlightLua(src)
  wrap.appendChild(gutter)
  wrap.appendChild(code)
  return wrap
}

// ---- Simple markdown renderer ----

function renderMarkdown(text) {
  const esc = s => s.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;")
  const lines = text.split("\n")
  let html = ""
  let inCode = false
  let inList = false
  let listType = null

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i]

    // Fenced code blocks
    if (line.trimStart().startsWith("```")) {
      if (inCode) {
        html += "</code></pre>"
        inCode = false
      } else {
        if (inList) { html += listType === "ul" ? "</ul>" : "</ol>"; inList = false }
        html += '<pre class="data-md-code"><code>'
        inCode = true
      }
      continue
    }
    if (inCode) {
      html += esc(line) + "\n"
      continue
    }

    // Close list if line is not a list item
    if (inList && !/^\s*[-*]\s|^\s*\d+\.\s/.test(line) && line.trim() !== "") {
      html += listType === "ul" ? "</ul>" : "</ol>"
      inList = false
    }

    // Blank line
    if (line.trim() === "") {
      if (inList) { html += listType === "ul" ? "</ul>" : "</ol>"; inList = false }
      continue
    }

    // Headings
    const hm = line.match(/^(#{1,6})\s+(.*)/)
    if (hm) {
      const level = hm[1].length
      html += `<h${level}>${inlineFormat(esc(hm[2]))}</h${level}>`
      continue
    }

    // Unordered list
    if (/^\s*[-*]\s/.test(line)) {
      if (!inList || listType !== "ul") {
        if (inList) html += listType === "ul" ? "</ul>" : "</ol>"
        html += "<ul>"
        inList = true
        listType = "ul"
      }
      html += `<li>${inlineFormat(esc(line.replace(/^\s*[-*]\s/, "")))}</li>`
      continue
    }

    // Ordered list
    if (/^\s*\d+\.\s/.test(line)) {
      if (!inList || listType !== "ol") {
        if (inList) html += listType === "ul" ? "</ul>" : "</ol>"
        html += "<ol>"
        inList = true
        listType = "ol"
      }
      html += `<li>${inlineFormat(esc(line.replace(/^\s*\d+\.\s/, "")))}</li>`
      continue
    }

    // Paragraph
    html += `<p>${inlineFormat(esc(line))}</p>`
  }

  if (inCode) html += "</code></pre>"
  if (inList) html += listType === "ul" ? "</ul>" : "</ol>"
  return html
}

function inlineFormat(text) {
  return text
    .replace(/`([^`]+)`/g, "<code>$1</code>")
    .replace(/\*\*([^*]+)\*\*/g, "<strong>$1</strong>")
    .replace(/\*([^*]+)\*/g, "<em>$1</em>")
    .replace(/\[([^\]]+)\]\(([^)]+)\)/g, '<a href="$2" target="_blank" rel="noopener">$1</a>')
}

// ---- Hex dump renderer ----

function renderHexDump(bytes) {
  const view = new Uint8Array(bytes)
  const limit = Math.min(view.length, 256)
  let html = ""
  for (let i = 0; i < limit; i += 16) {
    const addr = i.toString(16).padStart(8, "0")
    let hex = ""
    let ascii = ""
    for (let j = 0; j < 16; j++) {
      if (i + j < limit) {
        const b = view[i + j]
        hex += b.toString(16).padStart(2, "0") + " "
        ascii += b >= 0x20 && b <= 0x7e ? String.fromCharCode(b) : "."
      } else {
        hex += "   "
        ascii += " "
      }
      if (j === 7) hex += " "
    }
    html += `<div class="hex-row"><span class="hex-addr">${addr}</span><span class="hex-bytes">${hex}</span><span class="hex-ascii">${ascii}</span></div>`
  }
  if (view.length > limit) {
    html += `<div class="hex-row hex-truncated">... ${view.length - limit} more bytes</div>`
  }
  return html
}

// ---- JSON collapsible viewer ----

const JSON_MAX_DEPTH = 20
const JSON_MAX_KEYS = 200

function renderJsonTree(obj, depth = 0) {
  if (depth > JSON_MAX_DEPTH) return '<span class="json-null">...</span>'
  if (obj === null) return '<span class="json-null">null</span>'
  if (typeof obj === "boolean") return `<span class="json-bool">${obj}</span>`
  if (typeof obj === "number") return `<span class="json-number">${obj}</span>`
  if (typeof obj === "string") {
    const display = obj.length > 10000 ? obj.slice(0, 10000) + "..." : obj
    const esc = display.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;").replace(/"/g, "&quot;")
    return `<span class="json-string">"${esc}"</span>`
  }
  if (Array.isArray(obj)) {
    if (obj.length === 0) return "[]"
    const limited = obj.slice(0, JSON_MAX_KEYS)
    const items = limited.map(v => `<div class="json-indent">${renderJsonTree(v, depth + 1)},</div>`).join("")
    const truncated = obj.length > JSON_MAX_KEYS ? `<div class="json-indent"><span class="json-null">... ${obj.length - JSON_MAX_KEYS} more items</span></div>` : ""
    return `[${items}${truncated}]`
  }
  if (typeof obj === "object") {
    const keys = Object.keys(obj)
    if (keys.length === 0) return "{}"
    const limited = keys.slice(0, JSON_MAX_KEYS)
    const items = limited.map(k => {
      const esc = k.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;").replace(/"/g, "&quot;")
      return `<div class="json-indent"><span class="json-key">"${esc}"</span>: ${renderJsonTree(obj[k], depth + 1)},</div>`
    }).join("")
    const truncated = keys.length > JSON_MAX_KEYS ? `<div class="json-indent"><span class="json-null">... ${keys.length - JSON_MAX_KEYS} more keys</span></div>` : ""
    return `{${items}${truncated}}`
  }
  return String(obj)
}

// ---- Data viewer header ----

function dataViewerHeader(contentType, size, blobUrl, filename, text) {
  const header = document.createElement("div")
  header.className = "data-viewer-header"
  const badge = document.createElement("span")
  badge.className = "data-ct-badge"
  badge.textContent = contentType
  header.appendChild(badge)

  const sizeEl = document.createElement("span")
  sizeEl.className = "data-size"
  sizeEl.textContent = size < 1024 ? `${size} B` : size < 1048576 ? `${(size / 1024).toFixed(1)} KB` : `${(size / 1048576).toFixed(1)} MB`
  header.appendChild(sizeEl)

  const spacer = document.createElement("span")
  spacer.style.flex = "1"
  header.appendChild(spacer)

  if (text !== null) {
    header.appendChild(makeCopyBtn(text))
  }

  const dlBtn = document.createElement("a")
  dlBtn.className = "data-download-btn"
  dlBtn.href = blobUrl
  dlBtn.download = filename
  dlBtn.innerHTML = '<svg width="14" height="14" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><path d="M21 15v4a2 2 0 01-2 2H5a2 2 0 01-2-2v-4"/><polyline points="7 10 12 15 17 10"/><line x1="12" y1="15" x2="12" y2="3"/></svg> Download'
  header.appendChild(dlBtn)

  return header
}

// ---- Rich data viewer (replaces autoLoadData) ----

async function renderDataViewer(container, txId, action, node) {
  const loading = document.createElement("div")
  loading.className = "loading"
  loading.style.padding = "12px"
  loading.textContent = "Loading data..."
  container.appendChild(loading)
  try {
    const data = await getTxData(txId)
    loading.remove()
    if (!data) {
      const empty = document.createElement("div")
      empty.className = "data-preview"
      empty.textContent = "No data"
      container.appendChild(empty)
      return
    }

    const { contentType, blob, text, size } = data

    // Handle files too large for in-browser rendering
    if (data.tooLarge) {
      const info = document.createElement("div")
      info.className = "data-viewer"
      const sizeStr = size < 1048576 ? (size / 1024).toFixed(1) + " KB" : (size / 1048576).toFixed(1) + " MB"
      info.innerHTML = `<div class="data-viewer-header"><span class="data-ct-badge">${contentType}</span><span class="data-size">${sizeStr}</span><span style="flex:1"></span><a class="data-download-btn" href="/ar/${txId}" download="${txId}"><svg width="14" height="14" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><path d="M21 15v4a2 2 0 01-2 2H5a2 2 0 01-2-2v-4"/><polyline points="7 10 12 15 17 10"/><line x1="12" y1="15" x2="12" y2="3"/></svg> Download</a></div><div class="data-preview">File too large for in-browser preview (${sizeStr}). Use the download button.</div>`
      container.appendChild(info)
      return
    }

    const blobUrl = URL.createObjectURL(blob)
    trackBlobUrl(blobUrl)
    const viewer = document.createElement("div")
    viewer.className = "data-viewer"

    // Header with badge, size, buttons
    viewer.appendChild(dataViewerHeader(contentType, size, blobUrl, txId, text))

    // Dispatch on content type
    if (action === "Eval" || contentType === "text/x-lua") {
      // Lua code with syntax highlighting
      viewer.appendChild(buildCodeBody(text || "-- (empty)"))
    } else if (contentType.startsWith("image/svg+xml") && text) {
      // SVG: render + toggle to code (sanitize scripts)
      const btnGroup = document.createElement("div")
      btnGroup.className = "data-view-tabs"
      const btnPreview = document.createElement("button")
      btnPreview.className = "data-view-tab active"
      btnPreview.textContent = "Preview"
      const btnSource = document.createElement("button")
      btnSource.className = "data-view-tab"
      btnSource.textContent = "Source"
      btnGroup.appendChild(btnPreview)
      btnGroup.appendChild(btnSource)
      viewer.firstChild.insertBefore(btnGroup, viewer.firstChild.querySelector(".data-size").nextSibling)

      const svgContainer = document.createElement("div")
      svgContainer.className = "data-svg-wrap"
      const svgRender = document.createElement("div")
      svgRender.className = "data-svg-render"
      const sanitizedSvg = text.replace(/<script[\s\S]*?<\/script>/gi, "").replace(/\bon\w+\s*=/gi, "data-removed=")
      svgRender.innerHTML = sanitizedSvg
      const codeView = document.createElement("div")
      codeView.className = "data-svg-code"
      codeView.style.display = "none"
      codeView.appendChild(buildGenericCodeBody(text))
      btnPreview.addEventListener("click", () => {
        svgRender.style.display = ""
        codeView.style.display = "none"
        btnPreview.classList.add("active")
        btnSource.classList.remove("active")
      })
      btnSource.addEventListener("click", () => {
        svgRender.style.display = "none"
        codeView.style.display = ""
        btnSource.classList.add("active")
        btnPreview.classList.remove("active")
      })
      svgContainer.appendChild(svgRender)
      svgContainer.appendChild(codeView)
      viewer.appendChild(svgContainer)
    } else if (contentType.startsWith("image/")) {
      const box = document.createElement("div")
      box.className = "data-media-box"
      const img = document.createElement("img")
      img.className = "data-img"
      img.src = blobUrl
      img.alt = "Transaction data"
      box.appendChild(img)
      viewer.appendChild(box)
    } else if (contentType.startsWith("video/")) {
      const box = document.createElement("div")
      box.className = "data-video-box"
      const video = document.createElement("video")
      video.className = "data-video"
      video.src = blobUrl
      video.controls = true
      box.appendChild(video)
      viewer.appendChild(box)
    } else if (contentType.startsWith("audio/")) {
      const audio = document.createElement("audio")
      audio.src = blobUrl
      audio.controls = true
      audio.style.width = "100%"
      viewer.appendChild(audio)
    } else if (contentType === "text/markdown" && text) {
      // Add view mode buttons to header
      const btnGroup = document.createElement("div")
      btnGroup.className = "data-view-tabs"
      const btnPreview = document.createElement("button")
      btnPreview.className = "data-view-tab active"
      btnPreview.textContent = "Preview"
      const btnSource = document.createElement("button")
      btnSource.className = "data-view-tab"
      btnSource.textContent = "Source"
      btnGroup.appendChild(btnPreview)
      btnGroup.appendChild(btnSource)
      viewer.firstChild.insertBefore(btnGroup, viewer.firstChild.querySelector(".data-size").nextSibling)

      const mdWrap = document.createElement("div")
      mdWrap.className = "data-md-wrap"
      const mdPreview = document.createElement("div")
      mdPreview.className = "data-markdown"
      mdPreview.innerHTML = renderMarkdown(text)
      const mdCode = document.createElement("div")
      mdCode.style.display = "none"
      mdCode.appendChild(buildGenericCodeBody(text))
      btnPreview.addEventListener("click", () => {
        mdPreview.style.display = ""
        mdCode.style.display = "none"
        btnPreview.classList.add("active")
        btnSource.classList.remove("active")
      })
      btnSource.addEventListener("click", () => {
        mdPreview.style.display = "none"
        mdCode.style.display = ""
        btnSource.classList.add("active")
        btnPreview.classList.remove("active")
      })
      mdWrap.appendChild(mdPreview)
      mdWrap.appendChild(mdCode)
      viewer.appendChild(mdWrap)
    } else if (contentType === "text/html" && text) {
      const iframe = document.createElement("iframe")
      iframe.className = "data-html-frame"
      iframe.sandbox = "allow-scripts"
      iframe.srcdoc = text
      viewer.appendChild(iframe)
    } else if (contentType.includes("json") && text) {
      try {
        const obj = JSON.parse(text)
        const pre = document.createElement("pre")
        pre.className = "data-json"
        pre.innerHTML = renderJsonTree(obj)
        viewer.appendChild(pre)
      } catch {
        viewer.appendChild(buildGenericCodeBody(text))
      }
    } else if (
      contentType.includes("javascript") || contentType.includes("typescript") ||
      contentType.includes("ecmascript") || contentType.includes("css") ||
      contentType.includes("x-python") || contentType.includes("python") ||
      contentType.includes("x-toml") || contentType.includes("x-yaml") ||
      contentType.includes("x-sh") || contentType.includes("shell") ||
      contentType.includes("x-rust") || contentType.includes("x-go") ||
      contentType.includes("x-c") || contentType.includes("x-java")
    ) {
      viewer.appendChild(buildGenericCodeBody(text || "", contentType))
    } else if (text !== null) {
      // Fallback: plain text with ANSI support
      const pre = document.createElement("div")
      pre.className = "data-preview"
      pre.appendChild(renderAnsiText(text))
      viewer.appendChild(pre)
    } else {
      // Binary fallback — just show info, no hex dump
      const info = document.createElement("div")
      info.className = "data-preview"
      info.textContent = `Binary data (${size < 1024 ? size + " bytes" : size < 1048576 ? (size / 1024).toFixed(1) + " KB" : (size / 1048576).toFixed(1) + " MB"})`
      viewer.appendChild(info)
    }

    container.appendChild(viewer)

    // Bundle items: if this tx has Bundle-Format tag, show child items
    if (node) {
      const tags = node.tags || []
      const hasBundleFormat = tags.some(t => t.name === "Bundle-Format")
      if (hasBundleFormat) {
        await renderBundleItems(container, txId)
      }
    }
  } catch {
    loading.remove()
  }
}

// ---- Generic code body with syntax highlighting ----

function highlightGeneric(code, lang) {
  const esc = s => s.replace(/&/g,"&amp;").replace(/</g,"&lt;").replace(/>/g,"&gt;")
  const kw = {
    js: ["const","let","var","function","return","if","else","for","while","do","switch","case","break","continue","new","this","class","extends","import","export","from","default","async","await","try","catch","throw","finally","typeof","instanceof","in","of","yield","delete","void","null","undefined","true","false","super","static","get","set"],
    py: ["def","class","return","if","elif","else","for","while","try","except","finally","with","as","import","from","raise","pass","break","continue","yield","lambda","and","or","not","in","is","True","False","None","global","nonlocal","assert","del","async","await"],
    css: ["@media","@keyframes","@import","@font-face","@charset","@supports","!important"],
    sh: ["if","then","else","elif","fi","for","while","do","done","case","esac","in","function","return","local","export","source","alias","echo","exit","read","set","unset","shift","trap","eval","exec","test"],
    toml: [],
    yaml: ["true","false","null","yes","no","on","off"],
  }
  const keywords = new Set(kw[lang] || kw.js)
  const lineComment = lang === "py" || lang === "sh" || lang === "toml" || lang === "yaml" ? "#" : "//"
  const hasBlock = lang === "js" || lang === "css"

  let result = ""
  let i = 0
  while (i < code.length) {
    // Block comments
    if (hasBlock && code[i] === "/" && code[i+1] === "*") {
      const end = code.indexOf("*/", i + 2)
      const slice = end === -1 ? code.slice(i) : code.slice(i, end + 2)
      result += `<span class="lua-comment">${esc(slice)}</span>`
      i += slice.length
      continue
    }
    // Line comments
    if (code.slice(i, i + lineComment.length) === lineComment) {
      const nl = code.indexOf("\n", i)
      const slice = nl === -1 ? code.slice(i) : code.slice(i, nl)
      result += `<span class="lua-comment">${esc(slice)}</span>`
      i += slice.length
      continue
    }
    // Strings
    if (code[i] === '"' || code[i] === "'" || code[i] === "`") {
      const q = code[i]
      let j = i + 1
      while (j < code.length && code[j] !== q) {
        if (code[j] === "\\") j++
        j++
      }
      if (j < code.length) j++
      result += `<span class="lua-string">${esc(code.slice(i, j))}</span>`
      i = j
      continue
    }
    // Numbers
    if (/[0-9]/.test(code[i]) && (i === 0 || /[\s,;:({[=+\-*/%<>!&|^~?]/.test(code[i-1]))) {
      let j = i
      if (code[j] === "0" && (code[j+1] === "x" || code[j+1] === "X")) j += 2
      while (j < code.length && /[0-9a-fA-F._]/.test(code[j])) j++
      result += `<span class="lua-number">${esc(code.slice(i, j))}</span>`
      i = j
      continue
    }
    // Identifiers / keywords
    if (/[A-Za-z_$]/.test(code[i])) {
      let j = i
      while (j < code.length && /[A-Za-z0-9_$]/.test(code[j])) j++
      const word = code.slice(i, j)
      if (keywords.has(word)) {
        result += `<span class="lua-keyword">${esc(word)}</span>`
      } else {
        result += esc(word)
      }
      i = j
      continue
    }
    result += esc(code[i])
    i++
  }
  return result
}

function detectLang(contentType) {
  if (contentType.includes("javascript") || contentType.includes("ecmascript")) return "js"
  if (contentType.includes("typescript")) return "js"
  if (contentType.includes("python")) return "py"
  if (contentType.includes("css")) return "css"
  if (contentType.includes("x-sh") || contentType.includes("shell") || contentType.includes("bash")) return "sh"
  if (contentType.includes("toml")) return "toml"
  if (contentType.includes("yaml") || contentType.includes("yml")) return "yaml"
  if (contentType.includes("lua")) return "lua"
  return "js" // default fallback
}

function buildGenericCodeBody(src, contentType) {
  const wrap = document.createElement("div")
  wrap.className = "code-block-body"
  const lines = src.split("\n")
  const gutter = document.createElement("pre")
  gutter.className = "code-gutter"
  gutter.textContent = lines.map((_, i) => i + 1).join("\n")
  const code = document.createElement("pre")
  code.className = "code-content"
  if (contentType) {
    const lang = detectLang(contentType)
    if (lang === "lua") {
      code.innerHTML = highlightLua(src)
    } else {
      code.innerHTML = highlightGeneric(src, lang)
    }
  } else {
    code.textContent = src
  }
  wrap.appendChild(gutter)
  wrap.appendChild(code)
  return wrap
}

// ---- Bundle items UI ----

async function renderBundleItems(container, bundleId) {
  try {
    const items = await queryTxs({
      first: 50,
      bundledIn: [bundleId],
    }).catch(() => null)

    // Also try bundledIn filter via owner query (items reference parent)
    let edges = items?.edges || []

    // If that didn't find anything, try a broader approach
    if (edges.length === 0) return

    const sec = document.createElement("div")
    sec.className = "section bundle-items"
    sec.innerHTML = `<div class="section-title">Bundle Items (${edges.length})</div>`

    const rows = edges.map(e => {
      const mt = tagsMap(e.node.tags)
      return {
        type: mt["Type"] || "Data",
        name: mt["Name"] || "\u2014",
        id: e.node.id,
        owner: e.node.owner?.address || "\u2014",
        size: e.node.data?.size || "\u2014",
        _id: e.node.id,
        _owner: e.node.owner?.address || "",
      }
    })

    sec.appendChild(table(
      [
        { key: "type", label: "Type", render: v => typeBadgeHtml(v) },
        { key: "name", label: "Name" },
        { key: "id", label: "ID", mono: true, render: v => `<span class="link">${short(v)}</span>` },
        { key: "owner", label: "Owner", mono: true, render: (v, r) => r._owner ? `<span class="link" onclick="event.stopPropagation();location.hash='#/entity/${r._owner}'">${short(v)}</span>` : v },
        { key: "size", label: "Data Size" },
      ],
      rows,
      row => navigate("#/entity/" + row._id),
    ))

    container.appendChild(sec)
  } catch {
    // ok
  }
}

async function loadComputeResult(container, mid, pid) {
  // Check cache first
  const cached = getCachedResult(mid)
  if (cached) {
    renderComputeOutput(container, cached)
    return cached
  }

  // Auto-load
  const loading = document.createElement("div")
  loading.className = "loading"
  loading.style.padding = "12px"
  loading.textContent = "Loading compute result..."
  container.appendChild(loading)

  try {
    const result = await getResult(mid, pid)
    loading.remove()
    setCachedResult(mid, result)
    renderComputeOutput(container, result)
    return result
  } catch (err) {
    loading.remove()
    const errDiv = document.createElement("div")
    errDiv.className = "loading"
    errDiv.style.padding = "8px"
    errDiv.textContent = err.message || "Failed to fetch compute result"
    container.appendChild(errDiv)
    return null
  }
}

// ---- Message ----

async function renderMessage(el, node, tags) {
  el.innerHTML = ""
  const action = tags["Action"] || null
  const header = detailHeader("Message", node.id, action)

  const target = node.recipient || tags["Target"]
  const pushedFor = tags["Pushed-For"]
  const fromProcess = tags["From-Process"]
  const fieldRows = [
    ["Action", tags["Action"] || "\u2014"],
    ["From", link(node.owner?.address, node.owner?.address)],
    ["To (Process)", target ? link(target, target) : "\u2014"],
  ]
  if (pushedFor) fieldRows.push(["Parent Message", link(pushedFor, pushedFor)])
  if (fromProcess) fieldRows.push(["From Process", link(fromProcess, fromProcess)])
  fieldRows.push(
    ["Block", node.block?.height ?? "\u2014"],
    ["Timestamp", formatDate(node.block?.timestamp)],
    ["Data Size", node.data?.size ? node.data.size + " bytes" : "\u2014"],
  )
  const fields = detailFields(fieldRows)
  el.appendChild(twoCol(header, fields, section("Tags", renderTags(node.tags))))

  // Data — auto-load
  if (parseInt(node.data?.size || "0") > 0) {
    const dataSection = document.createElement("div")
    dataSection.className = "section"
    dataSection.innerHTML = '<div class="section-title">Data</div>'
    el.appendChild(dataSection)
    renderDataViewer(dataSection, node.id, tags["Action"], node)
  }

  // Compute Result — auto-load with caching
  let computeResult = null
  if (target) {
    const resultSection = document.createElement("div")
    resultSection.className = "section"
    resultSection.innerHTML = '<div class="section-title">Compute Result</div>'
    el.appendChild(resultSection)
    computeResult = await loadComputeResult(resultSection, node.id, target)
  }

  // Message Tree — always show, with parent-child relationships
  // Find child messages + parent action in parallel
  let childMsgs = []
  let parentAction = null
  {
    const promises = []
    promises.push(
      queryTxs({
        first: 50,
        tags: [
          { name: "Type", values: ["Message"] },
          { name: "Pushed-For", values: [node.id] },
        ],
        sort: "HEIGHT_DESC",
      }).then(r => { childMsgs = r?.edges || [] }).catch(() => {})
    )
    if (pushedFor) {
      promises.push(
        queryTxs({ ids: [pushedFor] }).then(r => {
          const pt = tagsMap(r?.edges?.[0]?.node?.tags)
          parentAction = pt["Action"] || null
        }).catch(() => {})
      )
    }
    await Promise.all(promises)
  }

  {
    const treeSection = document.createElement("div")
    treeSection.className = "section"
    treeSection.innerHTML = '<div class="section-title">Message Flow</div>'

    treeSection.appendChild(renderMessageTree(node, tags, childMsgs, computeResult, pushedFor, parentAction))
    el.appendChild(treeSection)
  }

}

// ---- Module ----

async function renderModule(el, node, tags) {
  el.innerHTML = ""
  const name = tags["Name"] || tags["Module-Name"] || null
  const header = detailHeader("Module", node.id, name)
  const fields = detailFields([
    ["Owner", link(node.owner?.address, node.owner?.address)],
    ["Name", tags["Name"] || tags["Module-Name"] || "\u2014"],
    ["Format", tags["Module-Format"] || tags["Content-Type"] || "\u2014"],
    ["Memory Limit", tags["Memory-Limit"] || "\u2014"],
    ["Compute Limit", tags["Compute-Limit"] || "\u2014"],
    ["Input Encoding", tags["Input-Encoding"] || "\u2014"],
    ["Output Encoding", tags["Output-Encoding"] || "\u2014"],
    ["Block", node.block?.height ?? "\u2014"],
    ["Timestamp", formatDate(node.block?.timestamp)],
    ["Data Size", node.data?.size ? node.data.size + " bytes" : "\u2014"],
  ])
  el.appendChild(twoCol(header, fields, section("Tags", renderTags(node.tags))))

  // Processes using this module
  try {
    const procs = await queryTxs({
      first: 50,
      tags: [
        { name: "Type", values: ["Process"] },
        { name: "Module", values: [node.id] },
      ],
      sort: "HEIGHT_DESC",
    })
    const procEdges = procs?.edges || []
    const rows = procEdges.map((e) => {
      const pt = tagsMap(e.node.tags)
      return {
        name: pt["Name"] || "\u2014",
        id: e.node.id,
        owner: e.node.owner?.address,
        time: fromNow(e.node.block?.timestamp),
        _id: e.node.id,
        _owner: e.node.owner?.address || "",
      }
    })
    if (rows.length) {
      el.appendChild(section(
        `Processes (${procEdges.length})`,
        table(
          [
            { key: "name", label: "Name" },
            { key: "id", label: "ID", mono: true, render: (v) => `<span class="link">${short(v)}</span>` },
            { key: "owner", label: "Owner", mono: true, render: (v, r) => `<span class="link" onclick="event.stopPropagation();location.hash='#/entity/${r._owner}'">${short(v)}</span>` },
            { key: "time", label: "Time" },
          ],
          rows,
          (row) => navigate("#/entity/" + row._id),
        ),
      ))
    } else {
      el.appendChild(section("Processes", document.createTextNode("No processes using this module")))
    }
  } catch {
    // ok
  }
}

// ---- Block ----

async function renderBlock(el, height) {
  const blocks = await queryBlocks({ first: 1, min: height, max: height })
  const edge = blocks?.edges?.[0]
  if (!edge) {
    el.innerHTML = `<div class="loading">Block ${height} not found</div>`
    return
  }
  await renderBlockNode(el, edge.node)
}

async function renderBlockNode(el, node) {
  el.innerHTML = ""
  el.appendChild(detailHeader("Block", String(node.height), `Block #${node.height}`))

  el.appendChild(detailFields([
    ["Block ID", node.id],
    ["Height", node.height],
    ["Timestamp", formatDate(node.timestamp)],
    ["Previous", node.previous ? link(node.previous, node.previous) : "\u2014"],
  ]))

  try {
    const txs = await queryTxs({ first: 100, sort: "HEIGHT_DESC" })
    const blockTxs = (txs?.edges || []).filter((e) => e.node.block?.height === node.height && !e.node.bundledIn?.id)
    if (blockTxs.length) {
      const rows = blockTxs.map((e) => {
        const tags = tagsMap(e.node.tags)
        return {
          type: tags["Bundle-Format"] ? "Bundle" : tags["Type"] || contentTypeLabel(e.node.data?.type || tags["Content-Type"]) || "TX",
          id: e.node.id,
          owner: e.node.owner?.address,
          _id: e.node.id,
          _owner: e.node.owner?.address || "",
        }
      })
      el.appendChild(section(
        `Transactions (${blockTxs.length})`,
        table(
          [
            { key: "type", label: "Type", render: (v) => typeBadgeHtml(v) },
            { key: "id", label: "ID", mono: true, render: (v) => `<span class="link">${short(v)}</span>` },
            { key: "owner", label: "Owner", mono: true, render: (v, r) => `<span class="link" onclick="event.stopPropagation();location.hash='#/entity/${r._owner}'">${short(v)}</span>` },
          ],
          rows,
          (row) => navigate("#/entity/" + row._id),
        ),
      ))
    }
  } catch {
    // ok
  }
}

// ---- Address ----

async function renderAddress(el, addr, txs) {
  el.innerHTML = ""
  el.appendChild(detailHeader("Address", addr))

  const rows = txs.edges.map((e) => {
    const tags = tagsMap(e.node.tags)
    return {
      type: tags["Type"] || contentTypeLabel(e.node.data?.type || tags["Content-Type"]) || "TX",
      id: e.node.id,
      recipient: e.node.recipient || "\u2014",
      time: fromNow(e.node.block?.timestamp),
      _id: e.node.id,
      _recipient: e.node.recipient || "",
    }
  })
  el.appendChild(section(
    `Transactions (${txs.edges.length})`,
    table(
      [
        { key: "type", label: "Type", render: (v) => typeBadgeHtml(v) },
        { key: "id", label: "ID", mono: true, render: (v) => `<span class="link">${short(v)}</span>` },
        { key: "recipient", label: "Recipient", mono: true, render: (v, r) => r._recipient ? `<span class="link" onclick="event.stopPropagation();location.hash='#/entity/${r._recipient}'">${short(v)}</span>` : v },
        { key: "time", label: "Timestamp" },
      ],
      rows,
      (row) => navigate("#/entity/" + row._id),
    ),
  ))

  // Also show messages sent by this address
  try {
    const msgs = await queryTxs({
      first: 50,
      owners: [addr],
      tags: [{ name: "Type", values: ["Message"] }],
      sort: "HEIGHT_DESC",
    })
    const msgEdges = msgs?.edges || []
    if (msgEdges.length) {
      el.appendChild(section(
        `Messages Sent (${msgEdges.length})`,
        msgTable(msgEdges),
      ))
    }
  } catch {
    // ok
  }
}

// ---- Generic ----

async function renderGeneric(el, node, tags, type) {
  el.innerHTML = ""
  const header = detailHeader(type, node.id)
  const fields = detailFields([
    ["Owner", link(node.owner?.address, node.owner?.address)],
    ["Recipient", node.recipient ? link(node.recipient, node.recipient) : "\u2014"],
    ["Block", node.block?.height ?? "\u2014"],
    ["Timestamp", formatDate(node.block?.timestamp)],
    ["Data Size", node.data?.size ? node.data.size + " bytes" : "\u2014"],
  ])
  el.appendChild(twoCol(header, fields, section("Tags", renderTags(node.tags))))

  // For bundles, show item list instead of raw binary
  if (tags["Bundle-Format"]) {
    const bundleSection = document.createElement("div")
    bundleSection.className = "section"
    bundleSection.innerHTML = '<div class="section-title">Bundle Items</div>'
    el.appendChild(bundleSection)
    await renderBundleItems(bundleSection, node.id)
  } else {
    const dataSection = document.createElement("div")
    dataSection.className = "section"
    dataSection.innerHTML = '<div class="section-title">Data</div>'
    el.appendChild(dataSection)
    renderDataViewer(dataSection, node.id, null, node)
  }
}
