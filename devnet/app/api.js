async function gql(query, variables = {}) {
  const res = await fetch("/ar/graphql", {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ query, variables }),
  })
  return res.json()
}

export async function getArInfo() {
  const res = await fetch("/ar")
  return res.json()
}

export async function queryBlocks({ first = 10, after, min, max } = {}) {
  const vars = { first }
  if (after) vars.after = after

  let blockFilter = ""
  if (min !== undefined || max !== undefined) {
    const parts = []
    if (min !== undefined) parts.push(`min: ${min}`)
    if (max !== undefined) parts.push(`max: ${max}`)
    blockFilter = `, block: {${parts.join(", ")}}`
  }

  const { data } = await gql(`
    query($first: Int, $after: String) {
      blocks(first: $first, after: $after, sort: HEIGHT_DESC${blockFilter}) {
        pageInfo { hasNextPage }
        edges {
          cursor
          node {
            id
            height
            timestamp
            previous
          }
        }
      }
    }
  `, vars)
  return data.blocks
}

export async function queryTxs({ first = 10, after, ids, owners, recipients, tags, sort, bundledIn } = {}) {
  const vars = { first }
  if (after) vars.after = after
  if (ids) vars.ids = ids
  if (owners) vars.owners = owners
  if (recipients) vars.recipients = recipients
  if (tags) vars.tags = tags
  if (sort) vars.sort = sort
  if (bundledIn) vars.bundledIn = bundledIn

  const { data } = await gql(`
    query($first: Int, $after: String, $ids: [ID!], $owners: [String!], $recipients: [String!], $tags: [TagFilter!], $sort: SortOrder, $bundledIn: [ID!]) {
      transactions(first: $first, after: $after, ids: $ids, owners: $owners, recipients: $recipients, tags: $tags, sort: $sort, bundledIn: $bundledIn) {
        pageInfo { hasNextPage }
        edges {
          cursor
          node {
            id
            owner { address }
            recipient
            block { id height timestamp }
            tags { name value }
            data { size type }
            bundledIn { id }
          }
        }
      }
    }
  `, vars)
  return data.transactions
}

export async function getProcesses() {
  const res = await fetch("/su")
  return res.json()
}

export async function getProcessMsgs(pid) {
  const res = await fetch(`/su/${pid}`)
  return res.json()
}

export async function getResults(pid, { from, limit } = {}) {
  const params = new URLSearchParams()
  if (from) params.set("from", from)
  if (limit) params.set("limit", limit)
  const qs = params.toString()
  const res = await fetch(`/cu/results/${pid}${qs ? "?" + qs : ""}`)
  return res.json()
}

export async function getResult(mid, pid) {
  const res = await fetch(`/cu/result/${mid}?process-id=${pid}`)
  return res.json()
}

const MAX_DATA_SIZE = 50 * 1024 * 1024 // 50 MB

export async function getTxData(id) {
  const res = await fetch(`/ar/${id}`)
  if (!res.ok) return null
  const contentType = res.headers.get("content-type") || "application/octet-stream"

  // Check size before downloading full body
  const cl = parseInt(res.headers.get("content-length") || "0", 10)
  if (cl > MAX_DATA_SIZE) {
    return { contentType, blob: null, text: null, size: cl, tooLarge: true }
  }

  const blob = await res.blob()
  if (blob.size > MAX_DATA_SIZE) {
    return { contentType, blob: null, text: null, size: blob.size, tooLarge: true }
  }

  let text = null
  const textTypes = [
    "text/", "json", "javascript", "xml", "svg", "markdown",
    "x-lua", "css", "x-python", "x-toml", "x-yaml", "x-sh",
  ]
  if (textTypes.some(t => contentType.includes(t))) {
    text = await blob.text()
  }
  return { contentType, blob, text, size: blob.size }
}
