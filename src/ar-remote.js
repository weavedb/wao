export default class RemoteAR {
  constructor(ar_url) {
    this.url = ar_url.replace(/\/$/, "")
  }

  async data(id) {
    const res = await fetch(`${this.url}/ar/${id}`)
    if (!res.ok) return null
    return new Uint8Array(await res.arrayBuffer())
  }

  async getTx(id) {
    const query = `{ transactions(ids: ["${id}"]) {
      edges { node { id owner { address key } recipient tags { name value }
        anchor signature data { size type } block { id height timestamp } } }
    } }`
    const res = await fetch(`${this.url}/ar/graphql`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ query }),
    })
    const json = await res.json()
    const node = json?.data?.transactions?.edges?.[0]?.node
    if (!node) return null
    return {
      id: node.id,
      owner: node.owner?.address,
      _owner_key: node.owner?.key,
      recipient: node.recipient,
      tags: node.tags,
      anchor: node.anchor,
      signature: node.signature,
      _data: node.data,
      block: node.block?.id,
    }
  }

  async postTx(tx) {
    const res = await fetch(`${this.url}/ar/${tx.id || "tx"}`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(tx),
    })
    return await res.json()
  }

  async postMU(rawDataItem) {
    const res = await fetch(`${this.url}/mu`, {
      method: "POST",
      headers: { "Content-Type": "application/octet-stream" },
      body: rawDataItem,
    })
    return await res.json()
  }

  async getHeight() {
    const res = await fetch(`${this.url}/ar/`)
    const json = await res.json()
    return json.height ?? 0
  }

  async getAnchor() {
    const res = await fetch(`${this.url}/ar/tx_anchor`)
    return await res.text()
  }

  async graphql(query, variables) {
    const res = await fetch(`${this.url}/ar/graphql`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ query, variables }),
    })
    return await res.json()
  }

  async findSU(schedulerAddr) {
    const query = `{ transactions(
      owners: ["${schedulerAddr}"],
      tags: [{ name: "Type", values: ["Scheduler-Location"] }],
      first: 1, sort: HEIGHT_DESC
    ) { edges { node { tags { name value } } } } }`
    const json = await this.graphql(query)
    const tags = json?.data?.transactions?.edges?.[0]?.node?.tags ?? []
    const urlTag = tags.find(t => t.name === "Url")
    return urlTag?.value ?? null
  }
}
