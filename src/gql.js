import { includes, map, is, isNil, last, clone } from "ramda"

const full = `id anchor signature recipient owner { address key } fee { winston ar } quantity { winston ar } data { size type } tags { name value } block { id timestamp height previous } parent { id }`
const full_blocks = `id timestamp height previous`

const subs = {
  owner: ["address", "key"],
  fee: ["winston", "ar"],
  quantity: ["winston", "ar"],
  data: ["size", "type"],
  tags: ["name", "value"],
  block: ["id", "timestamp", "height", "previous"],
  parent: ["id"],
}
const field = (key, val = true) => {
  if (includes(key, ["id", "anchor", "signature", "recipient"])) {
    return key
  } else if (subs[key]) {
    let _subs = []
    if (val === true) val = subs[key]
    else if (is(Object, val) && !is(Array, val)) {
      let _val = []
      let isTrue = false
      let isFalse = false
      for (const k in val)
        if (val[k] === true) isTrue = true
        else if (val[k] === false) isFalse = true
      if (!isTrue && isFalse) {
        for (const k of subs[key]) if (val[k] !== false) _val.push(k)
      } else {
        for (const k in val) if (val[k] === true) _val.push(k)
      }
      val = _val
    } else if (!is(Array, val)) val = subs[key]
    for (const v2 of val) {
      if (is(String, v2) && includes(v2, subs[key])) _subs.push(v2)
    }
    if (_subs.length === 0) return null
    return `${key} { ${_subs.join(" ")} }`
  }
  return null
}

const field_blocks = (key, val = true) => {
  if (includes(key, ["id", "timestamp", "height", "previous"])) {
    return key
  }
  return null
}

const query = (opt = {}) => {
  let cond = []
  let tags = []
  for (const k in opt.tags ?? {}) {
    if (is(String, opt.tags[k])) {
      tags.push(`{ name: "${k}", values: ["${opt.tags[k]}"] }`)
    } else if (is(Array, opt.tags[k])) {
      tags.push(
        `{ name: "${k}", values: [${map(v => `"${v}"`, opt.tags[k]).join(", ")}] }`,
      )
    }
  }
  if (tags.length > 0) cond.push(`tags: [${tags.join(", ")}]`)
  let recipients = null
  if (is(Array, opt.recipients)) recipients = opt.recipients
  else if (is(String, opt.recipient)) recipients = [opt.recipient]
  if (!isNil(recipients) && recipients.length > 0) {
    cond.push(`recipients: [${map(v => `"${v}"`, recipients).join(", ")}]`)
  }

  let owners = null
  if (is(Array, opt.owners)) owners = opt.owners
  else if (is(String, opt.owner)) owners = [opt.owner]
  if (!isNil(owners) && owners.length > 0) {
    cond.push(`owners: [${map(v => `"${v}"`, owners).join(", ")}]`)
  }

  let ids = null
  if (is(Array, opt.ids)) ids = opt.ids
  else if (is(String, opt.id)) ids = [opt.id]
  if (!isNil(ids) && ids.length > 0) {
    cond.push(`ids: [${map(v => `"${v}"`, ids).join(", ")}]`)
  }

  let _block = opt.block
  if (is(Number, opt.block)) {
    _block = { min: opt.block, max: opt.block }
  } else if (is(Array, opt.block) && opt.block.length > 0) {
    _block = {}
    if (!isNil(opt.block[0])) _block.min = opt.block[0]
    if (!isNil(opt.block[1])) _block.max = opt.block[1]
  }
  if (!isNil(_block?.max) || !isNil(_block?.min)) {
    let block = []
    if (!isNil(_block.min)) block.push(`min: ${_block.min}`)
    if (!isNil(_block.max)) block.push(`max: ${_block.max}`)
    if (block.length > 0) cond.push(`block : {${block.join(", ")}}`)
  }
  if (opt.first) cond.push(`first: ${opt.first}`)
  if (opt.after) cond.push(`after: "${opt.after}"`)
  if (opt.asc) cond.push(`sort: HEIGHT_ASC`)

  let _cond = ""
  if (cond.length > 0) _cond = `(${cond.join(", ")})`
  let fields = []
  if (is(Array, opt.fields) && opt.fields.length > 0) {
    for (const v of opt.fields) {
      if (is(String, v)) {
        const fld = field(v)
        if (fld) fields.push(fld)
      } else if (is(Object, v) && !is(Array, v)) {
        for (const k in v) {
          const fld = field(k, v[k])
          if (fld) fields.push(fld)
        }
      }
    }
  } else if (is(Object, opt.fields) && !is(Array, opt.fields)) {
    for (const k in opt.fields) {
      const fld = field(k, opt.fields[k])
      if (fld) fields.push(fld)
    }
  }
  let _fields = fields.length > 0 ? fields.join(" ") : full
  return `query {
  transactions ${_cond}{
    edges { cursor node { ${_fields} } }
  }
}`
}

const query_blocks = (opt = {}) => {
  let cond = []

  let _height = opt.height
  if (is(Number, opt.height)) {
    _height = { min: opt.height, max: opt.height }
  } else if (is(Array, opt.height) && opt.height.length > 0) {
    _height = {}
    if (!isNil(opt.height[0])) _height.min = opt.height[0]
    if (!isNil(opt.height[1])) _height.max = opt.height[1]
  }
  if (!isNil(_height?.max) || !isNil(_height?.min)) {
    let height = []
    if (!isNil(_height.min)) height.push(`min: ${_height.min}`)
    if (!isNil(_height.max)) height.push(`max: ${_height.max}`)
    if (height.length > 0) cond.push(`height : {${height.join(", ")}}`)
  }

  if (opt.first) cond.push(`first: ${opt.first}`)
  if (opt.after) cond.push(`after: "${opt.after}"`)
  if (opt.asc) cond.push(`sort: HEIGHT_ASC`)

  let _cond = ""
  if (cond.length > 0) _cond = `(${cond.join(", ")})`
  let fields = []
  if (is(Array, opt.fields) && opt.fields.length > 0) {
    let fields = []
    for (const v of opt.fields) {
      if (is(String, v)) {
        const fld = field_blocks(v)
        if (fld) fields.push(fld)
      } else if (is(Object, v) && !is(Array, v)) {
        for (const k in v) {
          const fld = field_blocks(k, v[k])
          if (fld) fields.push(fld)
        }
      }
    }
  } else if (is(Object, opt.fields) && !is(Array, opt.fields)) {
    for (const k in opt.fields) {
      const fld = field_blocks(k, opt.fields[k])
      if (fld) fields.push(fld)
    }
  }
  let _fields = fields.length > 0 ? fields.join(" ") : full_blocks
  return `query {
  blocks ${_cond}{
    edges { cursor node { ${_fields} } }
  }
}`
}

export default class GQL {
  constructor({ url = "https://arweave.net/graphql" }) {
    this.url = url
  }
  async _fetch(query, tar) {
    const json = await fetch(this.url, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ query }),
    }).then(r => r.json())
    return map(v => ({ cursor: v.cursor, ...v.node }))(json.data[tar].edges)
  }
  async fetch(opt = {}, query, tar = "transactions") {
    const data = await this._fetch(query, tar)
    if (opt.next === true) {
      let cursor = null
      if (data.length > 0) cursor = last(data).cursor
      const next = !cursor
        ? null
        : async () => {
            let _opt = clone(opt)
            _opt.after = cursor
            return await this.txs(_opt)
          }
      return { data, next }
    } else {
      return data
    }
  }
  async txs(opt = {}) {
    return await this.fetch(opt, query(opt))
  }
  async blocks(opt = {}) {
    return await this.fetch(opt, query_blocks(opt), "blocks")
  }
}
