// D1-backed GQL query engine
// Replaces O(n) scan in tgql.js with indexed SQL queries

import { reverse, is, isNil, last, clone, pick, map, includes } from "ramda"

const subs = {
  owner: ["address", "key"],
  fee: ["winston", "ar"],
  quantity: ["winston", "ar"],
  data: ["size", "type"],
  tags: ["name", "value"],
  block: ["id", "timestamp", "height", "previous"],
  parent: ["id"],
  bundledIn: ["id"],
}

const field = (key, val = true) => {
  if (includes(key, ["id", "anchor", "signature", "recipient"])) {
    return { key }
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
    return { key, subs: _subs }
  }
  return null
}

const field_blocks = (key, val = true) => {
  if (includes(key, ["id", "timestamp", "height", "previous"])) {
    return { key }
  }
  return null
}

function applyFieldProjection(tx, opt) {
  if (isNil(opt.fields)) return tx

  let _tx2 = { cursor: tx.cursor }
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
  for (const f of fields) {
    if (isNil(f.subs)) {
      _tx2[f.key] = tx[f.key] ?? ""
    } else {
      _tx2[f.key] = {}
      if (is(Array, tx[f.key])) {
        _tx2[f.key] = map(pick(f.subs))(tx[f.key])
      } else {
        for (const f2 of f.subs) {
          _tx2[f.key][f2] = tx[f.key]?.[f2] ?? ""
        }
      }
    }
  }
  return _tx2
}

export default class D1GQL {
  constructor({ d1, mem }) {
    this.d1 = d1
    this.mem = mem
  }

  async txs(opt = {}) {
    let _block = {}
    if (is(Number, opt.block)) {
      _block = { min: opt.block, max: opt.block }
    } else if (is(Array, opt.block) && opt.block.length > 0) {
      _block = {}
      if (!isNil(opt.block[0])) _block.min = opt.block[0]
      if (!isNil(opt.block[1])) _block.max = opt.block[1]
    } else if (is(Object, opt.block)) {
      if (!isNil(opt.block.min)) _block.min = opt.block.min
      if (!isNil(opt.block.max)) _block.max = opt.block.max
    }

    const first = opt.first ?? 10
    let tags = []
    for (const k in opt.tags ?? {}) {
      if (is(String, opt.tags[k])) {
        tags.push({ name: k, values: [opt.tags[k]] })
      } else if (is(Array, opt.tags[k])) {
        tags.push({ name: k, values: opt.tags[k] })
      }
    }

    let recipients = null
    if (is(Array, opt.recipients)) recipients = opt.recipients
    else if (is(String, opt.recipient)) recipients = [opt.recipient]

    let owners = null
    if (is(Array, opt.owners)) owners = opt.owners
    else if (is(String, opt.owner)) owners = [opt.owner]

    let ids = null
    if (is(Array, opt.ids)) ids = opt.ids
    else if (is(String, opt.id)) ids = [opt.id]

    // Build SQL query
    const conditions = []
    const params = []

    if (!isNil(_block.min)) {
      conditions.push("t.block_height >= ?")
      params.push(_block.min)
    }
    if (!isNil(_block.max)) {
      conditions.push("t.block_height <= ?")
      params.push(_block.max)
    }
    if (ids && ids.length > 0) {
      conditions.push(`t.id IN (${ids.map(() => "?").join(",")})`)
      params.push(...ids)
    }
    if (owners && owners.length > 0) {
      conditions.push(`t.owner IN (${owners.map(() => "?").join(",")})`)
      params.push(...owners)
    }
    if (recipients && recipients.length > 0) {
      conditions.push(`t.recipient IN (${recipients.map(() => "?").join(",")})`)
      params.push(...recipients)
    }
    if (!isNil(opt.bundledIn) && opt.bundledIn.length > 0) {
      conditions.push(`t.bundle_id IN (${opt.bundledIn.map(() => "?").join(",")})`)
      params.push(...opt.bundledIn)
    }

    // Tag filters — each tag filter requires ALL to match (AND)
    for (const tag of tags) {
      const valuePlaceholders = tag.values.map(() => "?").join(",")
      conditions.push(
        `t.id IN (SELECT tx_id FROM tx_tags WHERE name = ? AND value IN (${valuePlaceholders}))`
      )
      params.push(tag.name, ...tag.values)
    }

    // After cursor (pagination)
    if (!isNil(opt.after)) {
      // Get the block_height of the cursor tx to paginate correctly
      const cursorRow = await this.d1
        .prepare("SELECT block_height FROM txs WHERE id = ?")
        .bind(opt.after)
        .first()
      if (cursorRow) {
        if (opt.asc === true) {
          conditions.push("(t.block_height > ? OR (t.block_height = ? AND t.id > ?))")
          params.push(cursorRow.block_height, cursorRow.block_height, opt.after)
        } else {
          conditions.push("(t.block_height < ? OR (t.block_height = ? AND t.id < ?))")
          params.push(cursorRow.block_height, cursorRow.block_height, opt.after)
        }
      }
    }

    const where = conditions.length > 0 ? `WHERE ${conditions.join(" AND ")}` : ""
    const order = opt.asc === true ? "ASC" : "DESC"

    const sql = `
      SELECT t.id, t.block_id, t.block_height, t.owner, t.recipient,
             t.anchor, t.signature, t.data_size, t.data_type, t.bundle_id, t.parent_id,
             b.height, b.timestamp, b.previous
      FROM txs t
      JOIN blocks b ON t.block_id = b.id
      ${where}
      ORDER BY t.block_height ${order}, t.id ${order}
      LIMIT ?
    `
    params.push(first)

    const { results: rows } = await this.d1
      .prepare(sql)
      .bind(...params)
      .all()

    // Fetch tags for all result txs in one query
    const txIds = rows.map(r => r.id)
    let tagMap = {}
    if (txIds.length > 0) {
      const tagSql = `SELECT tx_id, name, value FROM tx_tags WHERE tx_id IN (${txIds.map(() => "?").join(",")})`
      const { results: tagRows } = await this.d1
        .prepare(tagSql)
        .bind(...txIds)
        .all()
      for (const t of tagRows) {
        tagMap[t.tx_id] ??= []
        tagMap[t.tx_id].push({ name: t.name, value: t.value })
      }
    }

    // Fetch addrmap for owners
    const ownerAddrs = [...new Set(rows.map(r => r.owner).filter(Boolean))]
    let addrMap = {}
    if (ownerAddrs.length > 0) {
      const addrSql = `SELECT address, key FROM addrmap WHERE address IN (${ownerAddrs.map(() => "?").join(",")})`
      const { results: addrRows } = await this.d1
        .prepare(addrSql)
        .bind(...ownerAddrs)
        .all()
      for (const a of addrRows) {
        addrMap[a.address] = { address: a.address, key: a.key }
      }
    }

    const data = rows.map(row => {
      let _tx = {
        cursor: row.id,
        id: row.id,
        recipient: row.recipient || "",
        tags: tagMap[row.id] || [],
        parent: row.parent_id ? { id: row.parent_id } : { id: "" },
        bundledIn: row.bundle_id ? { id: row.bundle_id } : { id: "" },
        block: {
          id: row.block_id,
          timestamp: row.timestamp,
          height: row.height,
          previous: row.previous || "",
        },
        anchor: row.anchor || "",
        signature: row.signature || "",
        owner: addrMap[row.owner] || { address: row.owner, key: "" },
        fee: { ar: "0.000000000000", winston: "0" },
        quantity: { ar: "0.000000000000", winston: "0" },
        data: { size: row.data_size || "0", type: row.data_type || "" },
      }
      return applyFieldProjection(_tx, opt)
    })

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
    }
    return data
  }

  async blocks(opt = {}) {
    let _block = {}
    if (is(Number, opt.block)) {
      _block = { min: opt.block, max: opt.block }
    } else if (is(Array, opt.block) && opt.block.length > 0) {
      _block = {}
      if (!isNil(opt.block[0])) _block.min = opt.block[0]
      if (!isNil(opt.block[1])) _block.max = opt.block[1]
    }
    const first = opt.first ?? 20

    let ids = null
    if (is(Array, opt.ids)) ids = opt.ids
    else if (is(String, opt.id)) ids = [opt.id]

    const conditions = []
    const params = []

    if (!isNil(_block.min)) {
      conditions.push("height >= ?")
      params.push(_block.min)
    }
    if (!isNil(_block.max)) {
      conditions.push("height <= ?")
      params.push(_block.max)
    }
    if (ids && ids.length > 0) {
      conditions.push(`id IN (${ids.map(() => "?").join(",")})`)
      params.push(...ids)
    }

    if (!isNil(opt.after)) {
      const cursorRow = await this.d1
        .prepare("SELECT height FROM blocks WHERE id = ?")
        .bind(opt.after)
        .first()
      if (cursorRow) {
        if (opt.asc === true) {
          conditions.push("height > ?")
          params.push(cursorRow.height)
        } else {
          conditions.push("height < ?")
          params.push(cursorRow.height)
        }
      }
    }

    const where = conditions.length > 0 ? `WHERE ${conditions.join(" AND ")}` : ""
    const order = opt.asc === true ? "ASC" : "DESC"

    const sql = `
      SELECT id, height, timestamp, previous
      FROM blocks
      ${where}
      ORDER BY height ${order}
      LIMIT ?
    `
    params.push(first)

    const { results: rows } = await this.d1
      .prepare(sql)
      .bind(...params)
      .all()

    let data = rows.map(row => {
      let block = {
        cursor: row.id,
        id: row.id,
        height: row.height,
        timestamp: row.timestamp,
        previous: row.previous || "",
      }

      // Apply field projection
      if (!isNil(opt.fields)) {
        let block2 = { cursor: block.id }
        let fields = []
        if (is(Array, opt.fields) && opt.fields.length > 0) {
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
        for (const f of fields) {
          if (isNil(f.subs)) {
            block2[f.key] = block[f.key] ?? ""
          } else {
            block2[f.key] = {}
            if (is(Array, block[f.key])) {
              block2[f.key] = map(pick(f.subs))(block[f.key])
            } else {
              for (const f2 of f.subs) {
                block2[f.key][f2] = block[f.key]?.[f2] ?? ""
              }
            }
          }
        }
        block = block2
      }

      return block
    })

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
    }
    return data
  }
}
