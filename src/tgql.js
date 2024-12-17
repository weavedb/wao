import { reverse, includes, map, is, isNil, last, clone, pick } from "ramda"

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

export default class GQL {
  constructor({ mem }) {
    this.mem = mem
  }
  async txs(opt = {}) {
    let block_max = null
    let block_min = null
    let _block = {}
    if (is(Number, opt.block)) {
      _block = { min: opt.block, max: opt.block }
    } else if (is(Array, opt.block) && opt.block.length > 0) {
      _block = {}
      if (!isNil(opt.block[0])) _block.min = opt.block[0]
      if (!isNil(opt.block[1])) _block.max = opt.block[1]
    }
    let first = opt.first ?? 20
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
    let data = []
    let blocks = this.mem.blocks
    if (opt.asc !== true) blocks = reverse(blocks)
    let count = 0
    let after = false
    for (const v of blocks) {
      const block = clone(this.mem.blockmap[v])
      if (!isNil(_block.min) && block.height < _block.min) continue
      if (!isNil(_block.max) && block.height > _block.max) continue
      let txs = block.txs
      if (opt.asc !== true) txs = reverse(txs)
      for (const v2 of txs) {
        if (!isNil(opt.after) && opt.after === v2) {
          after = true
          break
        }
        if (!isNil(opt.after) && count === 0 && !after) continue
        let tx = this.mem.txs[v2]
        if (!isNil(ids) && ids.length > 0) {
          if (!includes(tx.id, ids)) continue
        }
        if (!isNil(owners) && owners.length > 0) {
          if (!includes(tx.owner, owners)) continue
        }

        if (!isNil(recipients) && recipients.length > 0) {
          if (!includes(tx.recipient, recipients)) continue
        }
        let tag_unmatch = false
        for (const v of tags) {
          let ex = false
          for (const v2 of tx.tags) {
            if (v2.name === v.name && includes(v2.value, v.values)) {
              ex = true
              break
            }
          }
          if (!ex) {
            tag_unmatch = true
            break
          }
        }
        if (tag_unmatch) continue
        let _tx = {
          cursor: tx.id,
          ...tx,
          data: tx._data,
          block: pick(["id", "timestamp", "height", "previous"], block),
          anchor: tx.anchor ?? "",
          signature: tx.signature ?? "",
          owner: { address: tx.owner, key: this.mem.addrmap[tx.owner] },
          fee: { ar: "0", winston: "0" },
          quantity: { ar: "0", winston: "0" },
          data: { size: "0", type: "" },
        }
        if (!isNil(opt.fields)) {
          let _tx2 = { cursor: tx.id }
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
              _tx2[f.key] = _tx[f.key] ?? ""
            } else {
              let subs = {}
              _tx2[f.key] = {}
              if (is(Array, _tx[f.key])) {
                _tx2[f.key] = map(pick(f.subs))(_tx[f.key])
              } else {
                for (const f2 of f.subs) {
                  _tx2[f.key][f2] = _tx[f.key][f2] ?? ""
                }
              }
            }
          }
          _tx = _tx2
        }
        data.push(_tx)
        count += 1
        if (count >= first) break
      }
      if (count >= first) break
    }
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
  async blocks(opt = {}) {
    let block_max = null
    let block_min = null
    let _block = {}
    if (is(Number, opt.block)) {
      _block = { min: opt.block, max: opt.block }
    } else if (is(Array, opt.block) && opt.block.length > 0) {
      _block = {}
      if (!isNil(opt.block[0])) _block.min = opt.block[0]
      if (!isNil(opt.block[1])) _block.max = opt.block[1]
    }
    let first = opt.first ?? 20

    let ids = null
    if (is(Array, opt.ids)) ids = opt.ids
    else if (is(String, opt.id)) ids = [opt.id]

    let data = []
    let blocks = this.mem.blocks
    if (opt.asc !== true) blocks = reverse(blocks)
    let count = 0
    let after = false
    for (const v of blocks) {
      let block = clone({ ...this.mem.blockmap[v], cursor: v })
      delete block.txs
      if (!isNil(_block.min) && block.height < _block.min) continue
      if (!isNil(_block.max) && block.height > _block.max) continue
      if (!isNil(ids) && ids.length > 0) if (!includes(v.id, ids)) continue
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
            let subs = {}
            block2[f.key] = {}
            if (is(Array, block[f.key])) {
              block2[f.key] = map(pick(f.subs))(block[f.key])
            } else {
              for (const f2 of f.subs) {
                block2[f.key][f2] = block[f.key][f2] ?? ""
              }
            }
          }
        }
        block = block2
      }
      data.push(block)
      count += 1
      if (count >= first) break
    }
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
}
