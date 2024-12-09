import { clone, is, includes, fromPairs, map, isNil } from "ramda"

const allows = [
  { key: "allowed", val: "Allowed" },
  { key: "disallowed", val: "Disallowed" },
]
const allowsMap = fromPairs(allows.map(({ key, val }) => [key, val]))
const accesses = [
  { key: "none", val: "None" },
  { key: "one-time", val: "One-Time" },
]
const accessesMap = fromPairs(accesses.map(({ key, val }) => [key, val]))
const payments = [
  { key: "single", val: "Single" },
  { key: "random", val: "Random" },
  { key: "global", val: "Global" },
]
const paymentsMap = fromPairs(payments.map(({ key, val }) => [key, val]))
const dTerms = [
  { key: "credit", val: "With Credit" },
  { key: "indication", val: "With Indication" },
  { key: "passthrough", val: "With License Passthrough" },
  { key: "revenue", val: "With Revenue Share" },
  { key: "monthly", val: "With Monthly Fee" },
  { key: "one-time", val: "With One-Time Fee" },
]
const dtMap = fromPairs(dTerms.map(({ key, val }) => [key, val]))
const cTerms = [
  { key: "revenue", val: "With Revenue Share" },
  { key: "monthly", val: "With Monthly Fee" },
  { key: "one-time", val: "With One-Time Fee" },
]
const ctMap = fromPairs(cTerms.map(({ key, val }) => [key, val]))
const tTerms = [
  { key: "monthly", val: "With Monthly Fee" },
  { key: "one-time", val: "With One-Time Fee" },
]
const ttMap = fromPairs(tTerms.map(({ key, val }) => [key, val]))

const action = value => tag("Action", value)
const tag = (name, value) => ({ name, value: jsonToStr(value) })

const wait = ms => new Promise(res => setTimeout(() => res(), ms))

const tags = tags => fromPairs(map(v => [v.name, v.value])(tags))
const ltags = tags => fromPairs(map(v => [v.name.toLowerCase(), v.value])(tags))

const validAddress = addr => /^[a-zA-Z0-9_-]{43}$/.test(addr)

const isRegExp = obj => obj instanceof RegExp

const getTag = (_tags, name) => tags(_tags)[name] ?? null

const tagEq = (tags, name, val = null) => {
  const tag = getTag(tags, name)
  if (val === true) {
    return tag !== null
  } else if (isRegExp(val)) {
    let ok = false
    try {
      ok = val.test(tag)
    } catch (e) {}
    return ok
  } else return tag === val
}

const searchTag = (res, name, val) => {
  for (let v of res.Messages || []) {
    if (tagEq(v.Tags || {}, name, val)) return v
  }
  return null
}

const checkTag = (res, name, val) => {
  for (let v of res.Messages || []) {
    if (tagEq(v.Tags || {}, name, val)) return true
  }
  return false
}

const isData = (data, res) => {
  for (const v of res.Messages ?? []) {
    if (isRegExp(data)) {
      try {
        if (data.test(v.Data)) return true
      } catch (e) {}
    } else {
      if (data === true || v.Data === data) return true
    }
  }
  return false
}

const isLocalhost = v => includes(v, ["localhost", "127.0.0.1"])

const udl = ({ payment, access, derivations, commercial, training }) => {
  let tags = {
    License: "dE0rmDfl9_OWjkDznNEXHaSO_JohJkRolvMzaCroUdw",
    Currency: "xU9zFkq3X2ZQ6olwNVvr1vUWIjc3kXTWr7xKQD6dh10",
  }
  tags["Payment-Mode"] = paymentsMap[payment.mode]
  if (payment.mode === "single") tags["Payment-Address"] = payment.recipient
  let _access = accessesMap[access.mode]
  if (access.mode === "one-time") _access += "-" + access.fee
  tags["Access-Fee"] = _access

  let _derivations = allowsMap[derivations.mode]
  if (derivations.mode === "allowed") {
    if (derivations.term === "revenue") {
      _derivations += `-${dtMap[derivations.term].split(" ").join("-")}-${derivations.share}`
    } else if (
      derivations.term === "monthly" ||
      derivations.term === "one-time"
    ) {
      _derivations += `-${dtMap[derivations.term].split(" ").join("-")}-${derivations.fee}`
    } else {
      _derivations += `-${dtMap[derivations.term].split(" ").join("-")}-0`
    }
  }
  tags["Derivations"] = _derivations
  let _commercial = allowsMap[commercial.mode]
  if (commercial.mode === "allowed") {
    if (commercial.term === "revenue") {
      _commercial += `-${ctMap[commercial.term].split(" ").join("-")}-${commercial.share}`
    } else {
      _commercial += `-${ctMap[commercial.term].split(" ").join("-")}-${commercial.fee}`
    }
  }
  tags["Commercial-Use"] = _commercial
  let _training = allowsMap[training.mode]
  if (training.mode === "allowed") {
    _training += `-${ttMap[training.term].split(" ").join("-")}-${training.fee}`
  }
  tags["Data-Model-Training"] = _training
  return tags
}

const modGet = get => {
  let _get = clone(get)
  if (is(Array, get)) {
    _get = { obj: {} }
    for (const v of get) {
      if (typeof v === "string") _get.obj[v] = v
      else if (is(Array, v)) _get.obj[v[0]] = v[1]
      else if (is(Object, v)) for (const k in v) _get.obj[k] = v[k]
    }
  } else if (
    is(Object, get) &&
    isNil(get.data) &&
    isNil(get.json) &&
    isNil(get.name) &&
    isNil(get.obj)
  ) {
    _get = { obj: get }
  }
  return _get
}

const _getTagVal = (get, res) => {
  let out = null
  const _get = modGet(get)
  if (typeof _get === "object" && _get.obj) {
    out = {}
    for (const k in _get.obj ?? {}) out[k] = _getTagVal(_get.obj[k], res)
  } else {
    for (const v of res.Messages ?? []) {
      if (
        (typeof _get === "object" && _get.data) ||
        typeof _get === "boolean"
      ) {
        if (v.Data) out = v.Data
        try {
          if (_get.json || _get === true) out = JSON.parse(out)
        } catch (e) {}
      } else if (typeof _get === "object" && typeof _get.name === "string") {
        out = getTag(v.Tags ?? [], _get.name)
        try {
          if (_get.json) out = JSON.parse(out)
        } catch (e) {}
      } else out = getTag(v.Tags ?? [], _get)
      if (out) break
    }
  }
  return out
}

const getTagVal = (get, res) => {
  const _get = modGet(get)
  return _getTagVal(_get, res)
}

const srcs = {
  module: "cNlipBptaF9JeFAf4wUmpi43EojNanIBos3EfNrEOWo",
  module_sqlite: "ghSkge2sIUD_F00ym5sEimC63BDBuBrq4b5OcwxOjiw",
  module_aos2: "Do_Uc2Sju_ffp6Ev0AnLVdPtot15rvMjP-a9VVaA5fM",
  scheduler: "_GQ33BkPtZrqxA84vM8Zk-N2aO0toNNu_C-l-rawrBA",
  authority: "fcoN_xJeisVsPXA-trzVAuIiqO3ydLQxM-L4XbrQKzY",
}

const buildTags = (act, tags) => {
  let _tags = []
  if (isNil(tags) && typeof act !== "string") {
    tags = act
    act = null
  }
  if (act) _tags.push(action(act))
  for (const k in tags) {
    if (is(Array)(tags[k])) for (const v of tags[k]) _tags.push(tag(k, v))
    else _tags.push(tag(k, tags[k]))
  }
  return _tags
}

const mergeOut = (out, out2, get) => {
  const _get = modGet(get)
  if (_get.obj) {
    for (const k in out2 ?? {}) {
      if (isNil(out?.[k])) {
        if (!out) out = {}
        out[k] = out2[k]
      }
    }
    return out
  } else return out2
}

const mergeChecks = (check1, check2, check) => {
  if (!isRegExp(check) && !includes(typeof check)(["string", "boolean"])) {
    for (const k in check2 ?? {}) {
      if (!check1) check1 = {}
      check1[k] = check1[k] || check2[k]
    }
    return check1
  } else return check1 || check2
}

const isOutComplete = (out, get) => {
  if (isNil(get)) return true
  if (isNil(out)) return false
  const _get = modGet(get)
  if (_get.obj) {
    for (const k in out ?? {}) {
      if (isNil(out[k])) return false
    }
  }
  return true
}

const isCheckComplete = (checks, check) => {
  let i = 0
  for (const v of checks) {
    if (
      isRegExp(check[i]) ||
      includes(typeof check[i])(["string", "boolean"])
    ) {
      if (!v) return false
    } else {
      for (const k in v) {
        if (!v[k]) return false
      }
    }
    i++
  }
  return true
}

const dirname = async () =>
  typeof __dirname != "undefined"
    ? __dirname
    : (await import("./dirname.js")).default

function isJSON(obj) {
  if (obj === null || obj === undefined) return false
  if (
    typeof obj !== "object" ||
    obj instanceof Buffer ||
    obj instanceof ArrayBuffer ||
    Array.isArray(obj)
  ) {
    return false
  }

  try {
    const str = JSON.stringify(obj)
    const parsed = JSON.parse(str)
    const isjson = typeof parsed === "object" && parsed !== null
    return isjson ? str : false
  } catch (e) {
    return false
  }
}

const jsonToStr = obj =>
  isJSON(obj) || (is(Number, obj) ? Number(obj).toString() : obj)

export {
  jsonToStr,
  mergeChecks,
  isCheckComplete,
  mergeOut,
  isOutComplete,
  isRegExp,
  buildTags,
  srcs,
  getTagVal,
  isData,
  getTag,
  tagEq,
  searchTag,
  checkTag,
  validAddress,
  ltags,
  tags,
  wait,
  action,
  tag,
  isLocalhost,
  udl,
  isJSON,
  dirname,
}
