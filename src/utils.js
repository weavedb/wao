import { graphql, parse, validate, buildSchema } from "graphql"
import sha256 from "fast-sha256"
export { id, base, hashpath, rsaid, hmacid } from "./id.js"
import {
  clone,
  is,
  includes,
  fromPairs,
  map,
  isNil,
  equals,
  keys,
  omit,
} from "ramda"

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

const tags = (tags = []) => fromPairs(map(v => [v.name, v.value])(tags))

const ltags = (tags = []) =>
  fromPairs(map(v => [v.name.toLowerCase(), v.value])(tags))

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
    } else if (typeof data === "function") {
      return data(v.Data)
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
    isNil(get.from) &&
    isNil(get.json) &&
    isNil(get.name) &&
    isNil(get.obj)
  ) {
    _get = { obj: get }
  }
  return _get
}

const _getTagVal = (get, res, from) => {
  let out = null
  const _get = modGet(get)
  if (typeof _get === "object" && _get.obj) {
    out = {}
    for (const k in _get.obj ?? {}) out[k] = _getTagVal(_get.obj[k], res, from)
  } else {
    let i = 0
    for (const v of res.Messages ?? []) {
      if (typeof _get === "object" && isNil(_get.name) && isNil(_get.data)) {
        _get.data = true
      }
      if (
        (typeof _get === "object" && _get.data) ||
        typeof _get === "boolean"
      ) {
        let _from = null
        if (is(Object, _get) && _get.from) _from = _get.from
        if (_from && _from !== from) break
        if (v.Data) out = v.Data
        try {
          if (_get.json || _get === true) out = JSON.parse(out)
        } catch (e) {
          out = null
        }
      } else if (typeof _get === "object" && typeof _get.name === "string") {
        let _from = null
        if (is(Object, _get) && _get.from) _from = _get.from
        if (_from && _from !== from) break
        out = getTag(v.Tags ?? [], _get.name)
        try {
          if (_get.json) out = JSON.parse(out)
        } catch (e) {
          out = null
        }
      } else {
        out = getTag(v.Tags ?? [], _get)
      }
      if (
        out !== null &&
        typeof _get !== "string" &&
        typeof _get.match === "function"
      ) {
        if (!_get.match(out, i, res)) out = null
      }
      if (out) break
      i++
    }
  }
  return out
}

const getTagVal = (get, res, from) => {
  const _get = modGet(get)
  return _getTagVal(_get, res, from)
}

const srcs = {
  module_mainnet: "ISShJH1ij-hPPt9St5UFFr_8Ys3Kj5cyg7zrMGt7H9s",
  module: "cNlipBptaF9JeFAf4wUmpi43EojNanIBos3EfNrEOWo",
  module_sqlite: "ghSkge2sIUD_F00ym5sEimC63BDBuBrq4b5OcwxOjiw",
  module_aos2: "Do_Uc2Sju_ffp6Ev0AnLVdPtot15rvMjP-a9VVaA5fM",
  scheduler: "_GQ33BkPtZrqxA84vM8Zk-N2aO0toNNu_C-l-rawrBA",
  authority: "fcoN_xJeisVsPXA-trzVAuIiqO3ydLQxM-L4XbrQKzY",
  module_wao: "WASM32-D8q2OmZ4Mok00sD2Y_6SYEQ7Hjx-6VZ_jl3g",
  scheduler_wao: "_GQ33BkPtZrqxA84vM8Zk-N2aO0toNNu_C-l-rawrBA",
  authority_wao: "eNaLJLsMiWCSWvQKNbk_YT-9ydeWl9lrWwXxLVp9kcg",
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
  if (_get?.obj) {
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
  if (_get.obj) for (const k in out ?? {}) if (isNil(out[k])) return false
  return true
}

const isCheckComplete = (checks, check) => {
  let i = 0
  for (const v of checks || []) {
    if (!v) return false
    if (
      isRegExp(check[i]) ||
      includes(typeof check[i])(["string", "boolean"])
    ) {
      if (!v) return false
    } else for (const k in v) if (!v[k]) return false
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

const schema = buildSchema(`
type Query {
  transaction(id: ID!): Transaction
  transactions(
    ids: [ID!]
    owners: [String!]
    recipients: [String!]
    tags: [TagFilter!]
    bundledIn: [ID!]
    block: BlockFilter
    first: Int = 10
    after: String
    sort: SortOrder = HEIGHT_DESC
  ): TransactionConnection!
  block(id: String): Block
  blocks(
    ids: [ID!]
    height: BlockFilter
    first: Int = 10
    after: String
    sort: SortOrder = HEIGHT_DESC
  ): BlockConnection!
}

enum SortOrder {
  HEIGHT_ASC
  HEIGHT_DESC
}

input TagFilter {
  name: String!
  values: [String!]!
  op: TagOperator = EQ
}

input BlockFilter {
  min: Int
  max: Int
}

type BlockConnection {
  pageInfo: PageInfo!
  edges: [BlockEdge!]!
}

type BlockEdge {
  cursor: String!
  node: Block!
}

type TransactionConnection {
  pageInfo: PageInfo!
  edges: [TransactionEdge!]!
}

type TransactionEdge {
  cursor: String!
  node: Transaction!
}

type PageInfo {
  hasNextPage: Boolean!
}

type Transaction {
  id: ID!
  anchor: String!
  signature: String!
  recipient: String!
  owner: Owner!
  fee: Amount!
  quantity: Amount!
  data: MetaData!
  tags: [Tag!]!
  block: Block
  parent: Parent @deprecated(reason: "Use \`bundledIn\`")
  bundledIn: Bundle
}

type Parent {
  id: ID!
}

type Bundle {
  id: ID!
}

type Block {
  id: ID!
  timestamp: Int!
  height: Int!
  previous: ID!
}

type MetaData {
  size: String!
  type: String
}

type Amount {
  winston: String!
  ar: String!
}

type Owner {
  address: String!
  key: String!
}

type Tag {
  name: String!
  value: String!
}

enum TagOperator {
  EQ
  NEQ
}
`)

const root = {
  transactions: ({ first }) => ({
    edges: [],
    pageInfo: {
      hasNextPage: false,
    },
  }),
  block: ({ id }) => ({
    id,
    timestamp: Date.now(),
    height: 123456,
    previous: "previous-block-id",
    transactions: [],
    miner: "example-miner",
    reward: "1000",
    tags: [],
    indepHash: "example-indep-hash",
    nonce: "000000",
  }),
}

const mapParsed = (parsedQuery, variables) => {
  const operation = parsedQuery.definitions[0]

  if (operation.operation !== "query") {
    throw new Error("Only 'query' operations are supported.")
  }

  const rootField = operation.selectionSet.selections[0]
  const rootFieldName = rootField.name.value

  const parseArgumentValue = argValue => {
    if (argValue.kind === "Variable") {
      return variables[argValue.name.value]
    }

    if (argValue.kind === "ListValue") {
      return argValue.values.map(value => parseArgumentValue(value))
    }

    if (argValue.kind === "ObjectValue") {
      return argValue.fields.reduce((obj, field) => {
        obj[field.name.value] = parseArgumentValue(field.value)
        return obj
      }, {})
    }

    return argValue.value
  }

  const args = rootField.arguments.reduce((acc, arg) => {
    acc[arg.name.value] = parseArgumentValue(arg.value)
    return acc
  }, {})

  const extractFields = selectionSet => {
    return selectionSet.selections.map(selection => {
      const fieldName = selection.name.value

      if (selection.selectionSet) {
        return {
          [fieldName]: extractFields(selection.selectionSet),
        }
      }
      return fieldName
    })
  }

  const fields = extractFields(rootField.selectionSet)

  return { rootFieldName, args, fields }
}

const toGraphObj = ({ query, variables }) => {
  const parsedQuery = parse(query)
  const errors = validate(schema, parsedQuery)
  const parsed = mapParsed(parsedQuery, variables)
  const tar = parsed.rootFieldName
  let fields = null
  for (const v of parsed.fields) {
    if (v.edges) {
      for (const v2 of v.edges) {
        if (v2.node) {
          fields = v2.node
          break
        }
      }
    }
  }
  const args = parsed.args
  if (args.block) {
    for (const k in args.block) args.block[k] *= 1
  }
  if (args.first) args.first *= 1
  if (fields) args.fields = fields
  if (args.sort && args.sort === "HEIGHT_ASC") args.asc = true
  delete args.sort
  if (!args.tags) args.tags = []
  else if (!Array.isArray(args.tags)) args.tags = [args.tags]
  if (args.tags) {
    let _tags = {}
    for (const v of args.tags) _tags[v.name] = v.values
    args.tags = _tags
  }
  return { tar, args }
}
const optAO = port => {
  return {
    MU_URL: `http://localhost:${port + 2}`,
    SU_URL: `http://localhost:${port + 3}`,
    CU_URL: `http://localhost:${port + 4}`,
    GATEWAY_URL: `http://localhost:${port}`,
  }
}
const optServer = port => {
  return { ar: port, mu: port + 2, su: port + 3, cu: port + 4 }
}

const allChecked = (check, res, from) => {
  let checks = []
  let i = 0
  for (let v of check || []) {
    let _checks = checks[i] ?? null
    if (isRegExp(v) || includes(typeof v)(["string", "boolean", "function"])) {
      _checks = isData(v, res)
    } else {
      const _checkVal = (val, data) => {
        if (isRegExp(val)) {
          try {
            if (val.test(data)) return true
          } catch (e) {}
        } else if (typeof val === "function") {
          return val(data)
        } else if (typeof val === "object" && !isNil(val.json)) {
          try {
            data = JSON.parse(data)
            if (typeof val.json === "function") {
              return val.json(data)
            } else if (typeof val.json === "object") {
              if (val.eq) return equals(data, val.json)
              let ok = true
              for (let k in val.json) {
                if (!_checkVal(val.json[k], data[k])) {
                  ok = false
                  break
                }
              }
              return ok
            } else if (val.json === true) {
              return true
            }
          } catch (e) {
            return false
          }
        } else {
          if (val === true || data === val) return true
        }
        return false
      }
      const checkVal = (res, val, name) => {
        for (let v of res.Messages || []) {
          let data = v.Data
          if (!isNil(name)) {
            const t = tags(v.Tags || [])
            data = t[name]
          }
          if (isNil(data)) return false
          return _checkVal(val, data)
        }
        return false
      }
      if (typeof v === "object") {
        if (!isNil(v.json)) {
          const _from = v.from
          v = { data: v, from: _from }
        } else if (isNil(v.tags) && isNil(v.data) && isNil(v.from)) {
          v = { tags: v }
        }
        const _from = v.from ?? null
        let checks2 = {}
        if (!isNil(v.from) && v.from !== from) {
          _checks = false
        } else {
          let ok = true
          if (!isNil(v.data)) if (!checkVal(res, v.data)) ok = false
          for (const k in v.tags ?? {}) {
            if (!checkVal(res, v.tags[k], k)) ok = false
          }
          _checks = ok
        }
      }
    }
    checks[i] = _checks
    i++
  }
  return isCheckComplete(checks, check)
}

function toANS104Request(fields) {
  const dataItem = {
    target: fields.target,
    anchor: fields.anchor ?? "",
    tags: keys(
      omit(
        [
          "Target",
          "target",
          "Anchor",
          "anchor",
          "Data",
          "data",
          "data-protocol",
          "Data-Protocol",
          "variant",
          "Variant",
          "dryrun",
          "Dryrun",
          "Type",
          "type",
          "path",
          "method",
        ],
        fields
      )
    )
      .map(function (key) {
        return { name: key, value: fields[key] }
      }, fields)
      .concat([
        { name: "Data-Protocol", value: "ao" },
        { name: "Type", value: fields.Type ?? "Message" },
        { name: "Variant", value: fields.Variant ?? "ao.N.1" },
      ]),
    data: fields?.data || "",
  }
  return {
    headers: {
      "Content-Type": "application/ans104",
      "codec-device": "ans104@1.0",
    },
    item: dataItem,
  }
}

function parseSignatureInput(input) {
  const match = input.match(
    /^([^=]+)=\(([^)]+)\);alg="([^"]+)";keyid="([^"]+)"$/
  )
  if (!match) throw new Error("Invalid signature-input format")

  const [, label, fieldsStr, alg, keyid] = match
  const fields = fieldsStr.split('" "').map(f => f.replace(/"/g, ""))
  return { label, fields, alg, keyid }
}

function base64urlDecode(str) {
  str = str.replace(/-/g, "+").replace(/_/g, "/")
  const pad = str.length % 4
  if (pad === 2) str += "=="
  else if (pad === 3) str += "="
  else if (pad !== 0) throw new Error("Invalid base64url string")
  const bin = atob(str)
  const bytes = new Uint8Array(bin.length)
  for (let i = 0; i < bin.length; i++) bytes[i] = bin.charCodeAt(i)
  return bytes
}

function base64urlEncode(bytes) {
  let bin = ""
  for (const b of bytes) bin += String.fromCharCode(b)
  let b64 = btoa(bin)
  return b64.replace(/\+/g, "-").replace(/\//g, "_").replace(/=+$/g, "")
}

function toAddr(n) {
  const pubBytes = base64urlDecode(n)
  const hash = sha256(pubBytes)
  return base64urlEncode(hash)
}

export {
  toANS104Request,
  parseSignatureInput,
  allChecked,
  optAO,
  optServer,
  toGraphObj,
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
  toAddr,
}
