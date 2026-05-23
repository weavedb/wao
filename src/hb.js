import { createSigner } from "@permaweb/aoconnect"
import { isEmpty, last, isNotNil, mergeLeft, clone } from "ramda"
import { toAddr, buildTags, seed, srcs } from "./utils.js"
import {
  httpsig_from,
  structured_to,
  rsaid,
  hmacid,
  sign,
  signer,
  send as _send,
  commit,
  result,
} from "hbsig"
import hyper_aos from "./hyper-aos.js"
import aos_wamr from "./aos_wamr.js"
import { ArweaveSigner } from "@ar.io/sdk"
import { createData } from "@dha-team/arbundles"

// Convert objects whose keys are sequential numeric strings (e.g.
// `{1: "a", 2: "b"}`) into arrays. v0.9-FINAL's multipart re-encoder treats
// numbered TABMs as lists; JS hbsig only emits `.="list"` in ao-types when
// the value is an Array, so without this conversion the content-digest of
// the request body diverges from HB's recomputation and rsa-pss verification
// fails with `process_not_verified`.
const normalizeNumberedObjects = v => {
  if (v === null || v === undefined) return v
  if (Array.isArray(v)) return v.map(normalizeNumberedObjects)
  if (Buffer.isBuffer(v) || v instanceof Blob) return v
  if (typeof v !== "object") return v
  const keys = Object.keys(v)
  if (
    keys.length > 0 &&
    keys.every(k => /^[1-9][0-9]*$/.test(k)) &&
    keys
      .map(k => parseInt(k, 10))
      .sort((a, b) => a - b)
      .every((n, i) => n === i + 1)
  ) {
    return keys
      .map(k => parseInt(k, 10))
      .sort((a, b) => a - b)
      .map(i => normalizeNumberedObjects(v[String(i)]))
  }
  const out = {}
  for (const [k, val] of Object.entries(v)) out[k] = normalizeNumberedObjects(val)
  return out
}

// v0.9-FINAL returns Messages with lowercase fields (data, target, anchor, from)
// where legacy AOS-format consumers expect capitalised (Data, Target, Anchor,
// From). Also lower-cases tag {name, value} pairs and may emit them as a
// numbered-key map under "tags" rather than an array under "Tags". Normalise
// so existing callers that read `Messages[0].Data` keep working.
const _normalizeLegacyResult = res => {
  if (!res || typeof res !== "object") return res
  const _msgs = res.Messages ?? res.messages
  if (!Array.isArray(_msgs)) return res
  const _tagsArray = t => {
    if (!t) return []
    if (Array.isArray(t)) {
      return t.map(x => ({
        name: x?.name ?? x?.Name,
        value: x?.value ?? x?.Value,
      })).filter(x => x.name != null)
    }
    if (typeof t === "object") {
      return Object.keys(t)
        .filter(k => /^\d+$/.test(k))
        .sort((a, b) => Number(a) - Number(b))
        .map(k => {
          const x = t[k]
          return {
            name: x?.name ?? x?.Name,
            value: x?.value ?? x?.Value,
          }
        })
        .filter(x => x.name != null)
    }
    return []
  }
  const Messages = _msgs.map(m => {
    if (!m || typeof m !== "object") return m
    const tags = _tagsArray(m.Tags ?? m.tags)
    return {
      ...m,
      Data: m.Data ?? m.data,
      Target: m.Target ?? m.target,
      Anchor: m.Anchor ?? m.anchor,
      From: m.From ?? m.from,
      Tags: tags,
    }
  })
  return { ...res, Messages }
}

const toMsg = async req => {
  let msg = {}
  req?.headers?.forEach((v, k) => {
    msg[k] = v
  })
  //if (req.body) msg.body = await req.text?.()
  if (req.body) {
    const arrayBuffer = await req.arrayBuffer()
    msg.body =
      typeof Buffer !== "undefined"
        ? Buffer.from(arrayBuffer) // Node.js
        : new Uint8Array(arrayBuffer) // Browser
  }
  return msg
}

class HB {
  constructor({
    url = "http://localhost:10001",
    cu = "http://localhost:6363",
    jwk,
    format = "httpsig",
  } = {}) {
    this.format = format
    this.cu = cu
    this.url = url
    if (jwk) this._init(jwk)
  }
  async signEncoded(encoded) {
    const { path, ...msg } = encoded
    return await sign({
      jwk: this.jwk,
      msg,
      path,
      url: this.url,
    })
  }

  isArConnect() {
    return this.jwk?.id || this.jwk?.walletName === "ArConnect"
  }

  _init(jwk) {
    this.jwk = jwk
    this.signer = createSigner(jwk, this.url)
    if (this.jwk && !this.isArConnect()) this.addr = toAddr(jwk.n)
    this.sign = signer({ signer: this.signer, url: this.url })
  }

  async setInfo() {
    if (!this.operator) {
      try {
        this.operator = await this.g("/~meta@1.0/info/address")
      } catch (e) {
        console.log(e)
      }
    }
  }

  async init(jwk) {
    this._init(jwk)
    await this.setInfo()
    return this
  }

  async send(msg) {
    return await _send(msg)
  }

  async getImage() {
    const wasm = Buffer.from(aos_wamr, "base64")
    const id = await this.cacheBinary(wasm, "application/wasm")
    this.image ??= id
    return id
  }

  async getLua() {
    // Decode base64 to UTF-8 text string (Lua source code)
    const lua = Buffer.from(hyper_aos, "base64").toString("utf-8")
    const id = await this.cacheBinary(lua, "application/lua")
    this.lua ??= id
    return id
  }

  async messageAOS(args) {
    const { slot, pid } = await this.scheduleAOS(args)
    return { slot, outbox: await this.computeAOS({ pid, slot }) }
  }

  // JS-side push for AOS (wasm-64/stack@1.0) processes.
  // The HyperBEAM push@1.0 device has a bug where subresolve's deep-merge
  // modifies the process key, causing a ProcID mismatch and cache miss.
  // This method implements the same logic using direct HTTP calls that work.
  // Handles full cross-process round-trips: A→B→A for receive() resolution.
  async pushAOS({ pid, slot, outbox: initialOutbox, maxDepth = 10 }) {
    let currentSlot = Number(slot)
    let lastOutbox = null

    for (let depth = 0; depth < maxDepth; depth++) {
      let outbox
      if (depth === 0 && initialOutbox && typeof initialOutbox === "object") {
        outbox = initialOutbox
      } else {
        try {
          outbox = await this.computeAOS({ pid, slot: currentSlot })
        } catch (_e) {
          break
        }
      }
      lastOutbox = outbox
      if (!outbox || typeof outbox !== "object") break

      const msgs = this._extractOutboxMsgs(outbox)
      if (msgs.length === 0) break

      let hasSelfMsg = false
      let nextSlot = currentSlot

      for (const msg of msgs) {
        const target = msg.target ?? msg.Target
        if (!target) continue

        const tags = {}
        for (const [key, val] of Object.entries(msg)) {
          const lk = key.toLowerCase()
          if (lk === "data" || lk === "target" || lk === "anchor") continue
          if (typeof val === "string" || typeof val === "number") {
            tags[key] = String(val)
          }
        }
        tags["from-process"] = pid

        const data = msg.data ?? msg.Data ?? ""
        const action = tags.Action ?? tags.action ?? null
        delete tags.Action
        delete tags.action

        try {
          const { slot: newSlot } = await this.scheduleAOS({
            pid: target,
            action,
            data,
            tags,
          })
          if (target === pid) {
            nextSlot = Math.max(nextSlot, Number(newSlot))
            hasSelfMsg = true
          } else {
            // Cross-process: compute target, deliver any replies back to pid
            try {
              const targetOutbox = await this.computeAOS({
                pid: target,
                slot: newSlot,
              })
              if (targetOutbox && typeof targetOutbox === "object") {
                const replies = this._extractOutboxMsgs(targetOutbox)
                for (const reply of replies) {
                  const replyTarget = reply.target ?? reply.Target
                  if (!replyTarget) continue
                  const rTags = {}
                  for (const [rk, rv] of Object.entries(reply)) {
                    const rl = rk.toLowerCase()
                    if (rl === "data" || rl === "target" || rl === "anchor")
                      continue
                    if (typeof rv === "string" || typeof rv === "number") {
                      rTags[rk] = String(rv)
                    }
                  }
                  rTags["from-process"] = target
                  const rData = reply.data ?? reply.Data ?? ""
                  const rAction = rTags.Action ?? rTags.action ?? null
                  delete rTags.Action
                  delete rTags.action
                  try {
                    const { slot: replySlot } = await this.scheduleAOS({
                      pid: replyTarget,
                      action: rAction,
                      data: rData,
                      tags: rTags,
                    })
                    if (replyTarget === pid) {
                      nextSlot = Math.max(nextSlot, Number(replySlot))
                      hasSelfMsg = true
                    }
                  } catch (_e) {}
                }
              }
            } catch (_e) {}
          }
        } catch (_e) {}
      }

      if (!hasSelfMsg) break
      currentSlot = nextSlot
    }

    return { slot: currentSlot, outbox: lastOutbox }
  }

  // Extract outbox messages from a compute result (numbered keys).
  _extractOutboxMsgs(outbox) {
    return Object.keys(outbox)
      .filter(k => /^\d+$/.test(k))
      .sort((a, b) => Number(a) - Number(b))
      .map(k => outbox[k])
      .filter(m => m && typeof m === "object")
  }

  async messageLegacy(args) {
    const { slot, pid } = await this.scheduleLegacy(args)
    return { slot, res: await this.computeLegacy({ pid, slot }) }
  }

  async computeAOS({ pid, slot }) {
    return await this.getJSON({ path: `/${pid}/compute/results/outbox`, slot })
  }

  async computeLua({ pid, slot }) {
    return await this.getJSON({ path: `/${pid}/compute/results`, slot })
  }

  async compute({ pid, slot, path = "" }) {
    if (path && !/^\//.test(path)) path = "/" + path
    if (this.format === "ans104") {
      const res = await this.get({ path: `/${pid}/compute${path}`, slot })
      return res.out
    } else {
      return await this.getJSON({ path: `/${pid}/compute${path}`, slot })
    }
  }

  async computeLegacy({ pid, slot }) {
    // Match master: compute and parse results.json.body
    const json = await this.compute({ pid, slot })
    let parsed = json
    if (json?.results?.json?.body) {
      parsed = JSON.parse(json.results.json.body)
    } else if (json?.["compute/results/json"]?.body) {
      parsed = JSON.parse(json["compute/results/json"].body)
    } else if (json?.results?.raw) {
      const raw = typeof json.results.raw === "string"
        ? JSON.parse(json.results.raw) : json.results.raw
      if (raw?.Messages || raw?.Output) parsed = raw
    }
    return _normalizeLegacyResult(parsed)
  }

  async cacheScript(data, type = "application/lua") {
    if (!this.cache) {
      const { pid } = await this.spawn({})
      this.cache = pid
    }
    const { slot } = await this.schedule({
      data,
      pid: this.cache,
      tags: { "content-type": type },
    })
    const msgs = await this.messages({ pid: this.cache, from: slot, limit: 1 })
    return msgs.edges[0].node.message.Id
  }

  async cacheBinary(data, type) {
    // Convert Buffer to base64 string to avoid signature mismatch in JSON POST.
    // Buffer goes through structured field byte encoding in commit (`:base64:`)
    // but jsonReplacer converts to plain base64 string, causing invalid_commitment.
    const dataStr = Buffer.isBuffer(data) ? data.toString("base64") : data
    const res = await this.post({
      path: "/~wao@1.0/cache_module",
      data: dataStr,
      type,
    })
    return res.out.id
  }

  async message(args) {
    const pid = args.pid
    const { slot } = await this.schedule(args)
    const res = await this.compute({ pid, slot })
    return { slot, pid, res }
  }

  async scheduleFlat({ pid, tags = {}, data } = {}) {
    let _tags = mergeLeft(tags, { Type: "Message", target: pid })
    if (data) _tags.data = data
    let res = await this.post({ path: "/~process@1.0/schedule", body: _tags })
    return { slot: res.out.slot, res, pid }
  }

  async scheduleNP({ pid, tags = {}, data } = {}) {
    if (data) tags.data = data
    // Use direct fetch to avoid post() path conflation.
    // Commit tags without mixing in the request path.
    tags.nonce ??= seed(8)
    const committed = await this.commit(tags, { path: false })
    const requestPath = `/${pid}~node-process@1.0/schedule`
    const response = await fetch(`${this.url}${requestPath}`, {
      method: "POST",
      headers: { "content-type": "application/json", "accept-bundle": "true" },
      body: JSON.stringify(committed),
    })
    if (response.status >= 400) {
      const text = await response.text()
      throw new Error(`${response.status}: ${text}`)
    }
    const res = await result(response)
    return { slot: res.out?.slot, res, pid }
  }

  async send104({ path = "/~process@1.0/schedule", item }) {
    let res = await fetch(`${this.url}${path}`, {
      method: "POST",
      headers: {
        "codec-device": "ans104@1.0",
        "Content-Type": "application/ans104",
      },
      body: item.binary,
    })
    return await result(res)
  }

  async post104({
    path = "/~process@1.0/schedule",
    tags = {},
    data = "1984",
    target,
  }) {
    const _tags = buildTags(mergeLeft(tags, { signingFormat: "ANS-104" }))
    const signer = new ArweaveSigner(this.jwk)
    const item = createData(data, signer, { tags: _tags, target })
    await item.sign(signer)
    return await this.send104({ path, item })
  }

  async schedule({ pid, tags = {}, data } = {}) {
    let res = null
    if (this.format === "ans104") {
      let _tags = mergeLeft(tags, { Type: "Message" })
      res = await this.post104({
        target: pid,
        path: `/${pid}/schedule`,
        tags: _tags,
        data: data ?? "1984",
      })
      return { slot: res.out.slot, res, pid }
    } else {
      let _tags = mergeLeft(tags, { Type: "Message", target: pid })
      if (data) _tags.data = data

      const res = await this.post({
        path: "/~scheduler@1.0/schedule",
        ..._tags,
      })

      const slot = parseInt(res.headers?.slot ?? res.out?.slot)
      return {
        slot,
        pid,
        res: { status: res.status },
      }
    }
  }

  async scheduleLua({ action = "Eval", tags = {}, ...rest }) {
    if (action) tags.Action = action
    return await this.schedule({ tags, ...rest })
  }

  async spawnLua(lua) {
    await this.setInfo()
    // Try local wao@1.0 cache first, fall back to Arweave TX ID for remote
    let module = this.lua
    if (!module) {
      try {
        module = await this.getLua()
      } catch (_e) {
        module = srcs.module_lua
      }
    }
    const tags = {
      "data-protocol": "ao",
      variant: "ao.TN.1",
      module,
      "execution-device": "lua@5.3a",
      "push-device": "push@1.0",
      "patch-from": "/results/outbox",
      // Note: 'authority' excluded - conflicts with HTTP Message Signatures '@authority'
      // The Lua boot module (hyper-aos.js) is patched to default ao.authorities to {}
    }
    return this.spawn(tags)
  }

  async now({ pid, path = "" }) {
    if (path && !/^\//.test(path)) path = "/" + path
    if (this.format === "ans104") {
      const res = await this.get({ path: `/${pid}/now${path}` })
      return res.out
    } else {
      return await this.getJSON({ path: `/${pid}/now${path}` })
    }
  }

  async slot({ pid, path = "" }) {
    if (path && !/^\//.test(path)) path = "/" + path
    return await this.getJSON({ path: `/${pid}/slot${path}` })
  }

  async messages({ pid, from, to } = {}) {
    let params = `target=${pid}`
    if (isNotNil(from)) params += `&from=${from}`
    if (isNotNil(to)) params += `&to=${to}`
    params += `&accept=application/aos-2`
    let res = await fetch(`${this.url}/~scheduler@1.0/schedule?${params}`).then(
      r => r.json()
    )
    if (res.page_info.has_next_page) {
      res.next = async () => {
        const from2 = last(res.edges).cursor + 1
        return await this.messages({ pid, from: from2, to })
      }
    }
    return res
  }
  async spawn(tags = {}) {
    await this.setInfo()
    // v0.9-FINAL multipart re-encoding sees a numbered-key object as a list
    // (TABM with .="list" in ao-types). JS hbsig's structured codec encodes
    // a plain numeric-keyed object without that marker, so HB's recomputed
    // content-digest doesn't match what JS signed and verify fails. Pre-
    // normalize any numbered-key object value into an array so the array
    // path (which already emits .="list") is used.
    tags = normalizeNumberedObjects(tags)
    let res = null
    if (this.format === "ans104") {
      res = await this.post104({
        tags: mergeLeft(tags, {
          "codec-device": "ans104@1.0",
          "random-seed": seed(16),
          Type: "Process",
          "execution-device": "test-device@1.0",
          device: "process@1.0",
          Scheduler: this.operator,
        }),
      })
      return { res, pid: res.out.process }
    } else {
      // Use httpsig-signed multipart POST
      const spawnTags = mergeLeft(tags, {
        "random-seed": seed(16),
        type: "Process",
        "execution-device": "test-device@1.0",
        device: "process@1.0",
        scheduler: this.operator ?? this.addr,
      })

      const res = await this.post({
        path: "/~scheduler@1.0/schedule",
        ...spawnTags,
      })

      return {
        pid: res.headers?.process || res.out?.process,
        slot: parseInt(res.headers?.slot ?? res.out?.slot),
        res: { status: res.status },
      }
    }
  }

  async spawnLegacy({ module, tags = {}, data } = {}) {
    await this.setInfo()
    // Use ANS-104 format to include 'authority' and 'push-device' tags.
    // httpsig format can't include 'authority' — it conflicts with HTTP Signatures'
    // @authority derived component (RFC 9421), causing signature verification failure.
    const legacyTags = {
      "Data-Protocol": "ao",
      Variant: "ao.TN.1",
      Scheduler: this.operator ?? this.addr,
      Module: module ?? "ISShJH1ij-hPPt9St5UFFr_8Ys3Kj5cyg7zrMGt7H9s",
      device: "process@1.0",
      "execution-device": "genesis-wasm@1.0",
      "push-device": "push@1.0",
      authority: this.addr,
      "random-seed": seed(16),
      Type: "Process",
    }
    const t = mergeLeft(tags, legacyTags)

    // Prepend self-trust + authority management to boot data.
    // Push device re-schedules outbox messages with from-process = process_id.
    // AOS getOwner() returns from-process when present, so trust is based on the
    // sender's process ID, not the signer. We add ao.id for self-trust and provide
    // an AddAuthority handler for cross-process trust.
    let bootData = data ?? "1984"
    if (t["On-Boot"] === "Data" && typeof bootData === "string") {
      const trustPreamble = [
        "table.insert(ao.authorities, ao.id)",
        'Handlers.add("__AddAuthority", "__AddAuthority", function(msg)',
        "  table.insert(ao.authorities, msg.Addr)",
        '  msg.reply({ Data = "ok" })',
        "end)",
      ].join("\n")
      bootData = trustPreamble + "\n" + bootData
    }

    const res = await this.post104({
      path: "/~scheduler@1.0/schedule",
      tags: t,
      data: bootData,
    })

    return {
      pid: res.out?.process,
      slot: parseInt(res.out?.slot ?? "0"),
      res: { status: 200 },
    }
  }

  async scheduleLegacy({ action = "Eval", tags = {}, ...rest } = {}) {
    // Use uppercase 'Action' to match AOS handler matching (msg.Action)
    if (action) tags.Action = action
    return await this.schedule({ tags, ...rest })
  }

  async results({ process, limit, sort = "DESC", from, to } = {}) {
    let params = ""
    const addParam = (key, val) => {
      params += params === "" ? "?" : "&"
      params += `${key}=${val}`
    }
    if (limit) addParam("limit", limit)
    if (sort) addParam("sort", sort)
    if (from) addParam("from", from)
    if (to) addParam("to", to)
    const res = await this.post({
      path: "/~relay@1.0/call",
      method: "GET",
      "relay-path": `${this.cu}/results/${process}${params}`,
      "Content-Type": "application/json",
    })
    return JSON.parse(res.body)
  }

  async dryrun({ tags = {}, pid, action, data } = {}) {
    if (typeof action === "string") tags.Action = action
    let json = { Tags: buildTags({ ...tags }), Owner: this.addr }
    if (data) json.Data = data
    const res = await this.post({
      path: "/~relay@1.0/call",
      method: "POST",
      "relay-path": `${this.cu}/dry-run?process-id=${pid}`,
      "Content-Type": "application/json",
      "relay-body": JSON.stringify(json),
    })
    return JSON.parse(res.body)
  }

  async commit(obj, opts) {
    return await commit(obj, { ...opts, signer: this.sign })
  }

  async p(path, ...args) {
    let _args = clone(args)
    _args[0] ??= {}
    _args[0].path ??= path
    return (await this.post(..._args))?.out ?? null
  }

  async post(obj, opt = {}) {
    const _json = opt.json ? "/~json@1.0/serialize" : ""
    obj.path += _json
    // Flatten nested 'body' object to top-level fields.
    // Old API used body: { key: value } for multipart POST; now these
    // fields must be at the top level for the signing pipeline.
    if (obj.body && typeof obj.body === "object" && !Buffer.isBuffer(obj.body)
        && !(obj.body instanceof Blob) && !Array.isArray(obj.body)) {
      const originalPath = obj.path
      const { body, ...rest } = obj
      obj = { ...rest, ...body }
      if (originalPath) obj.path = originalPath
    }
    if (Buffer.isBuffer(obj.body)) {
      obj.body = obj.body.toString()
    }
    if (obj["ao-body-key"] === "body") {
      delete obj["ao-body-key"]
    }
    obj.nonce ??= seed(8)

    // Check if message has nested objects/arrays (excluding metadata fields).
    // Nested values require multipart encoding for the signer to properly
    // handle them. JSON POST can't preserve nested structures through the
    // signing→verification round-trip because the structured codec changes
    // the value representation (linkification) before verification.
    const hasNested = Object.entries(obj).some(([key, value]) => {
      if (key === "path" || key === "body" || key === "commitments" || key === "ao-types") return false
      if (Array.isArray(value)) return true
      if (typeof value === "object" && value !== null
          && !Buffer.isBuffer(value) && !(value instanceof Blob)) return true
      return false
    })

    if (hasNested) {
      // Direct HTTPSig multipart POST for messages with nested objects.
      const signedMsg = await this.sign(obj)
      let response
      for (let attempt = 0; attempt < 3; attempt++) {
        try {
          response = await fetch(signedMsg.url, {
            method: signedMsg.method || "POST",
            headers: signedMsg.headers,
            body: signedMsg.body,
          })
          break
        } catch (e) {
          if (attempt === 2) throw e
          await new Promise(r => setTimeout(r, 1000 * (attempt + 1)))
        }
      }
      if (response.status >= 400) {
        const text = await response.text()
        throw new Error(`${response.status}: ${text}`)
      }
      return await result(response)
    }

    // JSON POST with commitment signatures for flat messages.
    // path: false because @path derived component causes mismatch when
    // HyperBEAM reconstructs signature base from committed field "path"
    const committed = await this.commit(obj, { path: false })
    const jsonReplacer = (key, value) => {
      if (value?.type === "Buffer" && Array.isArray(value?.data)) {
        return Buffer.from(value.data).toString("base64")
      }
      if (Buffer.isBuffer(value)) {
        return value.toString("base64")
      }
      return value
    }
    const jsonBody = JSON.stringify(committed, jsonReplacer)
    const fetchUrl = `${this.url}${obj.path}`
    const fetchOpts = {
      method: "POST",
      headers: { "content-type": "application/json", "accept-bundle": "true" },
      body: jsonBody,
    }
    let response
    for (let attempt = 0; attempt < 3; attempt++) {
      try {
        response = await fetch(fetchUrl, fetchOpts)
        break
      } catch (e) {
        if (attempt === 2) throw e
        await new Promise(r => setTimeout(r, 1000 * (attempt + 1)))
      }
    }
    if (response.status >= 400) {
      const text = await response.text()
      throw new Error(`${response.status}: ${text}`)
    }
    return await result(response)
  }

  // Decode base64-encoded multipart body from HyperBEAM responses.
  // When HyperBEAM returns cached/stored messages, the body may be
  // base64-encoded multipart form-data. This method decodes it and
  // extracts parts based on the ao-result header.
  _decodeResult(res) {
    if (!res.body || typeof res.body !== "string") return res
    const aoResult = res.headers?.["ao-result"]

    // Try to detect and decode base64-encoded multipart body
    try {
      const decoded = Buffer.from(res.body, "base64").toString("binary")
      if (decoded.startsWith("--") && decoded.includes("content-disposition")) {
        // It's multipart form-data encoded as base64
        res.body = decoded

        if (aoResult) {
          // Extract the named part from multipart
          const boundaryMatch = decoded.match(/^--([^\r\n]+)/)
          if (boundaryMatch) {
            const boundary = boundaryMatch[1]
            const parts = decoded.split(`--${boundary}`)
            for (const part of parts) {
              if (!part || part.startsWith("--")) continue
              const nameMatch = part.match(/name="([^"]+)"/)
              if (nameMatch && nameMatch[1] === aoResult) {
                const sepIdx = part.indexOf("\r\n\r\n")
                if (sepIdx !== -1) {
                  let content = part.substring(sepIdx + 4)
                  // Remove trailing CRLF
                  content = content.replace(/\r\n$/, "")
                  res.out = content
                }
                break
              }
            }
          }
        }
      }
    } catch (e) {
      // Not valid base64 or not multipart - use as-is
    }

    return res
  }

  async g(path, ...args) {
    let _args = clone(args)
    _args[0] ??= {}
    _args[0].path ??= path
    return (await this.get(..._args))?.out ?? null
  }

  async get({ path, ...params }, opt = {}) {
    const _json = opt.json ? "/~json@1.0/serialize" : ""
    path ??= "/~message@1.0"
    if (!/^\//.test(path)) path = "/" + path
    let _params = ""
    if (!isEmpty(params)) {
      let i = 0
      for (const k in params) {
        _params += `${i === 0 ? "?" : "&"}${k}=${params[k]}`
        i++
      }
    }
    // Add accept-bundle header to get inline data instead of links
    const url = `${this.url}${path}${_json}${_params}`
    let response
    for (let attempt = 0; attempt < 3; attempt++) {
      try {
        response = await fetch(url, { headers: { "accept-bundle": "true" } })
        break
      } catch (e) {
        if (attempt === 2) throw e
        await new Promise(r => setTimeout(r, 1000 * (attempt + 1)))
      }
    }
    return this._decodeResult(await result(response))
  }

  async postJSON(args, opt = {}) {
    const res = await this.post(args, { ...opt, json: true })
    return JSON.parse(res.body)
  }

  async getJSON(args, opt = {}) {
    // Use regular GET with structured output instead of json@1.0/serialize
    // because the JSON serializer doesn't resolve linkified fields (body+link)
    const res = await this.get(args, opt)
    return res.out
  }
  async spawnAOS({ image, module, sign } = {}) {
    await this.setInfo()
    const isLocal =
      sign === "httpsig" ? true
      : sign === "ans104" ? false
      : this.url.includes("localhost") || this.url.includes("127.0.0.1")

    // Default: use Arweave WASI WASM module (works on all nodes)
    // Local: wao@1.0 cache with bundled binary (faster, no network fetch)
    if (!image && !this.image) {
      module ??= srcs.module_wasi
      try {
        // Try local wao@1.0 cache first (fast path for local HB)
        await this.getImage()
      } catch (_e) {
        // Use Arweave TX ID directly — node resolves via hb_store_gateway
        image = module
      }
    }
    image ??= this.image

    const baseTags = {
      "data-protocol": "ao",
      variant: "ao.TN.1",
      image,
      "execution-device": "stack@1.0",
      "push-device": "push@1.0",
      "output-prefix": "wasm",
      "patch-from": "/results/outbox",
      "patch-mode": "patches",
      passes: 2,
      "random-seed": seed(16),
      type: "Process",
      device: "process@1.0",
      scheduler: this.operator ?? this.addr,
    }

    let res
    if (isLocal) {
      // Local: Use httpsig encoding so device-stack is committed as a typed
      // list. ANS-104 encodes device-stack as flat device-stack/N tags which
      // get aggregated into an uncommitted map by structured@1.0, then stripped
      // by with_only_committed during cache write/read — breaking second compute
      // with {error, no_valid_device_stack}. httpsig signature covers HTTP
      // header string values; ao-types transforms happen before verification
      // with re-encoding for checking, so the committed list survives cache.
      // Note: 'authority' excluded — conflicts with HTTP Signatures '@authority'
      // derived component (RFC 9421). Self-trust is injected via Lua eval below.
      res = await this.post({
        path: "/~scheduler@1.0/schedule",
        ...baseTags,
        "device-stack": [
          "wasi@1.0",
          "json-iface@1.0",
          "wasm-64@1.0",
          "patch@1.0",
          "multipass@1.0",
        ],
      })
    } else {
      // Remote: Use ANS-104 because push.forward.computer nodes don't accept
      // httpsig multipart. ANS-104 flat tags are fine for push-only nodes
      // (no compute, so no cache round-trip stripping issue).
      const tags = { ...baseTags, authority: this.addr }
      tags["device-stack/1"] = "wasi@1.0"
      tags["device-stack/2"] = "json-iface@1.0"
      tags["device-stack/3"] = "wasm-64@1.0"
      tags["device-stack/4"] = "patch@1.0"
      tags["device-stack/5"] = "multipass@1.0"
      tags["ao-types"] = 'passes="integer"'
      res = await this.post104({
        path: "/~scheduler@1.0/schedule",
        tags,
      })
    }

    const pid = res.headers?.process || res.out?.process

    if (isLocal) {
      // Compute slot 0 (the spawn itself) to initialize the process in the
      // scheduler registry. Without this, immediate scheduleAOS calls fail
      // with process_not_available because the scheduler hasn't processed
      // the spawn yet.
      try {
        await this.computeAOS({ pid, slot: 0 })
      } catch (_e) {}

      // Schedule self-trust eval so push-delivered messages from this process
      // are accepted. The 'authority' tag can't be used in httpsig spawn
      // (conflicts with HTTP Signatures @authority), so inject via Lua eval.
      try {
        await this.scheduleAOS({
          pid,
          action: "Eval",
          data: [
            "table.insert(ao.authorities, ao.id)",
            'Handlers.add("__AddAuthority", "__AddAuthority", function(msg)',
            "  table.insert(ao.authorities, msg.Addr)",
            '  msg.reply({ Data = "ok" })',
            "end)",
          ].join("\n"),
          tags: {},
        })
      } catch (_e) {}
    }

    return {
      pid,
      slot: parseInt(res.headers?.slot ?? res.out?.slot),
      res: { status: res.status },
    }
  }

  async scheduleAOS({ action = "Eval", tags = {}, ...rest }) {
    if (action) tags.Action = action
    return await this.schedule({ tags, ...rest })
  }
}

export default HB
