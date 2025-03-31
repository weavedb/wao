import crypto from "crypto"
import express from "express"
import cors from "cors"
import base64url from "base64url"
import { DataItem } from "arbundles"
import { tags, toGraphObj, optAO } from "./utils.js"
import { connect } from "./aoconnect.js"
import { GQL, cu, su, mu } from "./test.js"
import bodyParser from "body-parser"
import { keys, map, isNil, reverse, omit } from "ramda"
import { Bundle } from "arbundles"
import { httpbis, createVerifier } from "http-message-signatures"
import { createPublicKey, randomBytes } from "node:crypto"
const { verifyMessage } = httpbis

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

class Server {
  constructor({
    ar = 4000,
    bundler = 4001,
    mu = 4002,
    su = 4003,
    cu = 4004,
    aoconnect,
    log = false,
    db,
    port,
  } = {}) {
    if (port) {
      ar = port
      bundler = port + 1
      mu = port + 2
      su = port + 3
      cu = port + 4
      aoconnect = optAO(5000)
    }
    const {
      ar: _ar,
      message,
      spawn,
      dryrun,
      result,
      results,
      mem,
      monitor,
      unmonitor,
    } = connect(aoconnect, { log, cache: db })
    this.monitor = monitor
    this.unmonitor = unmonitor
    this.spawn = spawn
    this._ar = _ar
    this.message = message
    this.dryrun = dryrun
    this.result = result
    this.results = results
    this.mem = mem
    this.gql = new GQL({ mem })
    this.ports = { ar, mu, su, cu, bundler }
    this.servers = []
    this.ar()
    this.bundler()
    this.mu()
    this.su()
    this.cu()
  }

  bundler() {
    const app = express()
    app.use(cors())
    app.use("/tx", bodyParser.raw({ type: "*/*", limit: "100mb" }))
    app.post("/tx", async (req, res) => {
      if (!Buffer.isBuffer(req.body)) {
        console.log("BD: Invalid body | expected raw Buffer")
        return res.status(400).send("Invalid body: expected raw Buffer")
      }
      const lines = req.body.toString("utf8").split(/\r?\n/)
      const sigs = {}
      let currentKey = null

      for (let line of lines) {
        const trimmed = line.trim()
        if (/^--[a-zA-Z0-9_\-=]+/.test(trimmed)) {
          currentKey = null
          continue
        }
        const headerMatch = trimmed.match(/^([a-zA-Z0-9_-]+):\s*(.*)$/)
        if (headerMatch && !headerMatch[2].includes(": ")) {
          const key = headerMatch[1]
          const value = headerMatch[2]
          sigs[key] = value
          currentKey = key
        } else if (currentKey) sigs[currentKey] += "\n" + line
      }
      sigs.target = req.headers.process
      sigs.slot = req.headers.slot

      const input = parseSignatureInput(req.headers["signature-input"])
      const key = { kty: "RSA", n: input.keyid, e: "AQAB" }
      const verifier = createVerifier(
        createPublicKey({ key, format: "jwk" }),
        "rsa-pss-sha512"
      )
      try {
        const isValid = await verifyMessage(
          { keyLookup: params => ({ verify: verifier }) },
          {
            method: req.method,
            headers: req.headers,
            url: `http://ao.com${req.headers.path}`,
          }
        )
        const item = toANS104Request(sigs).item
        if (sigs.slot === "0" || sigs.type === "Process") {
          for (let v of item.tags) if (v.name === "Type") v.value = "Process"
          const res = await this.spawn({
            http_msg: item,
            module: sigs.module,
            scheduler: sigs.scheduler,
          })
        } else if (sigs.type === "Message") {
          const res = await this.message({
            http_msg: item,
            process: sigs.target,
          })
        }
      } catch (e) {
        console.log(e, req.originalUrl)
      }
      return res.status(200).send("Success")
    })
    const server = app.listen(this.ports.bundler, () => {
      console.log(`BD on port ${this.ports.bundler}`)
    })
    this.servers.push(server)
  }

  ar() {
    const app = express()
    app.use(cors())
    app.use(bodyParser.json({ limit: "100mb" }))
    app.get("/", (req, res) =>
      res.json({
        version: 1,
        timestamp: Date.now(),
        height: this.mem.height,
        network: "wao.LN.1",
        current: this.mem.getAnchor(),
      })
    )
    app.get("/wallet/:id/balance", (req, res) => res.send("0"))
    app.get("/mint/:id/:amount", (req, res) => res.json({ id: "0" }))
    app.get("/tx/:id/offset", async (req, res) => {
      res.status(400)
      res.send(null)
    })
    app.get("/tx_anchor", async (req, res) => {
      res.send(this.mem.getAnchor())
    })
    app.get("/mine", async (req, res) => {
      res.json(req.params)
    })
    app.get("/:id", async (req, res) => {
      const _data = await this._ar.data(req.params.id)
      if (!_data) {
        res.status(404)
        res.send(null)
      } else {
        res.send(Buffer.from(_data, "base64"))
      }
    })
    app.get("/price/:id", async (req, res) => {
      res.send("0")
    })
    app.post("/graphql", async (req, res) => {
      try {
        const { query, variables } = req.body
        const { tar, args } = toGraphObj({ query, variables })
        let res2 = null
        if (tar === "transactions") {
          res2 = await this.gql.txs({ ...args })
        } else if (tar === "blocks") {
          res2 = await this.gql.blocks({ ...args })
        }
        const edges = map(v => ({ node: v, cursor: v.cursor }), res2)
        res.json({
          data: { transactions: { pageInfo: { hasNextPage: true }, edges } },
        })
      } catch (e) {
        console.log(e)
        res.status(400)
        res.json({ error: "bad request" })
      }
    })
    let data = {}
    app.post("/:id", async (req, res) => {
      // id = "tx" | "chunk"
      if (req.body.chunk) {
        if (data[req.body.data_root]) {
          data[req.body.data_root].data += req.body.chunk
          const buf = Buffer.from(req.body.chunk, "base64")
          if (!data[req.body.data_root].chunks) {
            data[req.body.data_root].chunks = buf
          } else {
            data[req.body.data_root].chunks = Buffer.concat([
              data[req.body.data_root].chunks,
              buf,
            ])
          }
          delete req.body.chunk
          if (
            data[req.body.data_root].data_size <=
            data[req.body.data_root].chunks.length
          ) {
            data[req.body.data_root].data =
              data[req.body.data_root].chunks.toString("base64")
            await this._ar.postTx(data[req.body.data_root])
            delete data[req.body.data_root]
          }
        }
        res.json({ id: req.body.id })
      } else {
        if (req.body.data_root && req.body.data === "") {
          data[req.body.data_root] = req.body
        } else {
          await this._ar.postTx(req.body)
        }
        res.json({ id: req.body.id })
      }
    })
    const server = app.listen(this.ports.ar, () => {
      console.log(`AR on port ${this.ports.ar}`)
    })
    this.servers.push(server)
  }
  mu() {
    const app = express()
    app.use(cors())
    app.use(express.raw({ type: "*/*" }))
    app.get("/", (req, res) => res.send("ao messenger unit"))
    app.post("/monitor/:id", async (req, res) => {
      await this.monitor({ process: req.params.id })
      res.json({ id: req.params.id, messag: "cron monitored!" })
    })
    app.delete("/monitor/:id", async (req, res) => {
      await this.unmonitor({ process: req.params.id })
      res.json({ id: req.params.id, message: "cron deleted!" })
    })
    app.post("/", async (req, res) => {
      const binary = req.body
      let valid = await DataItem.verify(binary)
      let type = null
      let item = null
      if (valid) item = new DataItem(binary)
      const _tags = tags(item.tags)
      let err = null
      if (_tags.Type === "Process") {
        const res = await this.spawn({
          item,
          module: _tags.Module,
          scheduler: _tags.Scheduler,
        })
        if (!res) err = "bad requrest"
      } else if (_tags.Type === "Message") {
        await this.message({ item, process: item.target })
      } else {
        err = true
      }
      if (err) {
        res.status(400)
        res.send({ err })
      } else {
        res.send({ id: item.id })
      }
    })
    const server = app.listen(this.ports.mu, () =>
      console.log(`MU on port ${this.ports.mu}`)
    )
    this.servers.push(server)
  }
  su() {
    const app = express()
    app.use(cors())
    app.use(bodyParser.json())
    app.get("/", (req, res) => {
      res.json({
        Unit: "Scheduler",
        Timestamp: Date.now(),
        Address: su.addr,
        Processes: keys(this.mem.env),
      })
    })
    app.get("/timestamp", (req, res) =>
      res.json({ timestamp: Date.now(), block_height: this.mem.height })
    )
    app.get("/:pid", (req, res) => {
      const pid = req.params.pid
      const edges = map(async v => {
        const tx = await this.mem.getTx(v)
        const _tags = tags(v.tags)
        const mid = _tags.Message
        const mtx = await this.mem.getTx(mid)
        return {
          cursor: v,
          node: {
            message: {
              id: mtx.id,
              tags: mtx.tags,
              owner: this.mem.addrmap[mtx.owner],
              anchor: mtx.anchor ?? null,
              target: pid,
              signature: mtx.signature,
              data: tx.data,
            },
            assignment: {
              id: tx.id,
              tags: tx.tags,
              owner: this.mem.addrmap[tx.owner],
              anchor: tx.anchor ?? null,
              target: null,
              signature: tx.signature,
            },
          },
        }
      })(reverse(this.mem.env[pid]?.results ?? [])) // need mod
      res.json({ page_info: { has_next_page: false }, edges })
    })
    const server = app.listen(this.ports.su, () =>
      console.log(`SU on port ${this.ports.su}`)
    )
    this.servers.push(server)
  }
  cu() {
    const app = express()
    app.use(cors())
    app.use(bodyParser.json())
    app.get("/", (req, res) =>
      res.json({ timestamp: Date.now(), address: cu.addr })
    )
    app.get("/result/:mid", async (req, res) => {
      let message = req.params.mid
      const process = req.query["process-id"]
      if (!/^--[0-9a-zA-Z_-]{43,44}$/.test(message)) {
        message = this.mem.env[process].results[message]
      }
      const res2 = await this.result({
        message,
        process,
      })
      res.json(res2)
    })
    app.get("/state/:pid", async (req, res) => {
      const pid = req.params.pid
      const memory = this.mem.env[pid]?.memory ?? null
      if (!memory) {
        res.status(404)
        res.json({
          error: `TransactionNotFound: Process ${pid} was not found on gateway`,
        })
      } else {
        res.send(Buffer.from(memory))
      }
    })
    app.post("/dry-run", async (req, res) => {
      const process = req.query["process-id"]
      const { Id: id, Owner: owner, Tags: tags, Data: data } = req.body
      const res2 = await this.dryrun({ id, owner, tags, data, process })
      if (!res2) {
        res.status(400)
        res.json({ err: true })
      } else {
        delete res2.Memory
        res.json(res2)
      }
    })
    app.get("/results/:pid", async (req, res) => {
      const pid = req.params.pid
      const { from = null, to = null, sort = "ASC", limit = 25 } = req.query
      let results = this.mem.env[pid]?.results ?? []
      if (sort === "DESC") results = reverse(results)
      let _res = []
      let i = 1
      let count = 0
      let started = isNil(from)
      for (let v of results) {
        if (started) {
          _res.push({ cursor: v, node: this.mem.msgs[v]?.res })
          count++
          if (!isNil(to) && v === to) break
          if (limit <= count) break
        } else if (from === v) started = true

        i++
      }
      res.json({ edges: _res })
    })

    const server = app.listen(this.ports.cu, () =>
      console.log(`CU on port ${this.ports.cu}`)
    )
    this.servers.push(server)
  }
  end() {
    return new Promise(res => {
      let count = 0
      for (const v of this.servers)
        v.close(() => {
          count += 1
          if (count >= 4) {
            console.log("servers closed!")
            res()
          }
        })
    })
  }
}

export default Server
