import express from "express"
import cors from "cors"
import base64url from "base64url"
import { DataItem } from "arbundles"
import { tags, toGraphObj, optAO } from "./utils.js"
import { connect } from "./aoconnect.js"
import { GQL, cu, su, mu } from "./test.js"
import bodyParser from "body-parser"
import { keys, map, isNil, reverse } from "ramda"

class Server {
  constructor({
    ar = 4000,
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
    this.ports = { ar, mu, su, cu }
    this.servers = []
    this.ar()
    this.mu()
    this.su()
    this.cu()
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
      }),
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
          console.log("or here...", req.body)
          await this._ar.postTx(req.body)
        }
        res.json({ id: req.body.id })
      }
    })
    app.get("*", async (req, res) => {
      console.log("what")
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
      console.log(`MU on port ${this.ports.mu}`),
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
      res.json({ timestamp: Date.now(), block_height: this.mem.height }),
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
    app.get("*", async (req, res) => {
      console.log("what2")
    })

    const server = app.listen(this.ports.su, () =>
      console.log(`SU on port ${this.ports.su}`),
    )
    this.servers.push(server)
  }
  cu() {
    const app = express()
    app.use(cors())
    app.use(bodyParser.json())
    app.get("/", (req, res) =>
      res.json({ timestamp: Date.now(), address: cu.addr }),
    )
    app.get("/result/:mid", async (req, res) => {
      const res2 = await this.result({
        message: req.params.mid,
        process: req.query["process-id"],
      })
      res.json(res2)
    })
    app.get("/state/:pid", async (req, res) => {
      const pid = req.params.pid
      const memory = (await this.mem.env[pid]?.memory) ?? null
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
      console.log(`CU on port ${this.ports.cu}`),
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
