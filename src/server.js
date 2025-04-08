import express from "express"
import cors from "cors"
import { optAO } from "./utils.js"
import { connect } from "./aoconnect.js"
import bodyParser from "body-parser"
import Adaptor from "./adaptor.js"

class Server {
  constructor({
    ar = 4000,
    bd = 4001,
    mu = 4002,
    su = 4003,
    cu = 4004,
    hb_url,
    aoconnect,
    log = false,
    db,
    port,
  } = {}) {
    if (port) {
      ar = port
      bd = port + 1
      mu = port + 2
      su = port + 3
      cu = port + 4
      aoconnect = optAO(5000)
    }
    const { mem } = connect(aoconnect, { log, cache: db })
    this.adaptor = new Adaptor({ hb_url, aoconnect: mem, log, db })
    this.ports = { ar, mu, su, cu, bd }
    this.servers = []
    this.ar()
    this.bd()
    this.mu()
    this.su()
    this.cu()
  }

  ar() {
    const app = express()
    app.use(bodyParser.json({ limit: "100mb" }))
    this.launch("AR", app, {
      get: [
        "/",
        "/wallet/:id/balance",
        "/mint/:id/:amount",
        "/tx/:id/offset",
        "/tx_anchor",
        "/mine",
        "/:id",
        "/price/:id",
      ],
      post: ["/graphql", "/:id"],
    })
  }

  bd() {
    const app = express()
    app.use("/tx", bodyParser.raw({ type: "*/*", limit: "100mb" }))
    this.launch("BD", app, { post: ["/tx"] })
  }

  mu() {
    const app = express()
    app.use(express.raw({ type: "*/*" }))
    this.launch("MU", app, {
      get: ["/"],
      post: ["/", "/monitor/:pid"],
      delete: ["/monitor/:pid"],
    })
  }
  su() {
    const app = express()
    app.use(bodyParser.json())
    this.launch("SU", app, { get: ["/", "/timestamp", "/:pid"] })
  }
  cu() {
    const app = express()
    app.use(bodyParser.json())
    this.launch("CU", app, {
      get: ["/", "/result/:mid", "/state/:pid", "/results/:pid"],
      post: ["/result/:mid", "/dry-run"],
    })
  }

  send(res, data) {
    if (data.status) res.status(data.status)
    if (data.error) {
      res.json({ error: data.error })
    } else if (data.json) {
      res.json(data.json)
    } else {
      res.send(data.send)
    }
  }
  req(req) {
    return {
      body: req.body,
      headers: req.headers,
      method: req.method,
      params: req.params,
      query: req.query,
    }
  }

  launch(name, app, paths) {
    app.use(cors())
    const port = this.ports[name.toLowerCase()]
    for (const method in paths) {
      for (const path of paths[method]) {
        app[method](path, async (req, res) =>
          this.send(
            res,
            await this.adaptor.get({
              type: name.toLowerCase(),
              path,
              req: this.req(req),
            })
          )
        )
      }
    }
    this.servers.push(
      app.listen(port, () => console.log(`${name} on port ${port}`))
    )
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
