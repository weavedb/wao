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
    storage,
    port,
    adaptor,
    units,
  } = {}) {
    if (port) {
      ar = port
      bd = port + 1
      mu = port + 2
      su = port + 3
      cu = port + 4
      aoconnect = optAO(5000)
    }
    // Parse enabled units (default: all)
    const enabled = units
      ? new Set(units.map(u => u.toLowerCase()))
      : new Set(["ar", "bd", "mu", "su", "cu"])
    if (enabled.has("ar")) enabled.add("bd")
    this._enabledUnits = enabled

    if (!aoconnect) {
      const { mem } = connect(aoconnect, { log, cache: db, storage })
      aoconnect = mem
    }
    this.adaptor = adaptor ?? new Adaptor({ hb_url, aoconnect, log, db, storage })
    this.ports = { ar, mu, su, cu, bd }
    this.servers = []
    if (enabled.has("ar")) this.ar()
    if (enabled.has("bd")) this.bd()
    if (enabled.has("mu")) this.mu()
    if (enabled.has("su")) this.su()
    if (enabled.has("cu")) this.cu()
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
      get: ["/", "/result/:mid", "/state/:pid", "/results/:pid", "/dry-run"],
      post: ["/result/:mid", "/dry-run"],
    })
  }

  res(res) {
    return data => {
      if (data.status) res.status(data.status)
      if (data.headers) {
        for (const [k, v] of Object.entries(data.headers)) res.set(k, v)
      }
      if (data.error) {
        res.json({ error: data.error })
      } else if (data.json) {
        res.json(data.json)
      } else {
        res.send(data.send)
      }
    }
  }

  req(req, device, path) {
    return {
      path,
      device,
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
        app[method](
          path,
          async (req, res) =>
            await this.adaptor.get(
              this.req(req, name.toLowerCase(), path),
              this.res(res)
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
      if (this.servers.length === 0) return res()
      let count = 0
      for (const v of this.servers) {
        // close() waits for active connections to drain; force-close any
        // hanging keepalive sockets so the test process can exit when a
        // dependent HyperBEAM instance dies mid-request.
        if (typeof v.closeAllConnections === "function") {
          try { v.closeAllConnections() } catch (_e) {}
        }
        v.close(() => {
          count += 1
          if (count >= this.servers.length) {
            console.log("servers closed!")
            res()
          }
        })
      }
    })
  }
}

export default Server
