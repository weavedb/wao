const express = require("express")
const cors = require("cors")
const bodyParser = require("body-parser")
const app2 = express()
const { generateId, toANS104Request, parseSignatureInput } = require("./utils")

const cu = (sus, cbs) => {
  app2.use(cors())
  app2.use(bodyParser.json())
  app2.post("/result/:mid", result)
  app2.get("/result/:mid", result)

  app2.post("/dry-run", async (req, res) => {
    const process = req.query["process-id"]
    const { Id: id, Owner: owner, Tags: tags, Data: data } = req.body
    dryrun({ id, owner, tags, data, process }, res)
  })

  const server2 = app2.listen(4004, () => console.log(`CU on port 4004`))

  // todo: request doesn't contain any info about hb node, how to handle it?
  async function result(req, res) {
    const id = generateId()
    cbs[id] = res
    let message = req.params.mid
    let exists = false
    const process = req.query["process-id"]
    for (let k in sus) {
      const socket = sus[k].socket
      exists = true
      socket.send(
        JSON.stringify({ type: "msg", subtype: "result", message, process, id })
      )
    }
    if (!exists) {
      res.status(404).send("success")
      delete cbs[id]
    } else {
      setTimeout(() => {
        if (cbs[id]) {
          res.status(404).send("success")
          delete cbs[id]
        }
      }, 10000)
    }
  }

  // todo: request doesn't contain any info about hb node, how to handle it?
  async function dryrun(opt, res) {
    const id = generateId()
    cbs[id] = res
    let exists = false
    for (let k in sus) {
      const socket = sus[k].socket
      exists = true
      socket.send(
        JSON.stringify({ type: "msg", subtype: "dryrun", message: opt, id })
      )
    }
    if (!exists) {
      res.status(404).send("success")
      delete cbs[id]
    } else {
      setTimeout(() => {
        if (cbs[id]) {
          res.status(404).send("success")
          delete cbs[id]
        }
      }, 3000)
    }
  }
}

module.exports = cu
