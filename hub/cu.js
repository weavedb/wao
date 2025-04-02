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

  async function result(req, res) {
    let message = req.params.mid
    const process = req.query["process-id"]
    for (let k in sus) {
      const socket = sus[k].socket
      const id = generateId()
      cbs[id] = res
      socket.send(
        JSON.stringify({ type: "msg", subtype: "result", message, process, id })
      )
      break
    }
  }

  async function dryrun(opt, res) {
    for (let k in sus) {
      const socket = sus[k].socket
      const id = generateId()
      cbs[id] = res
      socket.send(
        JSON.stringify({ type: "msg", subtype: "dryrun", message: opt, id })
      )
      break
    }
  }
}

module.exports = cu
