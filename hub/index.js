const express = require("express")
const cors = require("cors")
const bodyParser = require("body-parser")
const { keys, omit, isNil } = require("ramda")
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
  /*
  const verifier = createVerifier(
    createPublicKey({ key, format: "jwk" }),
    "rsa-pss-sha512"
  )*/
  let id = null
  try {
    /*const isValid = await verifyMessage(
      { keyLookup: params => ({ verify: verifier }) },
      {
        method: req.method,
        headers: req.headers,
        url: `http://ao.com${req.headers.path}`,
      }
    )*/
    const item = toANS104Request(sigs).item
    if (sigs.slot === "0" || sigs.type === "Process") {
      for (let v of item.tags) if (v.name === "Type") v.value = "Process"
      await message(
        {
          http_msg: item,
          module: sigs.module,
          scheduler: sigs.scheduler,
        },
        res
      )
    } else if (sigs.type === "Message") {
      for (let v of item.tags) if (v.name === "id") item.target = v.value
      await message(
        {
          slot: sigs.slot,
          http_msg: item,
          process: sigs.target,
        },
        res
      )
    } else {
      return res.status(500).send("unknown error")
    }
  } catch (e) {
    console.log(e, req.originalUrl)
    return res.status(500).send("unknown error")
  }
})
const server = app.listen(4001, () => console.log(`BD on port 4001`))

const app2 = express()
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

const WebSocket = require("ws")
const ws_server = new WebSocket.Server({ port: 8080 })
const sus = {}
const cbs = {}
ws_server.on("connection", socket => {
  const clientId = generateId()
  sus[clientId] = { socket, su: false, connections: {} }
  socket.send(JSON.stringify({ type: "registered", id: clientId }))
  socket.on("message", message => {
    const data = JSON.parse(message)
    if (data.type === "msg") {
      cbs[data.id].status(data.status).send(data.msg)
      delete cbs[data.id]
    } else if (data.type === "register") {
      sus[clientId].su = true
    } else if (data.type === "sus") {
      let keys = []
      for (let k in sus) {
        if (sus[k].su) keys.push(k)
      }
      socket.send(JSON.stringify({ type: "sus", ids: keys }))
    } else if (data.type === "offer") {
      sus[data.su].socket.send(
        JSON.stringify({ type: "offer", id: clientId, offer: data.offer })
      )
    } else if (data.type === "answer") {
      sus[data.client].socket.send(
        JSON.stringify({
          type: "answer",
          id: data.clientId,
          answer: data.answer,
        })
      )
    }
  })

  socket.on("close", () => {
    console.log("su disconnected", clientId)
    delete sus[clientId]
  })
})

function generateId() {
  return Math.random().toString(36).substring(2, 15)
}

console.log("WAO hub running on port 8080")

async function message(opt, res) {
  for (let k in sus) {
    const socket = sus[k].socket
    const id = generateId()
    cbs[id] = res
    socket.send(JSON.stringify({ type: "msg", id, message: opt }))
    break
  }
}

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
