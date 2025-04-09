const yargs = require("yargs")
let { port = 7000 } = yargs(process.argv.slice(2)).argv
const { Server } = require("wao/test")
const { generateId } = require("./utils")
const WebSocket = require("ws")
const ws_server = new WebSocket.Server({ port: 7070 })
const { keys, omit, isNil, mergeLeft } = require("ramda")
const { resolve } = require("path")
const { writeFileSync, readFileSync } = require("fs")

let sus = {}
let cbs = {}
let _socket = null

ws_server.on("connection", socket => {
  _socket = socket
  const clientId = generateId()
  console.log("new ws:", clientId)
  sus[clientId] = {
    socket,
    su: false,
    connections: {},
    address: null,
  }
  socket.send(JSON.stringify({ type: "registered", id: clientId }))
  socket.on("message", message => {
    const data = JSON.parse(message)
    if (data.type === "msg") {
      if (cbs[data.id]) {
        cbs[data.id](data.res)
        delete cbs[data.id]
      }
    }
  })

  socket.on("close", () => {
    console.log("ws disconnected", clientId)
    delete sus[clientId]
  })
})

console.log("WAO FS running on port 7070")
class Adaptor {
  constructor({ hb_url, aoconnect, log = false, db } = {}) {}
  async get(req, res) {
    if (_socket) {
      const id = generateId()
      cbs[id] = res
      _socket.send(JSON.stringify({ type: "msg", req, id }))
    } else {
      return { status: 404, error: "connection not found" }
    }
  }
}

const main = async () => new Server({ log: true, port, adaptor: new Adaptor() })

main()
