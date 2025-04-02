const express = require("express")
const cors = require("cors")
const bodyParser = require("body-parser")
const { keys, omit, isNil } = require("ramda")
const { generateId, toANS104Request, parseSignatureInput } = require("./utils")
const bundler = require("./bundler")
const cu = require("./cu")

const WebSocket = require("ws")
const ws_server = new WebSocket.Server({ port: 8080 })

let sus = {}
let cbs = {}
bundler(sus, cbs)
cu(sus, cbs)

ws_server.on("connection", socket => {
  const clientId = generateId()
  console.log("new ws:", clientId)
  sus[clientId] = {
    socket,
    su: false,
    connections: {},
    address: null,
    accept: {
      pid: {},
      hb: { O74OhzD1O_zE0uaKbaTQD1rfgTZEGMeXQ6M6M60TW_o: { "*": true } },
    },
  }
  socket.send(JSON.stringify({ type: "registered", id: clientId }))
  socket.on("message", message => {
    const data = JSON.parse(message)
    if (data.type === "msg") {
      if (cbs[data.id]) {
        cbs[data.id].status(data.status).send(data.msg)
        delete cbs[data.id]
      }
    } else if (data.type === "register") {
      sus[clientId].su = true
    } else if (data.type === "sus") {
      let keys = []
      for (let k in sus) if (sus[k].su) keys.push(k)
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

console.log("WAO hub running on port 8080")
