import express from "express"
import cors from "cors"
import bodyParser from "body-parser"
import { keys, omit, isNil, mergeLeft } from "ramda"
import { generateId, toANS104Request, parseSignatureInput } from "./utils.js"
import bundler from "./bundler.js"
import cu from "./cu.js"
import yargs from "yargs"
let { port = 8080 } = yargs(process.argv.slice(2)).argv
import WebSocket from "ws"
const ws_server = new WebSocket.Server({ port })

let sus = {}
let cbs = {}
let hbs = {
  O74OhzD1O_zE0uaKbaTQD1rfgTZEGMeXQ6M6M60TW_o: { update: Date.now() },
}

bundler(sus, cbs, hbs)
cu(sus, cbs, hbs)

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
      hb: {},
    },
  }
  socket.send(JSON.stringify({ type: "registered", id: clientId }))
  socket.on("message", message => {
    const data = JSON.parse(message)
    if (data.type === "list") {
      if (data.target === "hb") {
        let hb = []
        for (const k in hbs) {
          hb.push({ address: k, update: hbs[k].update })
        }
        socket.send(JSON.stringify({ hb, type: "list" }))
      } else {
        socket.send(JSON.stringify({ error: "unknown target" }))
      }
    } else if (data.type === "subscribe") {
      sus[clientId].accept = mergeLeft(data.accept, sus[clientId].accept)
      socket.send(
        JSON.stringify({ type: "subscribe", accept: sus[clientId].accept })
      )
    } else if (data.type === "msg") {
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
    console.log("ws disconnected", clientId)
    delete sus[clientId]
  })
})

console.log("WAO hub running on port 8080")
