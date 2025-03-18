const WebSocket = require("ws")
const server = new WebSocket.Server({ port: 8080 })

const sus = {}

server.on("connection", socket => {
  const clientId = generateId()
  sus[clientId] = { socket, su: false, connections: {} }
  socket.send(JSON.stringify({ type: "registered", id: clientId }))
  socket.on("message", message => {
    const data = JSON.parse(message)
    if (data.type === "register") {
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
