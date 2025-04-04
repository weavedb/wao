export default class Hub {
  constructor(serverUrl) {
    this.serverUrl = serverUrl
    this.socket = null
    this.clientId = null

    this.onOpen = null
    this.onRegister = null
    this.onSUs = null
    this.onOffer = null
    this.onAnswer = null
    this.onClose = null
    this.onError = null
  }

  connect() {
    this.socket = new WebSocket(this.serverUrl)

    this.socket.onopen = () => {
      console.log("Connected to signaling server")
      if (this.onOpen) this.onOpen()
    }
    this.socket.onmessage = event => {
      const message = JSON.parse(event.data)
      switch (message.type) {
        case "subscribe":
          if (this.onSubscribe) this.onSubscribe(message)
          break

        case "list":
          if (this.onList) this.onList(message)
          break
        case "msg":
          if (this.onMsg) this.onMsg(message)
          break
        case "registered":
          this.clientId = message.id
          console.log("Received client ID:", this.clientId)
          if (this.onRegister) this.onRegister(this.clientId)
          break
        case "sus":
          if (this.onSUs) this.onSUs(message.ids)
          break
        case "offer":
          if (this.onOffer) this.onOffer(message.offer, message.id)
          break
        case "answer":
          if (this.onAnswer) this.onAnswer(message.answer, message.id)
          break
      }
    }

    this.socket.onclose = event => {
      console.log(
        "Disconnected from signaling server:",
        event.code,
        event.reason
      )
      if (this.onClose) this.onClose(event)
    }

    this.socket.onerror = error => {
      console.error("WebSocket error:", error)
      if (this.onError) this.onError(error)
    }
  }

  registerSU() {
    if (!this.isConnected()) return false
    this.socket.send(JSON.stringify({ type: "register" }))

    return true
  }

  getSUs() {
    if (!this.isConnected()) return false
    this.socket.send(JSON.stringify({ type: "sus" }))

    return true
  }

  sendOffer(offer, su) {
    if (!this.isConnected()) return false
    this.socket.send(JSON.stringify({ type: "offer", offer, su }))

    return true
  }

  sendAnswer(answer, client) {
    if (!this.isConnected()) return false
    this.socket.send(JSON.stringify({ type: "answer", answer, client }))
    return true
  }

  isConnected() {
    if (!this.socket || this.socket.readyState !== WebSocket.OPEN) {
      console.error("Socket is not connected")
      return false
    }
    return true
  }

  disconnect() {
    if (this.socket) {
      this.socket.close()
      this.socket = null
    }
  }
}
