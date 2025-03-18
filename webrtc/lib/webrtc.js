export default class WebRTC {
  constructor() {
    this.config = { iceServers: [] }

    this.peerConnection = null
    this.dataChannel = null
    this.isInitiator = false

    this.onConnectionStateChange = null
    this.onDataChannelOpen = null
    this.onDataChannelClose = null
    this.onDataChannelMessage = null
    this.onIceGatheringStateChange = null
  }

  /**
   * Initialize as the connection initiator
   * @returns {Promise<string>} Connection offer to be shared with peer
   */
  async createOffer() {
    this.isInitiator = true
    this._setupPeerConnection()

    this.dataChannel =
      this.peerConnection.createDataChannel("serverlessChannel")
    this._setupDataChannel()

    try {
      // Create the offer
      const offer = await this.peerConnection.createOffer()
      await this.peerConnection.setLocalDescription(offer)

      // Wait for ICE gathering to complete
      await this._waitForIceGathering()

      // Return the complete offer with ICE candidates
      return JSON.stringify(this.peerConnection.localDescription)
    } catch (error) {
      console.error("Error creating offer:", error)
      throw error
    }
  }

  /**
   * Generate an answer as the connection receiver
   * @param {string} offerStr - The offer string from the initiator
   * @returns {Promise<string>} Connection answer to be shared back to initiator
   */
  async createAnswer(offerStr) {
    this.isInitiator = false
    this._setupPeerConnection()

    // Set up handler for receiving the data channel
    this.peerConnection.ondatachannel = event => {
      this.dataChannel = event.channel
      this._setupDataChannel()
    }

    try {
      const offer = JSON.parse(offerStr)
      await this.peerConnection.setRemoteDescription(
        new RTCSessionDescription(offer)
      )

      // Create the answer
      const answer = await this.peerConnection.createAnswer()
      await this.peerConnection.setLocalDescription(answer)

      // Wait for ICE gathering to complete
      await this._waitForIceGathering()

      // Return the complete answer with ICE candidates
      return JSON.stringify(this.peerConnection.localDescription)
    } catch (error) {
      console.error("Error creating answer:", error)
      throw error
    }
  }

  /**
   * Set the remote answer (initiator side)
   * @param {string} answerStr - The answer string from the receiver
   * @returns {Promise<void>}
   */
  async setAnswer(answerStr) {
    if (!this.isInitiator) {
      throw new Error("setAnswer can only be called by the initiator")
    }

    try {
      const answer = JSON.parse(answerStr)
      await this.peerConnection.setRemoteDescription(
        new RTCSessionDescription(answer)
      )
    } catch (error) {
      console.error("Error setting answer:", error)
      throw error
    }
  }

  /**
   * Send a message to the connected peer
   * @param {string} message - Message to send
   * @returns {boolean} - Success status
   */
  sendMessage(message) {
    if (this.dataChannel && this.dataChannel.readyState === "open") {
      this.dataChannel.send(message)
      return true
    }
    return false
  }

  /**
   * Close the connection
   */
  close() {
    if (this.dataChannel) {
      this.dataChannel.close()
    }

    if (this.peerConnection) {
      this.peerConnection.close()
    }
  }

  /**
   * Get current connection state
   * @returns {string} Connection state
   */
  getConnectionState() {
    return this.peerConnection ? this.peerConnection.connectionState : "closed"
  }

  /**
   * Setup the peer connection
   * @private
   */
  _setupPeerConnection() {
    this.peerConnection = new RTCPeerConnection(this.config)

    this.peerConnection.onicecandidate = event => {
      console.log("ICE candidate:", event.candidate)
    }

    this.peerConnection.onconnectionstatechange = () => {
      console.log(
        "Connection state changed:",
        this.peerConnection.connectionState
      )
      if (this.onConnectionStateChange) {
        this.onConnectionStateChange(this.peerConnection.connectionState)
      }
    }

    this.peerConnection.onicegatheringstatechange = () => {
      console.log("ICE gathering state:", this.peerConnection.iceGatheringState)
      if (this.onIceGatheringStateChange) {
        this.onIceGatheringStateChange(this.peerConnection.iceGatheringState)
      }
    }
  }

  /**
   * Setup the data channel
   * @private
   */
  _setupDataChannel() {
    this.dataChannel.onopen = () => {
      console.log("Data channel open")
      if (this.onDataChannelOpen) {
        this.onDataChannelOpen()
      }
    }

    this.dataChannel.onclose = () => {
      console.log("Data channel closed")
      if (this.onDataChannelClose) {
        this.onDataChannelClose()
      }
    }

    this.dataChannel.onmessage = event => {
      console.log("Message received:", event.data)
      if (this.onDataChannelMessage) {
        this.onDataChannelMessage(event.data)
      }
    }
  }

  /**
   * Wait for ICE gathering to complete
   * @returns {Promise<void>}
   * @private
   */
  _waitForIceGathering() {
    return new Promise(resolve => {
      if (this.peerConnection.iceGatheringState === "complete") {
        resolve()
      } else {
        const checkState = () => {
          if (this.peerConnection.iceGatheringState === "complete") {
            this.peerConnection.removeEventListener(
              "icegatheringstatechange",
              checkState
            )
            resolve()
          }
        }
        this.peerConnection.addEventListener(
          "icegatheringstatechange",
          checkState
        )
      }
    })
  }
}
