import { useState } from "react"

export default function App() {
  const [connected, setConnected] = useState(false)
  const [addr, setAddr] = useState(null)

  async function connectWallet() {
    await window.arweaveWallet.connect(["ACCESS_ADDRESS", "SIGN_TRANSACTION"])
    const address = await window.arweaveWallet.getActiveAddress()
    setAddr(address)
    setConnected(true)
  }

  return (
    <div style={{ padding: "2rem", fontFamily: "monospace" }}>
      <h1>WAO App</h1>
      {!connected ? (
        <button onClick={connectWallet}>Connect Wallet</button>
      ) : (
        <p>Connected: {addr}</p>
      )}
    </div>
  )
}
