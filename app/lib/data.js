const ctypes = [
  {
    key: "hb",
    name: "HyperBEAM Nodes",
    desc: "WS subscriptions to remote HyperBEAM nodes",
  },
  {
    key: "su",
    name: "Scheduler Units ( SUs )",
    desc: "Connections to browser SUs via WebRTC",
  },
  {
    key: "cu",
    name: "Compute Units ( CUs )",
    desc: "Connections to browser CUs via WebRTC",
  },
  {
    key: "c",
    name: "Clients",
    desc: "Browsers subscribing to your SU/CU via WebRTC",
  },
  {
    key: "hub",
    name: "WAO Hubs",
    desc: "Websocket connections to WAO hubs",
  },
]
const hb_url = "http://localhost:10001"
const mod = "JArYBF-D8q2OmZ4Mok00sD2Y_6SYEQ7Hjx-6VZ_jl3g"

export { ctypes, hb_url, mod }
