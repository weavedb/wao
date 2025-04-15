const ctypes = [
  {
    key: "hb",
    name: "HyperBEAM Nodes",
    desc: "Websocket subscriptions to remote HyperBEAM nodes",
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
    desc: "Other browsers subscribing to your SU & CU via WebRTC",
  },
  {
    key: "hub",
    name: "WAO Hubs",
    desc: "Websocket connections to WAO hubs",
  },
]
const hb_url = "http://localhost:10001"
export { ctypes, hb_url }
