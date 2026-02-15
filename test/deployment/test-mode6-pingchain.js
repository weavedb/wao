import { HyperBEAM } from "../../src/test.js"
import AO from "../../src/ao.js"

const MODULE = "AZ6EXxyOaw3ZjK6htaKA6qZaIh_AbJoJnskGtj7FKZ0"

const hbeam = await new HyperBEAM({ reset: true, genesis_wasm: true }).ready()
console.log("HyperBEAM ready at", hbeam.url)

const ao = new AO({ hb: hbeam.url, mode: "aos" })
await ao.init(hbeam.jwk)

console.log("\n=== PingChain: Multi-hop receive() ===")
try {
  const { pid: pidA } = await ao.spwn({})
  console.log("A:", pidA)
  await new Promise(r => setTimeout(r, 1000))
  const { pid: pidB } = await ao.spwn({})
  console.log("B:", pidB)

  // B: receives Ping, replies with Pong
  await ao.msg({
    pid: pidB,
    act: "Eval",
    data: `Handlers.add("Ping","Ping",function(msg)
  msg.reply({ Action = "Pong", Data = "pong:" .. (msg.Data or "nil") })
end)`,
  })

  // A: sends Ping to B, waits for Pong via Receive()
  await ao.msg({
    pid: pidA,
    act: "Eval",
    data: `
TARGET_B = "${pidB}"
Handlers.add("PingChain","PingChain",function(msg)
  ao.send({ Target = TARGET_B, Action = "Ping", Data = "hello" })
  local reply = Receive({ From = TARGET_B })
  msg.reply({ Data = "chain:" .. (reply.Data or "nil") })
end)`,
  })

  // Mutual trust
  await ao.msg({ pid: pidB, act: "Eval", data: `table.insert(ao.authorities, "${pidA}")` })
  await ao.msg({ pid: pidA, act: "Eval", data: `table.insert(ao.authorities, "${pidB}")` })
  console.log("Setup complete")

  // Fire PingChain
  console.log("Firing PingChain...")
  const { res, err } = await ao.msg({ pid: pidA, act: "PingChain", data: "start" })
  console.log("err:", err)
  console.log("res:", JSON.stringify(res)?.slice(0, 500))

  const data = res?.Messages?.[0]?.Data
  console.log("Data:", data)

  if (data === "chain:pong:hello") {
    console.log("PINGCHAIN: PASS")
  } else if (String(JSON.stringify(res)).includes("chain:pong:hello")) {
    console.log("PINGCHAIN: PASS (in result)")
  } else {
    console.log("PINGCHAIN: FAIL")
  }
} catch (e) {
  console.log("PINGCHAIN: FAIL -", e.message?.slice(0, 300))
}

hbeam.kill()
