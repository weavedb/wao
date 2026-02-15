import { HyperBEAM } from "../../src/test.js"
import AO from "../../src/ao.js"

const MODULE = "AZ6EXxyOaw3ZjK6htaKA6qZaIh_AbJoJnskGtj7FKZ0"

const hbeam = await new HyperBEAM({ reset: true, genesis_wasm: true }).ready()
console.log("HyperBEAM ready at", hbeam.url)

const ao = new AO({ hb: hbeam.url, mode: "aos" })
await ao.init(hbeam.jwk)

console.log("\n=== Test 1: Simple eval (Arweave module) ===")
try {
  const { pid } = await ao.spwn({ module: MODULE })
  console.log("pid:", pid)
  const { res } = await ao.msg({ pid, act: "Eval", data: "return 1+1" })
  console.log("Result:", JSON.stringify(res)?.slice(0, 200))
  console.log("TEST 1: PASS")
} catch (e) {
  console.log("TEST 1: FAIL -", e.message?.slice(0, 300))
}

console.log("\n=== Test 2: Cross-process receive() via ao.msg() ===")
try {
  const { pid: pidA } = await ao.spwn({ module: MODULE })
  const { pid: pidB } = await ao.spwn({ module: MODULE })
  console.log("A:", pidA)
  console.log("B:", pidB)

  // Load Echo handler on B
  await ao.msg({
    pid: pidB,
    act: "Eval",
    data: `Handlers.add("Echo", "Echo", function(msg)
  msg.reply({ Data = "echo:" .. (msg.Data or "nil") })
end)`,
  })

  // Load Ping handler on A
  await ao.msg({
    pid: pidA,
    act: "Eval",
    data: `
TARGET_B = "${pidB}"
Handlers.add("Ping", "Ping", function(msg)
  ao.send({ Target = TARGET_B, Action = "Echo", Data = "hello" })
  local reply = Receive({ From = TARGET_B })
  msg.reply({ Data = "got:" .. (reply.Data or "nil") })
end)`,
  })

  // Mutual trust
  await ao.msg({ pid: pidB, act: "Eval", data: `table.insert(ao.authorities, "${pidA}")` })
  await ao.msg({ pid: pidA, act: "Eval", data: `table.insert(ao.authorities, "${pidB}")` })
  console.log("Setup complete")

  // Fire Ping with check for the expected data
  console.log("Firing Ping...")
  const { res, out, err } = await ao.msg({
    pid: pidA,
    act: "Ping",
    data: "test",
    check: { data: "got:echo:hello" },
    get: "data",
  })
  console.log("err:", err)
  console.log("out:", out)
  console.log("res:", JSON.stringify(res)?.slice(0, 300))

  if (out === "got:echo:hello") {
    console.log("TEST 2: PASS - receive() resolved cross-process via ao.msg()!")
  } else if (String(JSON.stringify(res)).includes("got:echo:hello")) {
    console.log("TEST 2: PASS - data present in result")
  } else {
    console.log("TEST 2: PARTIAL")
  }
} catch (e) {
  console.log("TEST 2: FAIL -", e.message?.slice(0, 300))
}

console.log("\n=== Test 3: No module param (default fallback) ===")
try {
  const { pid } = await ao.spwn({})
  console.log("Spawned without module param, pid:", pid)
  const { res } = await ao.msg({ pid, act: "Eval", data: "return 'default-wasm'" })
  console.log("Result:", JSON.stringify(res)?.slice(0, 200))
  console.log("TEST 3: PASS")
} catch (e) {
  console.log("TEST 3: FAIL -", e.message?.slice(0, 300))
}

hbeam.kill()
