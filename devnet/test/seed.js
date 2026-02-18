/**
 * Seed script: spawns processes and sends messages through the MU so they
 * are properly executed and produce compute results.  Also posts some
 * direct Arweave transactions for the explorer to display.
 *
 * Usage:  node devnet/test/seed.js
 */
import _Arweave from "arweave"
const Arweave = _Arweave.default ?? _Arweave
import { createData, ArweaveSigner } from "arbundles"
import { acc } from "../../src/accounts.js"

const PORT = process.env.PORT || 8787
const BASE = `http://localhost:${PORT}`

// Built-in module with real wasm (aos2_0_6)
const AOS_MODULE = "ISShJH1ij-hPPt9St5UFFr_8Ys3Kj5cyg7zrMGt7H9s"

// ---- helpers ---------------------------------------------------------------

const arweave = Arweave.init()
arweave.transactions.getPrice = () => Promise.resolve("0")

async function getAnchor() {
  const info = await fetch(`${BASE}/ar`).then(r => r.json())
  return info.current || "0000000000000000000000000000000000000000000"
}
arweave.transactions.getTransactionAnchor = getAnchor

/**
 * Post a raw Arweave transaction via POST /ar/tx
 */
async function postTx(jwk, { data = "1984", tags = {}, target } = {}) {
  const tx = await arweave.createTransaction({ data, target: target || "" })
  for (const [name, value] of Object.entries(tags)) {
    tx.addTag(name, value)
  }
  await arweave.transactions.sign(tx, jwk)
  const body = tx.toJSON ? tx.toJSON() : JSON.parse(JSON.stringify(tx))
  const res = await fetch(`${BASE}/ar/tx`, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify(body),
  })
  const json = await res.json()
  if (res.status !== 200) {
    console.error(`  FAIL ${res.status}:`, json)
    return null
  }
  console.log(`  ${tags.Type || "Data"} ${tags.Name || ""}: ${tx.id}`)
  return tx.id
}

/**
 * Post a DataItem through the MU
 */
async function postMU(jwk, { data = "1984", tags = [], target } = {}) {
  const signer = new ArweaveSigner(jwk)
  const item = createData(data, signer, { tags, target: target || "" })
  await item.sign(signer)
  const raw = item.getRaw()
  const res = await fetch(`${BASE}/mu`, {
    method: "POST",
    headers: { "Content-Type": "application/octet-stream" },
    body: raw,
  })
  const json = await res.json().catch(() => ({}))
  if (res.status >= 400) {
    console.error(`  MU FAIL ${res.status}:`, json)
    return null
  }
  const id = json.id || item.id
  return id
}

function tag(name, value) {
  return { name, value: String(value) }
}

async function gql(query) {
  const res = await fetch(`${BASE}/ar/graphql`, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ query }),
  })
  return res.json()
}

// ---- main ------------------------------------------------------------------

console.log("Checking devnet...")
const info = await fetch(`${BASE}/ar`).then(r => r.json())
const VARIANT = info.network || "ao.DN.1"
console.log(`  network: ${VARIANT}  height: ${info.height}`)

const suInfo = await fetch(`${BASE}/su`).then(r => r.json())
const SCHEDULER = suInfo.Address
console.log(`  scheduler: ${SCHEDULER}`)
console.log(`  module: ${AOS_MODULE}`)

const owner = acc[0]
const owner2 = acc[1]

// ---- 1. Extra Module transactions (for explorer display) -------------------
console.log("\n--- Modules ---")
const mod1 = await postTx(owner.jwk, {
  data: "placeholder-wasm-module-aos-2.0.3",
  tags: {
    "Data-Protocol": "ao",
    Variant: VARIANT,
    Type: "Module",
    "Module-Format": "wasm64-unknown-emscripten-draft_2024_02_16",
    "Input-Encoding": "JSON-1",
    "Output-Encoding": "JSON-1",
    "Content-Type": "application/wasm",
    Name: "AOS 2.0.3",
  },
})

const mod2 = await postTx(owner.jwk, {
  data: "placeholder-wasm-module-aos-2.0.4",
  tags: {
    "Data-Protocol": "ao",
    Variant: VARIANT,
    Type: "Module",
    "Module-Format": "wasm64-unknown-emscripten-draft_2024_02_16",
    "Input-Encoding": "JSON-1",
    "Output-Encoding": "JSON-1",
    "Content-Type": "application/wasm",
    Name: "AOS 2.0.4",
  },
})

// ---- 2. Spawn processes via MU (uses built-in module for real execution) ----
console.log("\n--- Processes (via MU) ---")
const processNames = [
  "Counter",      // 0 - counts, relays pings
  "Greeter",      // 1 - greets, notifies TokenVault
  "TokenVault",   // 2 - handles transfers, logs to Registry
  "ChatRoom",     // 3 - greets, pings Counter, broadcasts to all
  "Registry",     // 4 - logs events, confirms
  "Relay",        // 5 - relays messages between processes (deep chains)
  "Aggregator",   // 6 - collects data from multiple processes
]
const processes = []
for (const name of processNames) {
  const jwk = processes.length % 2 === 0 ? owner.jwk : owner2.jwk
  const pid = await postMU(jwk, {
    data: `-- ${name} process source`,
    tags: [
      tag("Data-Protocol", "ao"),
      tag("Variant", VARIANT),
      tag("Type", "Process"),
      tag("Module", AOS_MODULE),
      tag("Scheduler", SCHEDULER),
      tag("Name", name),
      tag("Content-Type", "text/plain"),
    ],
  })
  if (pid) {
    processes.push({ name, id: pid })
    console.log(`  Process ${name}: ${pid}`)
  }
}

// Small delay to let processes initialize
await new Promise(r => setTimeout(r, 500))

// ---- 3. Install handlers FIRST (so subsequent messages trigger them) --------
console.log("\n--- Installing handlers (Eval) ---")

function evalTags() {
  return [
    tag("Data-Protocol", "ao"),
    tag("Variant", VARIANT),
    tag("Type", "Message"),
    tag("Action", "Eval"),
    tag("Content-Type", "text/plain"),
  ]
}

function msgTags(action) {
  return [
    tag("Data-Protocol", "ao"),
    tag("Variant", VARIANT),
    tag("Type", "Message"),
    tag("Action", action),
    tag("Content-Type", "text/plain"),
  ]
}

// Counter: Ping → Pong, Inc → Count, Relay-Ping → relays to Relay
await postMU(owner.jwk, {
  data: `local count = 0
Handlers.add("Ping", Handlers.utils.hasMatchingTag("Action", "Ping"), function(msg)
  ao.send({ Target = msg.From, Action = "Pong", Data = "pong from Counter" })
end)
Handlers.add("Inc", Handlers.utils.hasMatchingTag("Action", "Inc"), function(msg)
  count = count + 1
  ao.send({ Target = msg.From, Action = "Count", Data = tostring(count) })
end)
Handlers.add("Relay-Ping", Handlers.utils.hasMatchingTag("Action", "Relay-Ping"), function(msg)
  ao.send({ Target = "${processes[5].id}", Action = "Relay", Data = "ping-from-counter:" .. msg.Data,
    ["X-Origin"] = ao.id, ["X-Depth"] = tostring(tonumber(msg.Tags["X-Depth"] or "0") + 1) })
end)
Handlers.add("Broadcast-Ack", Handlers.utils.hasMatchingTag("Action", "Broadcast-Ack"), function(msg)
  ao.send({ Target = msg.From, Action = "Ack-Confirmed", Data = "Counter confirmed broadcast" })
end)
return "Counter handlers installed"`,
  target: processes[0].id,
  tags: evalTags(),
}).then(id => console.log(`  Counter handlers: ${id}`))

// Greeter: Hello → HelloReply + Notify(TokenVault), Greet → Greeting + logs
await postMU(owner2.jwk, {
  data: `Handlers.add("Hello", Handlers.utils.hasMatchingTag("Action", "Hello"), function(msg)
  ao.send({ Target = msg.From, Action = "HelloReply", Data = "Hello back from Greeter!" })
  ao.send({ Target = "${processes[2].id}", Action = "Notify", Data = "Someone said hello" })
  ao.send({ Target = "${processes[4].id}", Action = "Log", Data = "Greeter:Hello from " .. msg.From })
end)
Handlers.add("Greet", Handlers.utils.hasMatchingTag("Action", "Greet"), function(msg)
  ao.send({ Target = msg.From, Action = "Greeting", Data = "Hi there!" })
end)
Handlers.add("Broadcast-Ack", Handlers.utils.hasMatchingTag("Action", "Broadcast-Ack"), function(msg)
  ao.send({ Target = msg.From, Action = "Ack-Confirmed", Data = "Greeter confirmed broadcast" })
end)
return "Greeter handlers installed"`,
  target: processes[1].id,
  tags: evalTags(),
}).then(id => console.log(`  Greeter handlers: ${id}`))

// TokenVault: Transfer → Receipt + Log(Registry) + Notify(Aggregator), Notify → Ack
await postMU(owner.jwk, {
  data: `local balance = 1000
Handlers.add("Transfer", Handlers.utils.hasMatchingTag("Action", "Transfer"), function(msg)
  local qty = tonumber(msg.Data) or 0
  balance = balance - qty
  ao.send({ Target = msg.From, Action = "Transfer-Receipt", Data = "Transfer confirmed, balance: " .. tostring(balance) })
  ao.send({ Target = "${processes[4].id}", Action = "Log", Data = "Transfer:" .. tostring(qty) .. " from " .. msg.From })
  ao.send({ Target = "${processes[6].id}", Action = "Collect", Data = "transfer:" .. tostring(qty),
    ["X-Source"] = "TokenVault", ["X-Event"] = "Transfer" })
end)
Handlers.add("Notify", Handlers.utils.hasMatchingTag("Action", "Notify"), function(msg)
  ao.send({ Target = msg.From, Action = "Ack", Data = "Notification received by TokenVault" })
end)
Handlers.add("Broadcast-Ack", Handlers.utils.hasMatchingTag("Action", "Broadcast-Ack"), function(msg)
  ao.send({ Target = msg.From, Action = "Ack-Confirmed", Data = "TokenVault confirmed broadcast" })
end)
return "TokenVault handlers installed"`,
  target: processes[2].id,
  tags: evalTags(),
}).then(id => console.log(`  TokenVault handlers: ${id}`))

// ChatRoom: Greet → Welcome + Ping(Counter), Broadcast → sends to all processes
await postMU(owner2.jwk, {
  data: `Handlers.add("Greet", Handlers.utils.hasMatchingTag("Action", "Greet"), function(msg)
  ao.send({ Target = msg.From, Action = "Welcome", Data = "Welcome to ChatRoom!" })
  ao.send({ Target = "${processes[0].id}", Action = "Ping", Data = "new user joined" })
end)
Handlers.add("Broadcast", Handlers.utils.hasMatchingTag("Action", "Broadcast"), function(msg)
  local targets = { "${processes[0].id}", "${processes[1].id}", "${processes[2].id}", "${processes[4].id}", "${processes[5].id}", "${processes[6].id}" }
  for _, t in ipairs(targets) do
    ao.send({ Target = t, Action = "Broadcast-Ack", Data = "Broadcast from ChatRoom: " .. msg.Data })
  end
  ao.send({ Target = "${processes[4].id}", Action = "Log", Data = "ChatRoom:Broadcast:" .. msg.Data })
end)
Handlers.add("Broadcast-Ack", Handlers.utils.hasMatchingTag("Action", "Broadcast-Ack"), function(msg)
  ao.send({ Target = msg.From, Action = "Ack-Confirmed", Data = "ChatRoom confirmed broadcast" })
end)
return "ChatRoom handlers installed"`,
  target: processes[3].id,
  tags: evalTags(),
}).then(id => console.log(`  ChatRoom handlers: ${id}`))

// Registry: Log → LogConfirm, Digest → summarizes + notifies Aggregator
await postMU(owner.jwk, {
  data: `local logs = {}
Handlers.add("Log", Handlers.utils.hasMatchingTag("Action", "Log"), function(msg)
  table.insert(logs, msg.Data)
  ao.send({ Target = msg.From, Action = "LogConfirm", Data = "Logged #" .. tostring(#logs) .. ": " .. msg.Data })
end)
Handlers.add("Digest", Handlers.utils.hasMatchingTag("Action", "Digest"), function(msg)
  local summary = "Digest: " .. tostring(#logs) .. " entries"
  ao.send({ Target = msg.From, Action = "DigestResult", Data = summary })
  ao.send({ Target = "${processes[6].id}", Action = "Collect", Data = summary,
    ["X-Source"] = "Registry", ["X-Event"] = "Digest" })
end)
Handlers.add("Broadcast-Ack", Handlers.utils.hasMatchingTag("Action", "Broadcast-Ack"), function(msg)
  ao.send({ Target = msg.From, Action = "Ack-Confirmed", Data = "Registry confirmed broadcast" })
end)
return "Registry handlers installed"`,
  target: processes[4].id,
  tags: evalTags(),
}).then(id => console.log(`  Registry handlers: ${id}`))

// Relay: Relay → forwards to next hop (creates deep chains), Chain → cascades
await postMU(owner2.jwk, {
  data: `Handlers.add("Relay", Handlers.utils.hasMatchingTag("Action", "Relay"), function(msg)
  local depth = tonumber(msg.Tags["X-Depth"] or "0")
  ao.send({ Target = "${processes[4].id}", Action = "Log", Data = "Relay:depth=" .. tostring(depth) .. ":" .. msg.Data })
  if depth < 3 then
    ao.send({ Target = "${processes[0].id}", Action = "Relay-Ping", Data = msg.Data,
      ["X-Depth"] = tostring(depth) })
  else
    ao.send({ Target = "${processes[6].id}", Action = "Collect", Data = "relay-complete:depth=" .. tostring(depth),
      ["X-Source"] = "Relay", ["X-Event"] = "Chain-End" })
  end
end)
Handlers.add("Chain", Handlers.utils.hasMatchingTag("Action", "Chain"), function(msg)
  local step = tonumber(msg.Tags["X-Step"] or "1")
  ao.send({ Target = msg.From, Action = "Chain-Step", Data = "step-" .. tostring(step), ["X-Step"] = tostring(step) })
  if step < 4 then
    ao.send({ Target = "${processes[2].id}", Action = "Notify", Data = "chain-step-" .. tostring(step) })
    ao.send({ Target = "${processes[1].id}", Action = "Greet", Data = "chain-visitor-" .. tostring(step) })
  end
end)
Handlers.add("Broadcast-Ack", Handlers.utils.hasMatchingTag("Action", "Broadcast-Ack"), function(msg)
  ao.send({ Target = msg.From, Action = "Ack-Confirmed", Data = "Relay confirmed broadcast" })
end)
return "Relay handlers installed"`,
  target: processes[5].id,
  tags: evalTags(),
}).then(id => console.log(`  Relay handlers: ${id}`))

// Aggregator: Collect → gathers, Report → summarizes + notifies Registry
await postMU(owner.jwk, {
  data: `local collected = {}
Handlers.add("Collect", Handlers.utils.hasMatchingTag("Action", "Collect"), function(msg)
  table.insert(collected, {
    source = msg.Tags["X-Source"] or msg.From,
    event = msg.Tags["X-Event"] or "unknown",
    data = msg.Data
  })
  ao.send({ Target = msg.From, Action = "Collect-Ack", Data = "Collected #" .. tostring(#collected) })
end)
Handlers.add("Report", Handlers.utils.hasMatchingTag("Action", "Report"), function(msg)
  local report = "Aggregator Report: " .. tostring(#collected) .. " events collected"
  ao.send({ Target = msg.From, Action = "ReportResult", Data = report })
  ao.send({ Target = "${processes[4].id}", Action = "Log", Data = report })
  ao.send({ Target = "${processes[0].id}", Action = "Ping", Data = "report-generated" })
end)
Handlers.add("Broadcast-Ack", Handlers.utils.hasMatchingTag("Action", "Broadcast-Ack"), function(msg)
  ao.send({ Target = msg.From, Action = "Ack-Confirmed", Data = "Aggregator confirmed broadcast" })
end)
return "Aggregator handlers installed"`,
  target: processes[6].id,
  tags: evalTags(),
}).then(id => console.log(`  Aggregator handlers: ${id}`))

await new Promise(r => setTimeout(r, 500))

// ---- 4. Send messages that trigger handlers (produce child messages) --------
console.log("\n--- Messages (trigger handlers → spawn children) ---")

// === Flow 1: Simple ping → pong (1 parent → 1 child) ===
const pingMsg = await postMU(owner.jwk, {
  data: "ping!",
  target: processes[0].id,
  tags: msgTags("Ping"),
})
console.log(`  [Flow 1] Ping → Counter (→ Pong): ${pingMsg}`)

// === Flow 2: Hello → fan-out to 3 targets (1 parent → 3 children) ===
const helloMsg = await postMU(owner2.jwk, {
  data: "hello!",
  target: processes[1].id,
  tags: msgTags("Hello"),
})
console.log(`  [Flow 2] Hello → Greeter (→ HelloReply + Notify + Log): ${helloMsg}`)

// === Flow 3: Transfer → fan-out + deep chain ===
// Transfer → TokenVault → Receipt + Log(Registry) + Collect(Aggregator)
//                          Registry:Log → LogConfirm back to TokenVault
//                          Aggregator:Collect → Collect-Ack back to TokenVault
const transferMsg = await postMU(owner.jwk, {
  data: "100",
  target: processes[2].id,
  tags: msgTags("Transfer"),
})
console.log(`  [Flow 3] Transfer → TokenVault (→ Receipt + Log→LogConfirm + Collect→Ack): ${transferMsg}`)

// Another transfer for more data
await postMU(owner2.jwk, {
  data: "250",
  target: processes[2].id,
  tags: msgTags("Transfer"),
}).then(id => console.log(`  [Flow 3b] Transfer → TokenVault: ${id}`))

// === Flow 4: ChatRoom Greet → Welcome + Ping → Pong (2-level deep) ===
// Greet → ChatRoom → Welcome + Ping(Counter)
//                      Counter:Ping → Pong back to ChatRoom
const greetMsg = await postMU(owner2.jwk, {
  data: "hi everyone!",
  target: processes[3].id,
  tags: msgTags("Greet"),
})
console.log(`  [Flow 4] Greet → ChatRoom (→ Welcome + Ping→Pong): ${greetMsg}`)

// === Flow 5: Broadcast → fan-out to 6 processes (each replies) ===
// Broadcast → ChatRoom → sends Broadcast-Ack to all 6 processes + Log(Registry)
// Each process → Ack-Confirmed back to ChatRoom
// Registry:Log → LogConfirm back to ChatRoom
const broadcastMsg = await postMU(owner2.jwk, {
  data: "important announcement",
  target: processes[3].id,
  tags: msgTags("Broadcast"),
})
console.log(`  [Flow 5] Broadcast → ChatRoom (→ 6x Broadcast-Ack + Log): ${broadcastMsg}`)

// === Flow 6: Deep relay chain (Counter → Relay → Counter → Relay → ... ) ===
// Relay-Ping → Counter → Relay(depth=1) → Log + Relay-Ping(Counter)
//              Counter → Relay(depth=2) → Log + Relay-Ping(Counter)
//              Counter → Relay(depth=3) → Log + Collect(Aggregator) [chain ends]
const relayMsg = await postMU(owner.jwk, {
  data: "deep-chain-start",
  target: processes[0].id,
  tags: [
    ...msgTags("Relay-Ping"),
    tag("X-Depth", "0"),
  ],
})
console.log(`  [Flow 6] Relay-Ping → Counter → Relay → Counter → ... (depth 3): ${relayMsg}`)

// === Flow 7: Chain through Relay → parallel fan-out at each step ===
// Chain → Relay → Chain-Step + Notify(TokenVault) + Greet(Greeter)
// TokenVault:Notify → Ack back to Relay
// Greeter:Greet → Greeting back to Relay
for (let step = 1; step <= 3; step++) {
  const chainMsg = await postMU(owner.jwk, {
    data: `chain-data-${step}`,
    target: processes[5].id,
    tags: [
      ...msgTags("Chain"),
      tag("X-Step", String(step)),
    ],
  })
  console.log(`  [Flow 7.${step}] Chain(step=${step}) → Relay (→ Step + Notify + Greet): ${chainMsg}`)
}

// === Flow 8: Multi-send from Eval (1 parent → 3 children to different processes) ===
const evalMulti = await postMU(owner.jwk, {
  data: `ao.send({ Target = "${processes[1].id}", Action = "Greet", Data = "eval-greet-1" })
ao.send({ Target = "${processes[2].id}", Action = "Notify", Data = "eval-notify-1" })
ao.send({ Target = "${processes[4].id}", Action = "Log", Data = "eval-log-1" })
return "sent 3 messages to different processes"`,
  target: processes[0].id,
  tags: evalTags(),
})
console.log(`  [Flow 8] Eval+3xSend → Counter (→ Greet + Notify + Log): ${evalMulti}`)

// === Flow 9: Inc counter multiple times (each produces Count reply) ===
for (let i = 0; i < 5; i++) {
  const mid = await postMU(owner.jwk, {
    data: `increment-${i}`,
    target: processes[0].id,
    tags: msgTags("Inc"),
  })
  console.log(`  [Flow 9.${i}] Inc → Counter (→ Count=${i + 1}): ${mid}`)
}

// === Flow 10: Registry Digest → summarize + Collect(Aggregator) + Pong chain ===
// Trigger digest after logs have accumulated
const digestMsg = await postMU(owner.jwk, {
  data: "give-me-summary",
  target: processes[4].id,
  tags: msgTags("Digest"),
})
console.log(`  [Flow 10] Digest → Registry (→ DigestResult + Collect→Ack): ${digestMsg}`)

// === Flow 11: Aggregator Report → summarizes + Log(Registry) + Ping(Counter) ===
// Creates a triangle: Aggregator → Registry → LogConfirm, Aggregator → Counter → Pong
const reportMsg = await postMU(owner.jwk, {
  data: "generate-report",
  target: processes[6].id,
  tags: msgTags("Report"),
})
console.log(`  [Flow 11] Report → Aggregator (→ ReportResult + Log + Ping): ${reportMsg}`)

// === Flow 12: Second broadcast for more data ===
const broadcast2 = await postMU(owner.jwk, {
  data: "final update",
  target: processes[3].id,
  tags: msgTags("Broadcast"),
})
console.log(`  [Flow 12] Broadcast → ChatRoom (→ 6x Ack + Log): ${broadcast2}`)

// ---- 5. Some plain Arweave data transactions -------------------------------
console.log("\n--- Plain Arweave data ---")
await postTx(owner.jwk, {
  data: "Hello Arweave! This is a plain data transaction.",
  tags: { "Content-Type": "text/plain", "App-Name": "WAO-Explorer-Test" },
})
await postTx(owner2.jwk, {
  data: JSON.stringify({ foo: "bar", ts: Date.now() }),
  tags: { "Content-Type": "application/json", "App-Name": "WAO-Explorer-Test" },
})

// ---- 5b. Diverse content types for rich viewer ----------------------------
console.log("\n--- Rich content types ---")

// Markdown document
await postTx(owner.jwk, {
  data: `# WAO Explorer README

Welcome to the **WAO Explorer** — a transaction explorer for the AO network.

## Features

- Browse *transactions*, **processes**, and modules
- View message trees and compute results
- Real-time updates via WebSocket

## Getting Started

\`\`\`bash
npm install
npm run dev
\`\`\`

1. Open the explorer at \`http://localhost:8787\`
2. Navigate to any transaction
3. Explore the [AO documentation](https://ao.arweave.dev)

> This is a test markdown document for the rich data viewer.
`,
  tags: { "Content-Type": "text/markdown", "App-Name": "WAO-Explorer-Test", Name: "README" },
})

// SVG graphic
await postTx(owner.jwk, {
  data: `<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 200 200" width="200" height="200">
  <defs>
    <linearGradient id="g" x1="0%" y1="0%" x2="100%" y2="100%">
      <stop offset="0%" style="stop-color:#5137C5"/>
      <stop offset="100%" style="stop-color:#61afef"/>
    </linearGradient>
  </defs>
  <circle cx="100" cy="100" r="90" fill="url(#g)"/>
  <text x="100" y="110" text-anchor="middle" fill="white" font-size="36" font-weight="bold">WAO</text>
</svg>`,
  tags: { "Content-Type": "image/svg+xml", "App-Name": "WAO-Explorer-Test", Name: "Logo" },
})

// JavaScript code
await postTx(owner2.jwk, {
  data: `// Counter handler for AO process
const state = { count: 0, history: [] };

export function handle(msg) {
  if (msg.Action === "Inc") {
    state.count += 1;
    state.history.push({ ts: Date.now(), action: "inc" });
    return { Action: "Count", Data: String(state.count) };
  }
  if (msg.Action === "Get") {
    return { Action: "State", Data: JSON.stringify(state) };
  }
  return { Action: "Error", Data: "Unknown action" };
}`,
  tags: { "Content-Type": "application/javascript", "App-Name": "WAO-Explorer-Test", Name: "counter.js" },
})

// HTML page
await postTx(owner.jwk, {
  data: `<!DOCTYPE html>
<html>
<head><title>WAO Test Page</title></head>
<body style="font-family:sans-serif;padding:20px;background:#1a1a2e;color:#e4e4ed">
  <h1 style="color:#5137C5">Hello from WAO</h1>
  <p>This is a test HTML page stored on Arweave.</p>
  <ul><li>Item 1</li><li>Item 2</li><li>Item 3</li></ul>
</body>
</html>`,
  tags: { "Content-Type": "text/html", "App-Name": "WAO-Explorer-Test", Name: "test-page" },
})

// CSS stylesheet
await postTx(owner2.jwk, {
  data: `:root {
  --primary: #5137C5;
  --bg: #0f0f14;
  --text: #e4e4ed;
}

body {
  font-family: -apple-system, sans-serif;
  background: var(--bg);
  color: var(--text);
}

.button {
  background: var(--primary);
  color: white;
  border: none;
  padding: 8px 16px;
  border-radius: 6px;
  cursor: pointer;
}`,
  tags: { "Content-Type": "text/css", "App-Name": "WAO-Explorer-Test", Name: "theme.css" },
})

// JSON config (rich object)
await postTx(owner.jwk, {
  data: JSON.stringify({
    name: "WAO Devnet",
    version: "1.0.0",
    network: { ar: 4000, mu: 4002, su: 4003, cu: 4004 },
    modules: [AOS_MODULE],
    features: { websocket: true, explorer: true, rich_viewer: true },
    processes: processes.map(p => ({ name: p.name, id: p.id })),
  }, null, 2),
  tags: { "Content-Type": "application/json", "App-Name": "WAO-Explorer-Test", Name: "config.json" },
})

// Valid 1x1 red PNG
const pngData = Buffer.from(
  "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8/5+hHgAHggJ/PchI7wAAAABJRU5ErkJggg==",
  "base64"
)
await postTx(owner.jwk, {
  data: pngData,
  tags: { "Content-Type": "image/png", "App-Name": "WAO-Explorer-Test", Name: "pixel.png" },
})

// Valid minimal WAV audio (44-byte header + 4 bytes of silence)
const wavHeader = Buffer.alloc(48)
wavHeader.write("RIFF", 0)
wavHeader.writeUInt32LE(40, 4)
wavHeader.write("WAVE", 8)
wavHeader.write("fmt ", 12)
wavHeader.writeUInt32LE(16, 16)
wavHeader.writeUInt16LE(1, 20)   // PCM
wavHeader.writeUInt16LE(1, 22)   // mono
wavHeader.writeUInt32LE(8000, 24) // sample rate
wavHeader.writeUInt32LE(8000, 28) // byte rate
wavHeader.writeUInt16LE(1, 32)   // block align
wavHeader.writeUInt16LE(8, 34)   // bits per sample
wavHeader.write("data", 36)
wavHeader.writeUInt32LE(4, 40)   // data size
wavHeader.writeUInt32LE(0, 44)   // silence
await postTx(owner2.jwk, {
  data: wavHeader,
  tags: { "Content-Type": "audio/wav", "App-Name": "WAO-Explorer-Test", Name: "silence.wav" },
})

// Valid minimal WebM video (just the EBML header — enough to test the viewer)
const webmHeader = Buffer.from([
  0x1A, 0x45, 0xDF, 0xA3, 0x93, 0x42, 0x86, 0x81,
  0x01, 0x42, 0xF7, 0x81, 0x01, 0x42, 0xF2, 0x81,
  0x04, 0x42, 0xF3, 0x81, 0x08, 0x42, 0x82, 0x84,
  0x77, 0x65, 0x62, 0x6D, 0x42, 0x87, 0x81, 0x04,
  0x42, 0x85, 0x81, 0x02,
])
await postTx(owner.jwk, {
  data: webmHeader,
  tags: { "Content-Type": "video/webm", "App-Name": "WAO-Explorer-Test", Name: "test.webm" },
})

// ---- 6. Verify via GraphQL -------------------------------------------------
console.log("\n--- Verification ---")
const allTxs = await gql(`{
  transactions(first: 100, sort: HEIGHT_DESC) {
    edges { node { id tags { name value } } }
  }
}`)
const edges = allTxs.data?.transactions?.edges ?? []
console.log(`Total transactions: ${edges.length}`)

const types = {}
for (const e of edges) {
  const typeTag = e.node.tags.find(t => t.name === "Type")
  const type = typeTag?.value || "Data"
  types[type] = (types[type] || 0) + 1
}
console.log("By type:", types)

// Check compute results for Counter (should have many from all the interactions)
if (processes[0]) {
  const cuRes = await fetch(`${BASE}/cu/results/${processes[0].id}?limit=10`).then(r => r.json())
  console.log(`Compute results for ${processes[0].name}: ${cuRes?.edges?.length ?? 0}`)
}

// Check compute results for Aggregator (should have collected events)
if (processes[6]) {
  const cuRes = await fetch(`${BASE}/cu/results/${processes[6].id}?limit=5`).then(r => r.json())
  console.log(`Compute results for ${processes[6].name}: ${cuRes?.edges?.length ?? 0}`)
}

console.log(`\nDone! Explorer should now show data at http://localhost:${PORT}`)
console.log(`\nMessage flow summary:`)
console.log(`  7 processes spawned`)
console.log(`  Flow 1:  Ping → Pong (1→1)`)
console.log(`  Flow 2:  Hello → HelloReply + Notify + Log (1→3)`)
console.log(`  Flow 3:  Transfer → Receipt + Log→LogConfirm + Collect→Ack (1→3→2)`)
console.log(`  Flow 4:  Greet → Welcome + Ping→Pong (1→2→1)`)
console.log(`  Flow 5:  Broadcast → 6x Ack + Log (1→7→6)`)
console.log(`  Flow 6:  Relay-Ping → Counter↔Relay chain (depth 3) (1→1→1→...→Collect)`)
console.log(`  Flow 7:  Chain×3 → Step + Notify + Greet per step (3→9→...)`)
console.log(`  Flow 8:  Eval+3xSend → Greet + Notify + Log (1→3→3)`)
console.log(`  Flow 9:  5x Inc → 5x Count (5→5)`)
console.log(`  Flow 10: Digest → DigestResult + Collect→Ack (1→2→1)`)
console.log(`  Flow 11: Report → ReportResult + Log→LogConfirm + Ping→Pong (1→3→2)`)
console.log(`  Flow 12: Broadcast → 6x Ack + Log (1→7→6)`)
