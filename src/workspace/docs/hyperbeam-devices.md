# HyperBEAM Device Catalog

Complete reference for all HyperBEAM devices, endpoints, and configuration.

## URL Patterns

### Device Access

```
GET  /~device@version/method?param=value
POST /~device@version/method
```

### Process Operations

```
POST /~process@1.0/schedule                  Spawn process
POST /{pid}/schedule                         Schedule message
GET  /{pid}/compute?slot=N                   Compute up to slot
GET  /{pid}/now                              Get current state
GET  /{pid}/messages                         List scheduled messages
```

### Device Chaining (URL)

```
/path/~device1@1.0/method1/~device2@1.0/method2
```

Each method's output becomes the next method's input.

---

## Core Devices

### meta@1.0 — Node Gateway

Entry point for all HyperBEAM requests. Manages node config and authorization.

```js
// Get node info
await hb.g("/~meta@1.0/info")
// Returns: { initialized, address, port, ... }

// Update config (operator only)
await hb.p("/~meta@1.0/info", {
  route_owners: [addr1, addr2],
  cache_writers: [addr1],
  faff_allow_list: [addr1, addr2],
  simple_pay_price: 3,
})

// Get build info
await hb.g("/~meta@1.0/build")
```

**Node states:** `initialized: false` → `true` → `"permanent"` (locks config)

**Operator:** First to claim unclaimed node becomes operator. Use `HyperBEAM.OPERATOR` to auto-set.

### message@1.0 — Message Operations

Default device for direct message field access and manipulation.

```js
// Get field
await hb.g("/~message@1.0&hello=world/hello")

// Set field
await hb.p("/~message@1.0/set/hello", { hello: "world" })
await hb.p("/~message@1.0/set", { hello: "world" })

// Get keys
await hb.g("/~message@1.0/keys")

// Remove field
await hb.p("/~message@1.0/remove", { key: "hello" })

// Commit (sign)
await hb.get({ path: "/~message@1.0/commit", hello: "world" })

// Verify
await hb.g("/~message@1.0/verify")
```

### process@1.0 — Process Execution Engine

Core process device. Manages slot-based execution with device stacks.

Config tags when spawning:
- `device`: `"process@1.0"`
- `execution-device`: Which runtime (`genesis-wasm@1.0`, `wasm-64@1.0`, `lua@5.3a`, `wao@1.0`)
- `scheduler`: Scheduler address
- `type`: `"Process"`

### scheduler@1.0 — Message Scheduling

Manages message ordering and slot assignment for processes.

```js
// Spawn process via scheduler
const { process: pid } = await hb.p("/~scheduler@1.0/schedule", {
  body: {
    device: "process@1.0",
    type: "Process",
    scheduler: hb.addr,
    "execution-device": "wao@1.0",
  }
})

// Get slot
const { slot } = await hb.p("/~scheduler@1.0/schedule", { body: { target: pid } })

// Get status
const { processes } = await hb.g("/~scheduler@1.0/status")

// Register location
await hb.p("/~scheduler@1.0/location", {
  address: hb.addr,
  nonce: 0,
  url: "https://scheduler.example.com",
})
```

### stack@1.0 — Device Composition

Compose multiple devices into a pipeline. Each device's compute output feeds the next.

```js
// Spawn with device stack
const { pid } = await hb.spawn({
  "execution-device": "stack@1.0",
  "device-stack": ["wao@1.0", "patch@1.0"],
  "patch-from": "/results",
  "patch-to": "/cache",
})

// Stack as object (keyed)
const { pid } = await hb.spawn({
  "execution-device": "stack@1.0",
  "device-stack": { 1: "inc@1.0", 2: "double@1.0" },
})
```

**Prefix system:** Each device in stack gets a prefix to avoid key collisions.

### patch@1.0 — Message Reorganization

Copies data between paths in the message. Used in stacks to move results to cache.

Config:
- `patch-from`: Source path (e.g., `"/results"`)
- `patch-to`: Destination path (e.g., `"/cache"`)

```js
const { pid } = await hb.spawn({
  "execution-device": "stack@1.0",
  "device-stack": ["wao@1.0", "patch@1.0"],
  "patch-from": "/results",
  "patch-to": "/cache",
})

// Access patched data
const result = await hb.compute({ pid, slot, path: "/cache/key" })
```

### cache@1.0 — Local Cache Storage

Read/write data with access control.

```js
// Write binary — returns { path } directly
const { path } = await hb.p("/~cache@1.0/write", {
  "ao-body-key": "body",
  body: Buffer.from("data"),
})

// Read
const data = await hb.g("/~cache@1.0/read", { target: path })

// Link (alias)
await hb.p("/~cache@1.0/link", { source: path, destination: "alias" })

// Read via link
const same = await hb.g("/~cache@1.0/read", { target: "alias" })
```

Config: `cache_writers` (list of authorized addresses for writes)

### cron@1.0 — Scheduled Execution

Schedule one-time or recurring message execution.

```js
// Recurring task
const { body: taskId } = await hb.post({
  path: "/~cron@1.0/every",
  "cron-path": `/~wao@1.0/cron`,
  interval: "1000-milliseconds",
  target: pid,
})

// One-time task
await hb.post({
  path: "/~cron@1.0/once",
  "cron-path": `/~wao@1.0/cron`,
  interval: "5000-milliseconds",
  target: pid,
})

// Stop task
await hb.post({ path: "/~cron@1.0/stop", task: taskId })
```

**Interval format:** `"N-unit"` where unit is: `milliseconds`, `seconds`, `minutes`, `hours`, `days`

### router@1.0 — HTTP Message Routing

Route requests to different nodes based on URL patterns.

```js
// List routes
const routes = await hb.g("/~router@1.0/routes")

// Add route
await hb.p("/~router@1.0/routes", {
  template: "/api/v2/.*",
  node: "https://api-v2.example.com",
  priority: 1,
})

// Match route
const match = await hb.g("/~router@1.0/route", { "route-path": "/api/v2/users" })

// Preprocess (resolve route and relay)
await hb.p("/~router@1.0/preprocess", {
  request: { path: "/api/v2/data", method: "GET" },
  body: "optional-body",  // forwarded to matched node
})

// Get info
const info = await hb.g("/~router@1.0/info")
```

Config: `route_owners` (addresses allowed to manage routes), `router_preprocess_default` (`"local"` to handle unmatched routes locally)

### relay@1.0 — Message Relay

Relay messages between nodes/HTTP endpoints.

```js
// Synchronous relay (wait for response) — returns { body }
const { body } = await hb.getJSON({
  path: "/~relay@1.0/call",
  "relay-path": "http://other-node:10001/~meta@1.0/info",
})

// Async relay (fire-and-forget) — returns "OK"
await hb.get({
  path: "/~relay@1.0/cast",
  "relay-path": "http://other-node:10001/endpoint",
})
```

Options: `relay-path`, `relay-device`, `relay-method` (GET/POST), `relay-body`, `commit-request`

### local-name@1.0 — Local Name Registration

Register and look up local names.

```js
await hb.p("/~local-name@1.0/register", { value: "my-value", key: "my-key" })
const result = await hb.g("/~local-name@1.0/lookup", { key: "my-key" })
```

### lookup@1.0 — Resource Lookup

Fetch resources from cache by ID with format negotiation.

```js
const data = await hb.g("/~lookup@1.0/read", { target: messageId })
// With format conversion
const json = await hb.g("/~lookup@1.0/read", {
  target: messageId,
  accept: "application/aos-2",
})
```

### name@1.0 — Name Resolution

Resolve human-readable names via resolver chain.

```
GET /~name@1.0/{NAME}
GET /~name@1.0/{NAME}?load=false    (raw ID only)
```

Config: `name_resolvers` (list of resolver modules)

### monitor@1.0 — Process Monitoring

Non-intrusive observation of process execution.

```js
// In JS via AO SDK
await ao.monitor({ process: pid, signer })
await ao.unmonitor({ process: pid, signer })
```

### hook@1.0 — Lifecycle Hooks

Event-driven handlers for node lifecycle.

Built-in hooks: `start`, `request`, `step`, `response`

Config via `on` map in node config.

### node-process@1.0 — Singleton Processes

Node-specific singleton processes with lazy initialization.

```
GET  /{name}~node-process@1.0/path
POST /{name}~node-process@1.0/schedule
```

Config: `node_processes` map of process definitions, `spawn: true/false`

---

## Execution Devices

### genesis-wasm@1.0 — Legacy AO

Runs AOS processes via an external genesis-wasm Compute Unit server (port 6363).

```js
const hbeam = await new HyperBEAM({
  genesis_wasm: true,
  cu_port: 6363,
}).ready()

const { pid } = await hb.spawnLegacy()
const { slot } = await hb.scheduleLegacy({ pid, action: "Eval", data: luaCode })
const result = await hb.computeLegacy({ pid, slot })
```

**Limitations:**
- External CU is single-pass — **`Send().receive()` does NOT work**
- Auto-starts CU server at port 6363
- Requires `--experimental-wasm-memory64` Node.js flag

Functions: `init/3`, `compute/3`, `snapshot/3`, `import/3`

### wasm-64@1.0 — WASM Memory-64

WebAssembly runtime using WAMR (Memory-64 standard).

Functions: `init/3`, `compute/3`, `snapshot/3`, `normalize/3`, `terminate/3`

Config: `image` (WASM binary ID), `function`, `parameters`, `Mode` (AOT)

### lua@5.3a — Lua VM

Lua 5.3 execution with AO-Core library integration.

```js
const { pid } = await hb.spawnLua()
```

Functions: `init/3`, `functions/3`, `snapshot/3`, `normalize/3`

Config: `module` (Lua source ID), `sandbox` (true/false/map)

---

## Payment Devices

### simple-pay@1.0 — Flat-Rate Pricing

Per-request pricing with balance ledger.

```js
const hbeam = await new HyperBEAM({
  simple_pay: true,
  simple_pay_price: 2,
}).ready()

// Operator tops up user (only operator can topup)
await operator.hb.p("/~simple-pay@1.0/topup", {
  amount: 15,
  recipient: user.addr,
})

// Check balance
const bal = await user.hb.p("/~simple-pay@1.0/balance")

// Each POST costs: simple_pay_price * 3
await user.hb.p("/~message@1.0/set/hello", { hello: "world" })
// Balance decreases by 6 (2 * 3)

// Change price
await operator.hb.p("/~meta@1.0/info", { simple_pay_price: 3 })
```

### p4@1.0 — Lua-Based Payment Ledger

Programmable payment using Lua scripts for ledger and client logic.

```js
const hbeam = await new HyperBEAM({
  p4_lua: {
    processor: { body: ledgerScript, name: "ledger.lua" },
    client: { body: clientScript, name: "client.lua" },
    admin: operatorAddr,
    balance: { [operatorAddr]: 1000 },
  },
}).ready()

// Transfer via ledger
await hb.scheduleNP({
  pid: "ledger",
  tags: { action: "transfer", quantity: "100", recipient: addr },
})

// Check balance
const bal = await hb.g(`/ledger~node-process@1.0/now/balance/${addr}`)
```

### faff@1.0 — Friends & Family Whitelist

Simple access control via address whitelist.

```js
const hbeam = await new HyperBEAM({
  faff: [HyperBEAM.OPERATOR, allowedAddr],
}).ready()

// Operator can always access
await operator.hb.p(...)  // OK

// Allowed user can access
await allowed.hb.p(...)   // OK

// Others rejected
await assert.rejects(other.hb.p(...))

// Update list
await operator.hb.p("/~meta@1.0/info", {
  faff_allow_list: [operatorAddr, newAddr],
})
```

---

## Security Devices

### green-zone@1.0 — Trusted Node Networks

Secure trusted execution zones with encrypted communication.

```js
// Initialize green zone
await hb.p("/~green-zone@1.0/init")

// Join existing zone
await hb.p("/~green-zone@1.0/join", { peer: "http://trusted-node:10001" })

// Clone identity
await hb.p("/~green-zone@1.0/become", { peer: "http://source-node:10001" })

// Check trust
await hb.g("/~green-zone@1.0/is_trusted")
```

Encryption: RSA-4096 (asymmetric) + AES-256 (symmetric)

### auth-hook@1.0 — Authentication Hooks

Hook into authentication flow for custom auth logic.

---

## Encoding Devices

### httpsig@1.0 — HTTP Message Signatures

RFC 9421 HTTP Message Signatures. Default encoding for HyperBEAM.

- Flattens nested structures to multipart body
- Signs specified headers with RSA-PSS-SHA512
- Produces `signature` and `signature-input` headers

### structured@1.0 — Rich Typed Messages

RFC 9651 Structured Fields. Preserves types (int, float, atom, list).

### json@1.0 — JSON Codec

```js
// Serialize
await hb.p("/~json@1.0/serialize", { key: "value" })

// Deserialize
await hb.post({
  path: "/~json@1.0/deserialize",
  "ao-body-key": "body",
  body: JSON.stringify(data),
})
```

### flat@1.0 — Flat Path Format

Represents nested structures as flat path-based keys.

### ans104@1.0 — ANS-104 Bundle Format

ArBundle data format for Arweave-native encoding.

```js
const hb = new HB({ url, format: "ans104" })
const { pid } = await hb.spawn({
  "execution-device": "wao@1.0",
  "Data-Protocol": "ao",
  Variant: "ao.WDB.1",
})
```

---

## Query & Indexing Devices

### dev_query — Data Query Interface

General query interface for data retrieval.

### dev_query_arweave — Arweave GraphQL

Query Arweave via GraphQL from within HyperBEAM.

---

## Multi-Instance Setup

Running multiple HyperBEAM nodes requires unique ports and Erlang node names:

```js
const hbeam1 = await new HyperBEAM({ port: 10001, reset: true }).ready()
const hbeam2 = await new HyperBEAM({ port: 10002, reset: true }).ready()
```

Each instance needs unique port and auto-gets unique Erlang node name (`hb_{port}`).

---

## Device Configuration Summary

| Device | Key Config | Set Via |
|--------|-----------|---------|
| meta@1.0 | operator, initialized | POST /~meta@1.0/info |
| cache@1.0 | cache_writers | POST /~meta@1.0/info |
| router@1.0 | route_owners | POST /~meta@1.0/info |
| simple-pay@1.0 | simple_pay_price | POST /~meta@1.0/info |
| faff@1.0 | faff_allow_list | POST /~meta@1.0/info |
| cron@1.0 | interval, cron-path | POST /~cron@1.0/every |
| stack@1.0 | device-stack | Spawn tags |
| patch@1.0 | patch-from, patch-to | Spawn tags |
| genesis-wasm@1.0 | genesis_wasm_port | HyperBEAM constructor |
| p4@1.0 | p4_lua | HyperBEAM constructor |
