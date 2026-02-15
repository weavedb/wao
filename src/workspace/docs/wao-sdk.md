# WAO SDK API Reference

Complete API reference for building and testing WAO applications.

## AO Class

Main SDK class for deploying and interacting with AOS processes.

### Constructor

```js
new AO(opt = {})
```

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `hb` | string/HB | — | HyperBEAM URL or HB instance. `"ans104"` or `"httpsig"` for format |
| `port` | number | — | Arweave local testnet port (auto-configures aoconnect/ar) |
| `module` | string | srcs.module_aos2 | WASM module ID |
| `module_type` | string | `"aos2"` | `"aos2"`, `"sqlite"`, or `"mainnet"` |
| `scheduler` | string | srcs.scheduler | Scheduler process ID |
| `aoconnect` | object | — | aoconnect config object |
| `ar` | object/AR | — | Arweave client config or AR instance |
| `in_memory` | boolean | auto | Use in-memory AOS loader (auto when ar.port set) |
| `wao` | boolean | false | Use WAO module/scheduler variants |
| `variant` | string | — | ao variant tag (e.g., `"ao.TN.1"`) |
| `mem` | object | — | Shared memory for multi-user testing |

### init(jwk)

Initialize with Arweave JWK wallet.

```js
const ao = await new AO().init(acc[0])
```

Returns: `this`

### deploy(opts)

Deploy a process with optional Lua source.

```js
const { pid, p, err } = await ao.deploy({
  src_data,                    // Inline Lua source code
  boot: true,                  // Boot process after spawn
  loads: [code1, code2],       // Additional Lua to load after boot
  fills: { TOKEN: "0x123" },   // Replace <TOKEN> in source
  module: "custom-module-id",  // Custom WASM module
  scheduler: "custom-sched",   // Custom scheduler
  tags: { Extension: "WeaveDrive" },
  data: "boot data",
})
```

Returns: `{ err, jwk, pid, p }`
- `pid` — Process ID
- `p` — Process handle (shorthand client)

### msg(opts)

Send a message and wait for result.

```js
const { mid, res, err, out } = await ao.msg({
  pid,
  act: "Transfer",              // Action name (default: "Eval")
  tags: { Recipient: addr },    // Message tags
  data: "lua code",             // Message data
  check: { Status: "ok" },     // Validate response
  get: { data: true, json: true }, // Extract output
  timeout: 5000,                // Max wait ms
  mode: "aoconnect",            // "aoconnect" or "gql" (result fetching mode)
  limit: 25,                    // Max results to fetch
})
```

### dry(opts)

Dry-run: execute message without state change. Same params as `msg()`, no timeout.

```js
const { res, err, out } = await ao.dry({ pid, act: "Balance", tags: { Target: addr } })
```

### res(opts)

Get result of a previously sent message.

```js
const { mid, res, err, out } = await ao.res({
  pid, mid, check, get, timeout,
  mode: "aoconnect",            // "aoconnect" or "gql"
  limit: 25,                    // Max results to fetch
})
```

### ress(opts)

Paginated results for a process.

```js
const { err, out, res, next } = await ao.ress({ pid, limit: 10, asc: true, cursor: null })
const page2 = next ? await next() : null
```

### asgn(opts)

Assign message to process.

```js
const { mid, res, err, out } = await ao.asgn({ pid, mid })
```

### load(opts)

Load and execute Lua source on an existing process.

```js
await ao.load({ pid, data: luaCode, fills: { VAR: "value" } })
```

### var(opts)

Get a Lua variable value from process.

```js
const val = await ao.var({ pid, data: "Table" })
const nested = await ao.var({ pid, data: "Table.Array[2]" })
```

### wait(opts)

Wait for process to initialize.

```js
const { err, pid } = await ao.wait({ pid, attempts: 10 })
```

### attest(opts)

Post attestation for data (used with WeaveDrive).

```js
await ao.attest({ id: txId })
```

### avail(opts)

Announce data availability.

```js
await ao.avail({ ids: [txId1, txId2] })
```

### pipe(opts)

Execute a pipeline of operations. Each step can forward results to the next.

```js
const { out } = await ao.pipe({
  fns: [
    { fn: "deploy", args: { src_data }, then: { "args.pid": "pid" } },
    { fn: "msg", args: { act: "Inc" }, then: { "args.pid": "pid" } },
    { fn: "dry", args: { act: "Get" } },
  ]
})
```

- `then` maps result fields to next step's args
- `err` function handles errors per step

### postModule(opts)

Post a WASM module to Arweave.

```js
const { id } = await ao.postModule({ data: wasmBinary })
```

### postScheduler(opts)

Register scheduler location.

```js
await ao.postScheduler({ url: "https://scheduler.example.com" })
```

### spwn(opts)

Low-level spawn (use `deploy` for most cases).

```js
const { pid, p } = await ao.spwn({ module, scheduler, tags })
```

### transform(opts)

Apply template fills to Lua source.

```js
const { out } = await ao.transform({
  data: luaCode,
  fills: { OWNER: addr, TOKEN: tokenPid }
})
// Replaces <OWNER> and <TOKEN> in luaCode
```

### eval(opts)

Evaluate a Lua expression on an existing process (shorthand for msg with act "Eval").

```js
const { mid, res, err, out } = await ao.eval({ pid, data: "return count" })
```

### p(pid)

Create a Process handle for an existing process.

```js
const proc = ao.p(existingPid)
await proc.d("Get", false)
```

---

## Process Handle (p)

Returned by `ao.deploy()` or `ao.p(pid)`. Provides shorthand for process operations.

**Important:** The second argument to `p.msg()`/`p.m()` passes additional fields as **message tags** (accessible via `msg.Tags.FieldName` in Lua), NOT as `msg.Data`. To pass structured data, use tags.

### msg(act, tags, opts)

Send message with shorthand syntax.

```js
await p.msg("Inc")                          // basic action
await p.msg("Inc", { Plus: "3" })           // with tags (Lua: msg.Tags.Plus)
await p.msg("Inc", false)                   // returns Data string
await p.msg("Inc", { Plus: "3" }, false)    // tags + Data string
```

### dry(act, tags, opts)

Dry-run (read-only) with same shorthand.

```js
const result = await p.dry("Get")
const data = await p.dry("Get", false)      // Data string only
```

### m(...args) — shorthand msg

Calls `msg()` and throws on error.

```js
await p.m("Inc", false)                     // returns Data or throws
```

### d(...args) — shorthand dry

Calls `dry()` and throws on error.

```js
const count = await p.d("Get", false)       // Data string or throws
```

### r(...args) — shorthand res

```js
const result = await p.r({ mid })
```

### v(data, json, pretty) — get variable

```js
const state = await p.v("State")            // JSON parsed
const count = await p.v("count")            // number
const item = await p.v("Table.items[1]")    // nested access
```

### load(opt) — load Lua source

Load and execute Lua source on an existing process via the process handle.

```js
await p.load({ data: luaCode, fills: { VAR: "value" } })
```

### o(name, args) — call and throw

Generic method caller that throws on error.

```js
const out = await p.o("msg", ["Inc", false])
```

---

## get/check Patterns

### get — Extract output from responses

| Value | Behavior |
|-------|----------|
| `false` | Return Data as string |
| `true` | Return full result |
| `"TagName"` | Return value of specific tag |
| `{ data: true }` | Return Data field |
| `{ json: true }` | Parse Data as JSON |
| `{ data: true, json: true }` | Parse Data JSON |
| `{ name: "Tag", json: true }` | Parse specific tag as JSON |
| `{ from: "pid" }` | Filter by sender process |
| `{ match: (v, i, r) => bool }` | Custom filter function |

```js
// Get Data as string
await p.d("Get", false)

// Get Data as parsed JSON
await p.d("Get", { get: { data: true, json: true } })

// Get specific tag value
await p.msg("Inc", { get: "Status" })
```

### check — Validate responses

| Value | Behavior |
|-------|----------|
| `true` | Check no error |
| `"value"` | Check Data equals value |
| `"TagName"` | Check tag exists |
| `{ Tag: "val" }` | Check tag equals value |
| `RegExp` | Pattern match on Data |
| `[array]` | Check multiple messages |

```js
await p.msg("Inc", { check: { Status: "ok" } })
await p.msg("Inc", { check: /Success/ })
```

---

## AR Class

Arweave client for wallet management, data posting, and transactions.

### Constructor

```js
new AR({ host, port, protocol } = {})
```

### Methods

| Method | Params | Returns | Description |
|--------|--------|---------|-------------|
| `init(jwk)` | JWK | `this` | Initialize with wallet |
| `checkWallet()` | — | `{ addr, jwk, pub }` | Validate wallet |
| `balance(addr?)` | address | string | Get AR balance |
| `mint(addr, amount?)` | address, AR string | balance | Mint on testnet |
| `transfer(ar, target)` | amount, address | `{ id }` | Send AR |
| `bundle(items)` | `[[data, tags]]` | `{ id, items, tx }` | Create bundle |
| `post({ data, tags })` | object | `{ id }` | Post data to Arweave |
| `data(txid, string?)` | txId, bool | data | Download tx data |
| `tx(txid)` | txId | tx object | Get tx metadata |
| `gen(amount?)` | AR amount | `{ jwk, addr }` | Generate new wallet |
| `toAddr(jwk)` | JWK | address | Get address from JWK |

```js
const ar = new AR()
await ar.init(jwk)
const { id } = await ar.post({ data: "Hello", tags: { Type: "test" } })
const content = await ar.data(id, true)
```

---

## HB Class

HTTP client for HyperBEAM nodes. Handles signing and process operations.

### Constructor

```js
new HB({
  url: "http://localhost:10001",
  cu: "http://localhost:6363",
  format: "httpsig",    // or "ans104"
} = {})
```

### init(jwk)

```js
const hb = await new HB({ url: hbeam.url }).init(jwk)
// hb.addr, hb.url, hb.jwk available
```

### HTTP Methods

```js
// GET — returns { out, headers, body, status }
await hb.get({ path: "/~meta@1.0/info" })
await hb.g("/~meta@1.0/info")                    // shorthand, returns out
await hb.g("/~meta@1.0/info", { key: "value" })  // with params

// POST — returns { out, headers, body, status }
await hb.post({ path: "/~meta@1.0/info", key: "value" })
await hb.p("/~meta@1.0/info", { key: "value" })  // shorthand, returns out

// JSON variants
await hb.getJSON({ path })
await hb.postJSON({ path, ...fields })
```

### Process Lifecycle

```js
// Spawn
const { pid } = await hb.spawn({})
const { pid } = await hb.spawn({ "execution-device": "wao@1.0" })
const { pid } = await hb.spawnLua()
const { pid } = await hb.spawnLegacy()
const { pid } = await hb.spawnAOS()

// Schedule message
const { slot } = await hb.schedule({ pid, tags: { Action: "Inc" } })
const { slot } = await hb.scheduleLegacy({ pid, action: "Inc" })
const { slot } = await hb.scheduleAOS({ pid, action: "Eval", data: luaCode })

// Compute result
const result = await hb.compute({ pid, slot })
const result = await hb.computeLegacy({ pid, slot })
const result = await hb.computeAOS({ pid, slot })

// All-in-one (schedule + compute)
const result = await hb.message({ pid, tags: { Action: "Inc" } })
const { outbox } = await hb.messageAOS({ pid, action: "Get" })

// Current slot
const slot = await hb.slot({ pid })
const slot = await hb.slot({ pid, path: "/cache/key" })

// Current state
const state = await hb.now({ pid })
const state = await hb.now({ pid, path: "/cache/key" })

// Messages list (paginated)
const { edges, next } = await hb.messages({ pid })
const page2 = next ? await next() : null
```

### Device Stack Spawn

```js
const { pid } = await hb.spawn({
  "execution-device": "stack@1.0",
  "device-stack": ["wao@1.0", "patch@1.0"],
  "patch-from": "/results",
  "patch-to": "/cache",
})
```

### Caching

```js
const moduleId = await hb.cacheBinary(wasmData, "application/wasm")
const scriptId = await hb.cacheScript(luaCode, "application/lua")
```

### Binary Data

```js
// Write binary — returns { path } directly
const { path } = await hb.p("/~cache@1.0/write", {
  "ao-body-key": "body",
  body: Buffer.from("data"),
})

// Read binary
const data = await hb.g("/~cache@1.0/read", { target: path })
```

### ANS-104 Format

```js
const hb = new HB({ url, format: "ans104" })
const { pid } = await hb.spawn({
  Name: "my-process",
  "execution-device": "wao@1.0",
  "Data-Protocol": "ao",
  Variant: "ao.WDB.1",
})
```

---

## GQL Class

GraphQL client for querying Arweave transactions and blocks.

### txs(opts)

```js
const txs = await gql.txs({
  ids: ["txId1"],                    // by ID
  owner: "addr",                      // by owner
  recipient: "addr",                  // by recipient
  tags: { Type: "Message" },          // by tags
  block: [100, 200],                  // block range [min, max]
  first: 10,                          // limit
  after: "cursor",                    // pagination cursor
  asc: true,                          // ascending sort
  fields: ["id", "tags"],             // select fields
  next: true,                         // include pagination fn
})
```

### blocks(opts)

```js
const blocks = await gql.blocks({ height: [100, 200], first: 10 })
```

---

## HyperBEAM Class

Node lifecycle manager for testing. Starts/stops Erlang HyperBEAM server.

### Constructor

```js
new HyperBEAM({
  port: 10001,                  // HTTP port
  cu_port: 6363,                // genesis-wasm CU port
  wallet: ".wallet.json",       // wallet path
  cwd: "./HyperBEAM",          // HyperBEAM directory
  reset: true,                  // clear cache
  genesis_wasm: true,           // enable genesis-wasm device
  operator: HyperBEAM.OPERATOR, // use self as operator
  logs: true,                   // print logs
  rebar3: true,                 // use rebar3 shell
  bundler_ans104: false,        // ANS-104 bundler
  // Payment options
  faff: [addr1, addr2],         // Friends & Family whitelist
  simple_pay: true,             // enable simple-pay device
  simple_pay_price: 2,          // cost per operation
  p4_lua: {                     // P4 payment config
    processor: { body: luaScript, name: "ledger.lua" },
    client: { body: clientScript, name: "client.lua" },
    admin: adminAddr,
    balance: { [addr]: 1000 },
  },
} = {})
```

### Methods

| Method | Returns | Description |
|--------|---------|-------------|
| `ready(timeout?)` | `this` or `false` | Wait for server ready (default 60s), returns `false` on timeout |
| `ok()` | boolean | Check if responding |
| `kill()` | — | Kill server and CU |
| `startCU()` | boolean | Start genesis-wasm CU |
| `eunit(module, test)` | Promise | Run Erlang eunit test |
| `file(path)` | string | Read file from HyperBEAM dir |

### Properties

| Property | Description |
|----------|-------------|
| `hb` | HB HTTP client instance |
| `jwk` | Server wallet JWK |
| `addr` | Server wallet address |
| `url` | Server URL |

### Static

- `HyperBEAM.OPERATOR` — Symbol: use self address as operator

---

## Test Exports (wao/test)

```js
import {
  AO,           // In-memory AOS client
  HyperBEAM,    // Node lifecycle manager
  HB,           // HTTP client
  acc,           // Array of 3 test accounts: [{ jwk, addr, signer }]
  mu, su, cu,    // Individual test accounts
  connect,       // aoconnect factory
  Testnet,       // Local testnet orchestrator
  Src,           // Lua source file manager
  setup,         // Full testnet setup with caching
  ok,            // Assert no error: ok(result)
  fail,          // Assert error: fail(result)
  blueprint,     // Load Lua blueprint: await blueprint("apm")
} from "wao/test"
```

### Test Accounts

```js
// 3 pre-generated accounts with JWK, address, and signer
acc[0].jwk    // Arweave JWK
acc[0].addr   // Arweave address
acc[0].signer // DataItem signer
```

### Src Class

```js
const src = new Src({ dir: "./src" })
const data = src.data("counter", "lua")     // Read file contents
const txId = await src.upload("counter")     // Upload to Arweave
```

### blueprint(name)

Load a Lua blueprint for AOS.

```js
const code = await blueprint("apm")
const { p } = await ao.deploy({ loads: [await blueprint("apm"), src_data] })
```

### Memory Sharing (Multi-User)

```js
const ao1 = await new AO().init(acc[0])
const { p } = await ao1.deploy({ src_data })

const ao2 = await new AO({ mem: ao1.mem }).init(acc[1])
const p2 = ao2.p(p.pid)
// p2 sees same process state as p, but signs as acc[1]
```

---

## Template Fills

Replace `<VAR>` placeholders in Lua source with actual values.

```js
await ao.deploy({
  src_data: `
    TOKEN = "<TOKEN_PID>"
    OWNER = "<OWNER_ADDR>"
  `,
  fills: {
    TOKEN_PID: "abc123...",
    OWNER_ADDR: "xyz789...",
  }
})
```

---

## Common Patterns

### Deploy and Test

```js
const ao = await new AO().init(acc[0])
const { p } = await ao.deploy({ src_data })
const count = await p.d("Get", false)
assert.equal(count, "0")
await p.m("Inc", false)
assert.equal(await p.d("Get", false), "1")
```

### Multi-User Token Transfer

```js
const ao1 = await new AO().init(acc[0])
const { p } = await ao1.deploy({ src_data })
const ao2 = await new AO({ mem: ao1.mem }).init(acc[1])
const p2 = ao2.p(p.pid)

await p.m("Transfer", { Recipient: acc[1].addr, Quantity: "100" })
const bal = await p2.d("Balance", { Target: acc[1].addr }, false)
```

### WeaveDrive Attestation

```js
const { p } = await ao.deploy({
  tags: { Extension: "WeaveDrive", Attestor: ao.ar.addr },
  loads: [await blueprint("apm"), handler],
  src_data,
})
const { id } = await ao.ar.post({ data: "Hello" })
await ao.attest({ id })
const result = await p.d("Get", { id }, false)
```

### HyperBEAM Integration

```js
const hbeam = await new HyperBEAM({ reset: true, genesis_wasm: true }).ready()
const ao = await new AO({ hb: hbeam.url }).init(hbeam.jwk)
const { p } = await ao.deploy({ src_data })
await p.m("Inc")
const count = await p.d("Get", false)
hbeam.kill()
```

---

## Browser / wao/web

For browser applications, use `wao/web` instead of `wao/test`. This provides the same AO and AR classes but configured for browser environments with ArConnect wallet integration.

### Import

```js
import { AO, AR } from "wao/web"
```

**Important**: Never use `wao/test` in browser code. `wao/test` is for Node.js testing only and includes dependencies that don't work in browsers.

### ArConnect Wallet Connection

```js
// Check ArConnect is available
if (!window.arweaveWallet) {
  throw new Error("ArConnect not found. Install from https://arconnect.io")
}

// Request permissions
await window.arweaveWallet.connect([
  "ACCESS_ADDRESS",
  "SIGN_TRANSACTION",
  "ACCESS_PUBLIC_KEY",
])

// Get user address
const addr = await window.arweaveWallet.getActiveAddress()
```

### AO Browser Constructor

```js
const ao = new AO()
await ao.init(window.arweaveWallet)
```

The browser AO constructor accepts the ArConnect wallet object directly (instead of a JWK). All signing happens through ArConnect.

### Interacting with Deployed Processes

```js
// Connect to an existing process by ID
const p = ao.p("YOUR_PROCESS_ID")

// Send a message
const { out } = await p.msg("Action", { Tag: "value" })

// Read state (dry-run)
const { out: balance } = await p.msg("Balance", { Target: addr })
```

### Browser-Specific AO Options

```js
const ao = new AO({
  // Custom gateway
  ar: { host: "arweave.net", port: 443, protocol: "https" },
})
```

### Example: Token Balance Checker

```js
import { AO } from "wao/web"

async function checkBalance(processId, address) {
  const ao = new AO()
  await ao.init(window.arweaveWallet)
  const p = ao.p(processId)
  const { out } = await p.msg("Balance", { Target: address })
  return out
}
```

### Example: Send Transfer

```js
import { AO } from "wao/web"

async function transfer(processId, recipient, quantity) {
  const ao = new AO()
  await ao.init(window.arweaveWallet)
  const p = ao.p(processId)
  const { out } = await p.msg("Transfer", {
    Recipient: recipient,
    Quantity: quantity,
  })
  return out
}
```
