---
paths:
  - "test/**/*.js"
---

# Testing Rules

For full SDK API reference, read `docs/wao-sdk.md`.

## Framework

Node.js built-in test runner (`node:test`). No external test framework needed.

```js
import assert from "assert"
import { describe, it, before, after } from "node:test"
```

## In-Memory AOS Testing (legacynet units)

Runs AOS WASM directly in Node.js — no server, no Erlang. Instant. Best for unit testing AOS script logic.

```js
import { AO, acc } from "wao/test"
import { readFileSync } from "fs"
import { resolve } from "path"

const src_data = readFileSync(
  resolve(import.meta.dirname, "../src/file.lua"),
  "utf8"
)

describe("My AOS Script", function () {
  it("should work", async () => {
    const ao = await new AO().init(acc[0])
    const { p, pid } = await ao.deploy({ src_data })

    // dry-run (read-only, no state change)
    const result = await p.d("ActionName", false)

    // message (mutates state)
    const result2 = await p.m("ActionName", false)

    // message with extra tags
    const result3 = await p.m("ActionName", { SomeTag: "value" }, false)
  })
})
```

### Process Handle Shorthand

- `p.d("Action", false)` — dry-run, returns Data string
- `p.m("Action", false)` — send message, returns Data string
- `p.m("Action", { Tag: "val" }, false)` — send message with tags
- `p.v("State")` — get Lua variable as JSON
- `p.v("Table.items[1]")` — nested access

### get/check Patterns

```js
// get: extract output
await p.d("Get", false)                           // Data as string
await p.d("Get", { get: { data: true, json: true } }) // Data as parsed JSON
await p.msg("Inc", { get: "Status" })              // Specific tag value
await p.d("Get", { get: { from: "pid", json: true } }) // Filter by sender

// check: validate response
await p.msg("Inc", { check: { Status: "ok" } })    // Tag value check
await p.msg("Inc", { check: /Success/ })            // Regex match
```

## HyperBEAM AOS Testing (integration)

Spawns a real Erlang HyperBEAM node. AOS processes run on the HyperBEAM stack via `genesis-wasm` device. Messages go through HTTP with slot-based scheduling. Best for integration testing the full production stack.

### Raw HyperBEAM API

```js
import { HyperBEAM } from "wao/test"

describe("HyperBEAM", function () {
  let hbeam, hb

  before(async () => {
    hbeam = await new HyperBEAM({ reset: true }).ready()
    hb = hbeam.hb
  })

  after(async () => hbeam.kill())

  it("should work", async () => {
    await hb.post({ path: "/~meta@1.0/info", key: "value" })
    const { out } = await hb.get({ path: "/~meta@1.0/info" })
  })
})
```

### HyperBEAM Spawn Patterns

```js
// Basic spawn (default device)
const { pid } = await hb.spawn({})

// Legacy AOS (genesis-wasm CU)
const { pid } = await hb.spawnLegacy()
const { pid } = await hb.spawnAOS()

// Lua VM
const { pid } = await hb.spawnLua()

// Custom execution device
const { pid } = await hb.spawn({ "execution-device": "wao@1.0" })
```

### Device Stack Testing

```js
const { pid } = await hb.spawn({
  "execution-device": "stack@1.0",
  "device-stack": ["wao@1.0", "patch@1.0"],
  "patch-from": "/results",
  "patch-to": "/cache",
})
const { slot } = await hb.schedule({ pid })
const result = await hb.compute({ pid, slot, path: "/cache/key" })
```

### AOS on HyperBEAM (via HB client)

```js
import { HyperBEAM, HB } from "wao/test"

let hbeam = await new HyperBEAM({ reset: true, genesis_wasm: true }).ready()
let hb = await new HB({ url: hbeam.url }).init(jwk)

// Spawn AOS process on HyperBEAM
const { pid } = await hb.spawnLegacy()

// Send message (slot-based scheduling)
const { slot } = await hb.scheduleLegacy({ pid, action: "Inc", data })

// Get result
const result = await hb.computeLegacy({ pid, slot })

hbeam.kill()
```

### AOS class routed through HyperBEAM

```js
import { AO } from "wao"
import { HyperBEAM } from "wao/test"

let hbeam = await new HyperBEAM({ reset: true, genesis_wasm: true }).ready()
let ao = await new AO({ hb: hbeam.url }).init(hbeam.jwk)

const { pid, p } = await ao.deploy({ src_data })
await p.m("Inc")
const result = await p.d("Get")

hbeam.kill()
```

Always call `hbeam.kill()` in `after()` to clean up the HyperBEAM Erlang process.

## Multi-User Testing

Share memory between AO instances to simulate multiple users:

```js
const ao1 = await new AO().init(acc[0])
const { p, pid } = await ao1.deploy({ src_data })

// second user shares the same memory
const ao2 = await new AO({ mem: ao1.mem }).init(acc[1])
const p2 = ao2.p(pid)
await p2.m("Transfer", { Recipient: acc[0].addr, Quantity: "100" })
```

## Payment Testing

### Simple Pay

```js
const hbeam = await new HyperBEAM({
  simple_pay: true,
  simple_pay_price: 2,
  operator: HyperBEAM.OPERATOR,
}).ready()

// Topup user
await operator.hb.p("/~simple-pay@1.0/topup", { amount: 15, recipient: user.addr })
const bal = await user.hb.p("/~simple-pay@1.0/balance")
```

### FAFF (Access Control)

```js
const hbeam = await new HyperBEAM({
  faff: [HyperBEAM.OPERATOR, allowedAddr],
}).ready()
await assert.rejects(disallowed.hb.p(...))  // rejected
```

### P4 Payment

```js
const hbeam = await new HyperBEAM({
  p4_lua: {
    processor: { body: ledgerScript, name: "ledger.lua" },
    client: { body: clientScript, name: "client.lua" },
    admin: addr,
    balance: { [addr]: 1000 },
  },
}).ready()
```

## Cron Testing

```js
const { body: taskId } = await hb.post({
  path: "/~cron@1.0/every",
  "cron-path": `/~wao@1.0/cron`,
  interval: "1000-milliseconds",
  target: pid,
})
await wait(3000)
await hb.post({ path: "/~cron@1.0/stop", task: taskId })
```

## ANS-104 Format Testing

```js
const hb = new HB({ url: hbeam.url, format: "ans104" })
const { pid } = await hb.spawn({
  "execution-device": "wao@1.0",
  "Data-Protocol": "ao",
  Variant: "ao.WDB.1",
})
```

## Port Cleanup

Before HyperBEAM tests, kill stale processes:

```bash
pkill -f beam.smp 2>/dev/null || true
lsof -ti :10000-10010 | xargs -r kill -9 2>/dev/null || true
```

## Running Tests

```bash
yarn test test/aos.test.js         # in-memory AOS (fast unit tests)
yarn test test/hyperbeam.test.js   # HyperBEAM (integration tests)
yarn test                          # all test files
```
