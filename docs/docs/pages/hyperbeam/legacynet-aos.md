# Legacynet Compatible AOS

Legacynet compatible AOS uses `genesis-wasm@1.0` to delegate compute to an external local CU. You can use wasm modules stored on the Arweave Mainnet storage, or you could create a helper method to locally store wasm modules for testing. You should also pass `as = ["genesis_wasm"]` to the test HyperBEAM node to auto-start a local CU server with HyperBEAM.

Let's use the production AOS2.0.6 module stored at `ISShJH1ij-hPPt9St5UFFr_8Ys3Kj5cyg7zrMGt7H9s` for now.

```js
import assert from "assert"
import { describe, it, before, after, beforeEach } from "node:test"
import { HyperBEAM, toAddr } from "wao/test"
import { HB } from "wao"
import { resolve } from "path"
import { readFileSync } from "fs"

const seed = num => {
  const array = new Array(num)
  for (let i = 0; i < num; i++) array[i] = Math.floor(Math.random() * 256)
  return Buffer.from(array).toString("base64")
}

const cwd = "../dev/wao/HyperBEAM"
const wallet = resolve(process.cwd(), cwd, ".wallet.json")
const jwk = JSON.parse(readFileSync(wallet, "utf8"))
const addr = toAddr(jwk.n)

const src_data = `local count = 0
Handlers.add("Inc", "Inc", function (msg)
  count = count + 1
end)

Handlers.add("Get", "Get", function (msg)
  msg.reply({ Data = tostring(count) })
end)`

describe("HyperBEAM", function () {
  let hbeam, hb
  before(async () => {
    hbeam = await new HyperBEAM({ cwd, as: ["genesis_wasm"] }).ready()
  })
  beforeEach(async () => (hb = await new HB({}).init(jwk)))
  after(async () => hbeam.kill())
  
  it("should spawn a legacynet AOS process", async () => {
    const {
      out: { process: pid },
    } = await hb.post({
      device: "process@1.0",
      path: "/schedule",
      Type: "Process",
      "data-protocol": "ao",
      variant: "ao.TN.1",
      scheduler: addr,
      authority: addr,
      "random-seed": seed(16),
      module: "ISShJH1ij-hPPt9St5UFFr_8Ys3Kj5cyg7zrMGt7H9s",
      "scheduler-device": "scheduler@1.0",
      "execution-device": "stack@1.0",
	  "device-stack" : ["genesis-wasm@1.0", "patch@1.0"],
	  "push-device": "push@1.0",
    })
    await hb.post({
      path: `/${pid}/schedule`,
      type: "Message",
      target: pid,
      action: "Eval",
      data: src_data,
    })
    await hb.post({
      path: `/${pid}/schedule`,
      type: "Message",
      target: pid,
      action: "Inc",
    })
    const {
      out: { slot },
    } = await hb.post({
      path: `/${pid}/schedule`,
      type: "Message",
      target: pid,
      action: "Inc",
    })
    const {
      out: { results: { outbox } },
    } = await hb.get({
      path: `/${pid}/compute`,
      slot,
    })
    assert.equal(outbox["1"].data, "Count: 2")
  })
})
```

## Dryruns

HyperBEAM introduces the `patch@1.0` device and disables the traditional dryruns for performance reasons, but we can use the `call` method on the `relay@1.0` device to access the `http://localhost:6363/dry-run` endpoint on the local CU server.

```js
const { body } = await hb.post({
  path: "/~relay@1.0/call",
  method: "POST",
  "relay-path": `http://localhost:6363/dry-run?process-id=${pid}`,
  "content-type": "application/json",
  "relay-body": JSON.stringify({
    Tags: [{ name: "Action", value: "Get" }],
    Owner: addr,
  }),
})
assert.equal(JSON.parse(body).Messages[0].Data, "Count: 2")
```

## WAO SDK

The `HB` class has convenient methods for legacynet AOS. To write the same tests:

```js
const { pid } = await hb.spawnLegacy()
await hb.scheduleLegacy({ pid, data: src_data })
const { slot } = await hb.scheduleLegacy({ pid, action: "Inc" })
const res = await hb.computeLegacy({ pid, slot })
assert.equal(res.Messages[0].Data, "Count: 1")
const { res: res2 } = await hb.messageLegacy({ pid, action: "Inc" })
assert.equal(res2.Messages[0].Data, "Count: 2")
const res3 = await hb.dryrun({ pid, action: "Get" })
assert.equal(res3.Messages[0].Data, "Count: 2")
```

The `AO` class makes the code even more concise.

```js
import { AO } from "wao"

const ao = await new AO({
  module_type: "mainnet",
  hb: "http://localhost:10001",
}).init(jwk)
const { p } = await ao.deploy({ src_data })
await p.m("Inc")
assert.equal(await p.d("Get"), "Count: 1")
```
