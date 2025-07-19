# Legacynet Compatible AOS

Legacynet compatible AOS uses `genesis-wasm@1.0` to delegate compute to an external local CU. You can use wasm modules stored on the Arweave Mainnet storage, or you could create a helper method to locally store wasm modules for testing. You should also pass `as = ["genesis_wasm"]` to the test HyperBEAM node to auto-start a local CU server with HyperBEAM.

Let's use the production AOS2.0.6 module stored at `ISShJH1ij-hPPt9St5UFFr_8Ys3Kj5cyg7zrMGt7H9s` for now.

Currently, nesting messages don't work with legacynet compatible AOS. HyperBEAM automatically replaces `path` with `schedule` for process-based messages, which invalidates signatures. To circumvent this, we need to exclude `path` from the signature base with `{ path: false }`.

```js [/test/legacynet-aos.test.js]
import assert from "assert"
import { describe, it, before, after, beforeEach } from "node:test"
import { HyperBEAM } from "wao/test"

const cwd = "../HyperBEAM"

const seed = num => {
  const array = new Array(num)
  for (let i = 0; i < num; i++) array[i] = Math.floor(Math.random() * 256)
  return Buffer.from(array).toString("base64")
}

const data = `
local count = 0
Handlers.add("Inc", "Inc", function (msg)
  count = count + 1
  msg.reply({ Data = "Count: "..tostring(count) })
end)

Handlers.add("Get", "Get", function (msg)
  msg.reply({ Data = "Count: "..tostring(count) })
end)`

describe("Processes and Scheduler", function () {
  let hbeam, hb
  before(async () => {
    hbeam = await new HyperBEAM({
      cwd,
      reset: true,
      as: ["genesis_wasm"],
    }).ready()
  })
  beforeEach(async () => (hb = hbeam.hb))
  after(async () => hbeam.kill())

  it("should spawn a legacynet AOS process", async () => {
    const { process: pid } = await hb.p(
      "/schedule",
      {
        device: "process@1.0",
        type: "Process",
        "data-protocol": "ao",
        variant: "ao.TN.1",
        scheduler: hb.addr,
        "scheduler-location": hb.addr,
        authority: hb.addr,
        "random-seed": seed(16),
        module: "ISShJH1ij-hPPt9St5UFFr_8Ys3Kj5cyg7zrMGt7H9s",
        "scheduler-device": "scheduler@1.0",
        "execution-device": "stack@1.0",
        "device-stack": ["genesis-wasm@1.0", "patch@1.0"],
        "push-device": "push@1.0",
        "patch-from": "/results/outbox",
      },
      { path: false }
    )

    await hb.p(
      `/${pid}/schedule`,
      { type: "Message", target: pid, action: "Eval", data },
      { path: false }
    )

    await hb.p(
      `/${pid}/schedule`,
      { type: "Message", target: pid, action: "Inc" },
      { path: false }
    )
    const { slot } = await hb.p(
      `/${pid}/schedule`,
      { type: "Message", target: pid, action: "Inc" },
      { path: false }
    )
    const { results } = await hb.g(`/${pid}/compute`, { slot })
    assert.equal("Count: 2", results.outbox["1"].data)

    const { body } = await hb.post({
      path: "/~relay@1.0/call",
      method: "POST",
      "relay-path": `http://localhost:6363/dry-run?process-id=${pid}`,
      "content-type": "application/json",
      "relay-body": JSON.stringify({
        Tags: [{ name: "Action", value: "Get" }],
        Owner: hb.addr,
      }),
    })
    assert.equal(JSON.parse(body).Messages[0].Data, "Count: 2")
  })
})
```

## Dryruns

HyperBEAM introduces the `patch@1.0` device and disables the traditional dryruns for performance reasons, but we can use the `call` method on the `relay@1.0` device to access the `http://localhost:6363/dry-run` endpoint on the local CU server.

```js [/test/legacynet-aos.test.js]
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

```js [/test/legacynet-aos.test.js]
const { pid } = await hb.spawnLegacy()
await hb.scheduleLegacy({ pid, data })
const { slot } = await hb.scheduleLegacy({ pid, action: "Inc" })
const res = await hb.computeLegacy({ pid, slot })
assert.equal(res.Messages[0].Data, "Count: 1")
const { res: res2 } = await hb.messageLegacy({ pid, action: "Inc" })
assert.equal(res2.Messages[0].Data, "Count: 2")
const res3 = await hb.dryrun({ pid, action: "Get" })
assert.equal(res3.Messages[0].Data, "Count: 2")
```

The `AO` class makes the code even more concise.

```js [/test/legacynet-aos.test.js]
import { AO } from "wao"

const ao = await new AO({ module_type: "mainnet", hb: hbeam.url }).init(
  hbeam.jwk
)
const { p } = await ao.deploy({ src_data: data })
await p.m("Inc")
assert.equal(await p.d("Get"), "Count: 1")
await p.m("Inc")
assert.equal(await p.d("Get"), "Count: 2")
```

## Running Tests

You can find the working test file for this chapter here:

- [legacynet-aos.test.js](https://github.com/weavedb/wao/blob/master/dhfs-tutorial-app/test/legacynet-aos.test.js)

Run tests:

```bash [Terminal]
yarn test test/legacynet-aos.test.js
```

## References

##### General

- [Building ao Processes](https://hyperbeam.ar.io/build/building-on-ao.html)
- [AO Cookbook](https://cookbook_ao.arweave.net/)
  
##### Device Docs

- [Device: ~relay@1.0](https://hyperbeam.ar.io/build/devices/relay-at-1-0.html)
  
##### Device API

- [dev_stack.erl](https://hyperbeam.ar.io/build/devices/source-code/dev_stack.html)
- [dev_message.erl](https://hyperbeam.ar.io/build/devices/source-code/dev_message.html)
- [dev_process.erl](https://hyperbeam.ar.io/build/devices/source-code/dev_process.html)
- [dev_scheduler.erl](https://hyperbeam.ar.io/build/devices/source-code/dev_scheduler.html)
- [dev_patch.erl](https://hyperbeam.ar.io/build/devices/source-code/dev_patch.html)
- [dev_relay.erl](https://hyperbeam.ar.io/build/devices/source-code/dev_relay.html)
- [dev_genesis_wasm.erl](https://hyperbeam.ar.io/build/devices/source-code/dev_genesis_wasm.html)
  
##### WAO API

- [AO Class API](/api/ao)
- [Process Class API](/api/process)
- [HyperBEAM Class API](/api/hyperbeam)
- [HB Class API](/api/hb)
