### Local Persistent Server

You can run a local WAO server with persistent storage, which enables connections with outside components such as frontend apps.

```bash
npx wao
```

- `port` : Arweave port, the ports of AO units are based on this port (default to `4000`)
  - AR: [localhost:4000](http://localhost:4000)
  - MU: [localhost:4002](http://localhost:4002)
  - SU: [localhost:4003](http://localhost:4003)
  - CU: [localhost:4004](http://localhost:4004)  
- `db` : a directory to store data (default to `.cache`)
- `reset` : to reset the database

```bash
npx wao --port 5000 --db .custom_cache_dir --reset
```
In this case, the ports will be, AR => `5000`, MU => `5002`, SU => `5003`, CU => `5004`.

You can use WAO SDK or AOConnect to connect with the WAO units, but the following tags will be automatically set with WAO SDK.

- AOS2.0.1 Module: `Do_Uc2Sju_ffp6Ev0AnLVdPtot15rvMjP-a9VVaA5fM`
- Scheduler: `_GQ33BkPtZrqxA84vM8Zk-N2aO0toNNu_C-l-rawrBA`
- Authority: `eNaLJLsMiWCSWvQKNbk_YT-9ydeWl9lrWwXxLVp9kcg`

```js
import { describe, it } from "node:test"
import assert from "assert"
import { AO } from "wao"

const src_data = `
Handlers.add("Hello", "Hello", function (msg)
  msg.reply({ Data = "Hello, World!" })
end)`

describe("WAO Server", ()=>{
  it("should connect with WAO SDK", async ()=>{
    const ao = await new AO(4000).init(YOUR_JWK)
    const { p } = await ao.deploy({ src_data })
    assert.equal(await p.d("Hello"), "Hello, World!")
  })
})
```
With AOConnect,

```js
import { describe, it } from "node:test"
import assert from "assert"
import { connect, createDataItemSigner } from "@permaweb/aoconnect"
const { spawn, message, dryrun, assign, result } = connect({
  MU_URL: `http://localhost:4002`,
  CU_URL: `http://localhost:4003`,
  GATEWAY_URL: `http://localhost:4000`
})

const src_data = `
Handlers.add("Hello", "Hello", function (msg)
  msg.reply({ Data = "Hello, World!" })
end)`

describe("WAO Server", () => {
  it("should connect with WAO SDK", async () => {
    const pid = await spawn({
      module: "Do_Uc2Sju_ffp6Ev0AnLVdPtot15rvMjP-a9VVaA5fM",
      scheduler: "_GQ33BkPtZrqxA84vM8Zk-N2aO0toNNu_C-l-rawrBA",
      tags: [
        {
          name: "Authority",
          value: "eNaLJLsMiWCSWvQKNbk_YT-9ydeWl9lrWwXxLVp9kcg",
        },
      ],
      signer: createDataItemSigner(YOUR_JWK),
    })

    // wait till the process becomes available

    const mid = await message({
      process: pid,
      tags: [{ name: "Action", value: "Eval" }],
      data: src_data,
      signer: createDataItemSigner(acc[0].jwk),
    })

    console.log(await result({ process: pid, message: mid }))

    const res = await dryrun({
      process: pid,
      data: "",
      tags: [{ name: "Action", value: "Hello" }],
      anchor: "1234",
    })

    assert.equal(res.Messages[0].Data, "Hello, World!")
  })
})
```

Connecting with the AOS terminal,

```bash
aos \
  --gateway-url http://localhost:4000 \
  --cu-url http://localhost:4004 \
  --mu-url http://localhost:4002 \
  --tag-name Authority \
  --tag-value eNaLJLsMiWCSWvQKNbk_YT-9ydeWl9lrWwXxLVp9kcg
```

---

<nav style="display:flex;justify-content:space-between;">
  <a href="./weavedrive.md">WeaveDrive</a>
  <a href="./hyperbeam.md">HyperBEAM</a>
</nav>
