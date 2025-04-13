### HyperBEAM

HyperBEAM integration is highly experimental at this stage.

Clone WAO and install the dependencies.

```bash
git clone https://github.com/weavedb/wao.git && cd wao && yarn
```

Run a WAO server with in-memory mode (the persistent-mode with HB is not well-tested yet).

```bash
yarn server --memory --hb http://localhost:10001
```

The CU should be running at [http://localhost:4004](http://localhost:4004).

Make sure you followed [the Hyperbeam setup guide](https://permaweb.github.io/HyperBEAM/hyperbeam/) and successfully compiled the latest version.

But don't run `rebar3 shell` just yet.

Instead, start a Hyperbeam node with the following command from the WAO root directory.

```bash
yarn hb PATH_TO_HB_DIR
```

The HyperBEAM should be running at [http://localhost:10001](http://localhost:10001).

You can test HyperBEAM requests with the WAO server as a local CU.

```js
import assert from "assert"
import { after, describe, it } from "node:test"
import { acc } from "wao/test"
import { HB } from "wao"

const data = `
local count = 0
Handlers.add("Inc", "Inc", function (msg)
  count = count + 1
  msg.reply({ Data = "Count: "..tostring(count) })
end)

Handlers.add("Get", "Get", function (msg)
  msg.reply({ Data = "Count: "..tostring(count) })
end)
`
describe("Hyperbeam", function () {
  it("should interact with a hyperbeam node", async () => {
    const hb = await new HB({ url: "http://localhost:10001" }).init(acc[0].jwk)
    const metrics = await hb.metrics()
    const info = await hb.info()
    const process = await hb.process()
    const slot = await hb.schedule({ process, data })
    const r = await hb.compute({ process, slot })
    const slot2 = await hb.schedule({ process, action: "Inc" })
    const r2 = await hb.compute({ process, slot: slot2 })
    assert.equal(r2.Messages[0].Data, "Count: 1")
    const r3 = await hb.dryrun({ process, action: "Get" })
	assert.equal(r3.Messages[0].Data, "Count: 1")
  })
})
```

---

<nav style="display:flex;justify-content:space-between;">
  <a href="./local-server.md">Local Server</a>
  <a href="../api/README.md">API Reference</a>
</nav>

