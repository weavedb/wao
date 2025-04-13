### WeaveDrive

The [WeaveDrive](https://hackmd.io/@ao-docs/H1JK_WezR) extension is fully emulated with WAO. You can use `attest` and `avail` functions from `AO`.

```js
import { blueprint, AO, acc } from "wao/test"
const attestor = acc[0]
const handler = `
apm.install('@rakis/WeaveDrive')
Drive = require('@rakis/WeaveDrive')
Handlers.add("Get", "Get", function (msg)
  msg.reply({ Data = Drive.getData(msg.id) })
end)`

describe("WeaveDrive", () => {
  it("should load Arweave tx data", async () => {
    const ao = await new AO().init(attestor)
	
    const { p } = await ao.deploy({
      tags: { Extension: "WeaveDrive", Attestor: attestor.addr },
      loads: [ await blueprint("apm"), handler ],
    })
	
    const { id } = await ao.ar.post({ data: "Hello" })
    await ao.attest({ id })
	
    assert.equal(await p.d("Get", { id }), "Hello")
  })
})
```

---

<nav style="display:flex;justify-content:space-between;">
  <a href="./fork-wasm-memory.md">Fork Processes</a>
  <a href="./local-server.md">Local Server</a>
</nav>
