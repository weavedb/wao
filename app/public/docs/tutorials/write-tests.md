### Writing Tests

Write a simple test in `test.js`.

```js
import assert from "assert"
import { describe, it } from "node:test"
import { connect, acc } from "wao/test"
const { spawn, message, dryrun } = connect()
const signer = acc[0].signer
const src_data = `
Handlers.add("Hello", "Hello", function (msg)
  msg.reply({ Data = "Hello, World!" })
end)
`
describe("WAO", function () {
  it("should spawn a process and send messages", async () => {
    const pid = await spawn({ 
      signer,
      module: "Do_Uc2Sju_ffp6Ev0AnLVdPtot15rvMjP-a9VVaA5fM",
      scheduler: "_GQ33BkPtZrqxA84vM8Zk-N2aO0toNNu_C-l-rawrBA"
    })

    // on mainnet, you need to wait here till the process becomes available.
    // WAO automatically handles it. No need with in-memory tests.
    // await wait({ pid })

    await message({
      process: pid,
      tags: [{ name: "Action", value: "Eval" }],
      data: src_data,
      signer,
    })
    const res = await dryrun({
      process: pid,
      tags: [{ name: "Action", value: "Hello" }],
      signer,
    })
    assert.equal(res.Messages[0].Data, "Hello, World!")
  })
})
```
Note that generating random Arweave wallets for every test takes time and slows down your test executions, so Wao connect provides pre-generated accounts for your tests, which saves hours if you are to run your tests thousands of times.

- `acc[0] = { jwk, addr, signer }`

Run the test.

```js
yarn test test/test.js
```

---

<nav style="display:flex;justify-content:space-between;">
  <a href="./setup-project.md">Setting up Project</a>
  <a href="./use-sdk.md">Using WAO SDK</a>
</nav>
