### Using WAO SDK

WAO comes with elegant syntactic sugar and makes writing AO projects an absolute joy.

The same test can be written as follows.

```js
import assert from "assert"
import { describe, it } from "node:test"
import { AO, acc } from "wao/test"

const src_data = `
Handlers.add("Hello", "Hello", function (msg)
  msg.reply({ Data = "Hello, World!" })
end)
`
describe("WAO", function () {
  it("should spawn a process and send messages", async () => {
    const ao = await new AO().init(acc[0])
    const { p } = await ao.deploy({ src_data })
    assert.equal(await p.d("Hello", false), "Hello, World!")
  })
})
```

The `AO` class is not only for in-memory tests, but also for production code. You just need to import from a different path.

```js
import { AR, AO, GQL } from "wao"
````

---

<nav style="display:flex;justify-content:space-between;">
  <a href="./write-tests.md">Writing Tests</a>
  <a href="./cherrypick.md">Cherry-Picking Results</a>
</nav>
