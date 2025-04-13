### Cherry-Picking Outputs

You often need to pick a specific piece of data from returned results with multiple spawned messages. You need to go through all the returned messages and further go through tags and data to find it. That's too much code to write. `AO` comes with `get` parameter to simplify it.

Consider the following Lua handlers.

```lua
local json = require('json')

Handlers.add("Hello", "Hello", function (msg)
  msg.reply({ Data = json.encode({ Name = "Bob" })})
end)

Handlers.add("Hello2", "Hello2", function (msg)
  msg.reply({ Data = "Hello, World!", Name = "Bob", Age = "30" })
end)

Handlers.add("Hello3", "Hello3", function (msg)
  msg.reply({ Profile = json.encode({ Name = "Bob", Age = "30" })})
end)

```

```js
// by default it extracts JSON decoded Data
const out = await p.d("Hello")
assert.deepEqual(out, { Name: "Bob" })

// equivalent
const out2 = await p.d("Hello", { get: true })
assert.deepEqual(out2, { Name: "Bob" })

// get string Data
const out3 = await p.d("Hello2", { get: false })
assert.equal(out3, "Hello, World!")

// get a tag
const out4 = await p.d("Hello2", { get: "Age" })
assert.equal(out4, "30")

// get multiple tags
const out5 = await p.d("Hello2", { get: { obj: { firstname: "Name", age: "Age" }}})
assert.deepEqual(out5, { firstname: "Bob", age: "30" })

// shortcut if keys don't include name, data, from, json
const out6 = await p.d("Hello2", { get: { firstname: "Name", age: "Age" }})
assert.deepEqual(out6, { firstname: "Bob", age: "30" })

// await p.d("Hello2", { get: { name: "Name", age: "Age" } }) doesn't work

// handle tag as json
const out7 = await p.d("Hello3", { get: { prof: { name: "Profile", json: true }}})
assert.deepEqual(out7, { prof: { Name: "Bob", Age: "30" }})

```

---

<nav style="display:flex;justify-content:space-between;">
  <a href="./use-sdk.md">Using WAO SDK</a>
  <a href="./check-response.md">Checking Responses</a>
</nav>
