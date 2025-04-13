### Fork Wasm Memory

You can fork wasm memory to a new process. This could come in handy to create checkpoints for tests.

It only works with in-memory testing.

```js
const src_counter = `
local count = 0
Handlers.add("Add", "Add", function (msg)
  count = count + tonumber(msg.Plus)
end)
Handlers.add("Get", "Get", function (msg)
  msg.reply({ Data = tostring(count) })
end)
`
const ao = await new AO().init(acc[0])
const { p, pid } = await ao.deploy({ boot: true, src_data: src_counter })
await p.m("Add", { Plus: 3 })
assert.equal(await p.d("Get"), "3")

const ao2 = await new AO().init(acc[0])
// pass the exisiting wasm memory to a new process
const { p: p2 } = await ao2.spwn({ memory: ao.mem.env[pid].memory })
assert.equal(await p2.d("Get"), "3")
await p2.m("Add", { Plus: 2 })
assert.equal(await p2.d("Get"), "5")
```

You can also get mainnet process memory from the CU endpoint (`GET /state/{pid}`) and fork it for tests.

---

<nav style="display:flex;justify-content:space-between;">
  <a href="./logging.md">Logging</a>
  <a href="./weavedrive.md">WeaveDrive</a>
</nav>
