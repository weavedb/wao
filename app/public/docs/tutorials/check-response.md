### Determining Message Success

To determine if your message is successful, you often need to track down a chain of asynchronous messages and examine resulted tags and data. This is actually a fairy complex operation and too much code to write. Luckily for you, `AO` comes with `check` parameter to extremely simplify it. `check` tracks down messages and lazy-evaluates if your `check` conditions are met.

```js
// check if Data exists
await p.m("Hello2", { check: true })

// check if Data is a certain value
await p.m("Hello2", { check: "Hello, World! })

// check if a tag exists
await p.m("Hello2", { check: "Name" })

// check if tags are certain values
await p.m("Hello2", { check: { Name: "Bob", Age: "30" } })

// it throws an Error if the conditions are not met
try{
  await p.m("Hello2", { check: { Name: "Bob", Age: "20" } })
}catch(e){
  console.log("something went wrong!")
}

// check if Name is Bob and Age exists, then get Age
const age = await p.m("Hello2", { check: { Name: "Bob", Age: true }, get : "Age" })
assert.equal(age, "30", "Bob is not 30 yo!")
```

---

<nav style="display:flex;justify-content:space-between;">
  <a href="./cherrypick.md">Cherry-Picking Results</a>
  <a href="./async-receive.md">Async Receive</a>
</nav>
