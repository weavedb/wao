### Async Message Tracking with receive()

AOS2 introduced a handy function `receive()` to send a message to another process and receive a reply in the same handler.

```lua
Handlers.add("Hello3", "Hello3", function (msg)
  msg.reply({ Data = "How old are you?" })
  local age = Send({
    Target = msg.To, Action = "Get-Age", Name = msg.Who
  }).receive().Data
  msg.reply({ Data = "got your age!", Name = msg.Who, Age = age })
end)
```

Since the second reply will be a part of another message triggerd by the `Target` process reply, you cannot get the final reply simply with the arconnect `result` function. You need to keep pinging the process `results` or track down the chain of messages to examine what went wrong. The AO `get` and `check` automatically handle this complex operation in a lazy short-circuit manner in the background for you. A proper `timeout` (ms) should be specified.

```js
const age = await p.m(
  "Hello3", 
  { Who: "Bob", To: DB_PROCESS_ID }, // second argument can be tags
  { get: "Age", check: "got your age!", timeout: 5000 }
)
assert.equal(age, "30")
```

There are so many more powerful tricks you can utilize to make complex AO development easier.

Read on to the API reference section to find out!

---

<nav style="display:flex;justify-content:space-between;">
  <a href="./check-response.md">Checking Responses</a>
  <a href="./logging.md">Logging</a>
</nav>
