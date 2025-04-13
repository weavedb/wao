### Logging

WAO hot-patches the core AOS module code so `ao.log` automatically is forwarded to JS `console.log` and whatever you log will be directly displayed in your terminal. Lua tables will be auto-converted to JSON objects. It doesn't affect your production code, it only hot-paches the module during testing. This makes complex debugging so easy.

```lua
Handlers.add("Hello4", "Hello4", function (msg)
  ao.log("Hello, Wordl!") -- will be displayed in the terminal
  ao.log({ Hello = "World!" }) -- will be auto-converted to JSON
  
  -- passing multiple values 
  ao.log("Hi", 3, true, [ 1, 2, 3 ], { Hello = "World!" })
end)
```

You can get logs even when an error occurs in the handler, which is extremely handy to identify the error causes.

---

<nav style="display:flex;justify-content:space-between;">
  <a href="./async-receive.md">Async Receive</a>
  <a href="./fork-wasm-memory.md">Fork Processes</a>
</nav>
