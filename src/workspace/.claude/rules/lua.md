---
paths:
  - "src/**/*.lua"
---

# AOS Script Rules

For full reference, read `docs/aos-lua.md`.

## AOS Script Pattern

```lua
Handlers.add("Name", "Name", function(msg)
  -- script logic
  msg.reply({ Data = "response" })
end)
```

The first argument is the name, the second is the action tag to match, and the third is the callback.

## State Persistence

Declare state at the top level so it persists across messages:

```lua
State = State or {}
count = count or 0
```

## Common Patterns

- **Reply**: `msg.reply({ Data = "..." })` or `msg.reply({ Data = json.encode(tbl) })`
- **Sender address**: `msg.From`
- **Process ID**: `ao.id` (this is the process ID, NOT the deployer's address)
- **Message tags**: `msg.Tags`, `msg.Action`, `msg.Tags.SomeTag` (tags are also promoted as direct properties: `msg.Quantity`, `msg.Recipient`)
- **Send to other process**: `ao.send({ Target = pid, Action = "Name" })` or `Send({...})`
- **Forward**: `msg.forward(targetPid)`

## CRITICAL: Send().receive() Does NOT Work with genesis-wasm

**Never use `Send().receive()`** in genesis-wasm/HyperBEAM contexts — the external CU is single-pass with no coroutine support. Use fire-and-forget `Send()` + separate Handlers:

```lua
-- WRONG: hangs/fails
local res = Send({ Target = pid, Action = "X" }).receive()

-- CORRECT: fire-and-forget
Send({ Target = pid, Action = "X" })
-- Handle response in a separate Handlers.add
```

## Action Tag — Must Be Uppercase

**CRITICAL**: Use `Action` (uppercase), not `action`. Handlers match on the uppercase `Action` tag.

## JSON

```lua
local json = require('json')
json.encode(table)    -- table → string
json.decode(string)   -- string → table
```

## bint (Token Math)

```lua
local bint = require('.bint')(256)
local amount = bint(100)
local total = bint("1000000000000")
local result = tostring(total - amount)
```

## Blueprint Patterns

- **Token**: Transfer, Balance, Balances, Mint, Info (uses bint)
- **Registry**: Register, Lookup, List
- **Counter**: Inc, Dec, Get
- **Key-Value Store**: Set, Get, Delete, List
- **Chat**: Join, Say, History

See `docs/aos-lua.md` for complete implementations.

## State Validation

```lua
-- Validate input
local qty = tonumber(msg.Tags.Quantity)
if not qty or qty <= 0 then
  msg.reply({ Tags = { Error = "Invalid quantity" } })
  return
end

-- Safe JSON decode
local ok, data = pcall(json.decode, msg.Data)
if not ok then
  msg.reply({ Tags = { Error = "Invalid JSON" } })
  return
end
```

## Lua Source in Tests

Lua files are read as strings and passed as `src_data` when deploying in tests:

```js
const src_data = readFileSync(resolve(import.meta.dirname, "../src/file.lua"), "utf8")
const { p } = await ao.deploy({ src_data })
```
