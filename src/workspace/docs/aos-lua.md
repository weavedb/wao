# AOS Lua Handler Reference

Everything needed to write Lua handlers for AOS processes.

## Handler Registration

```lua
Handlers.add("Name", "Name", function(msg)
  -- handler logic
end)
```

- **Arg 1**: Handler name (string)
- **Arg 2**: Pattern — string (matches Action tag), table, or function
- **Arg 3**: Callback function receiving `msg`

### Pattern Types

```lua
-- String: matches Action tag
Handlers.add("Inc", "Inc", function(msg) ... end)

-- Table: matches multiple tags
Handlers.add("Transfer", { Action = "Transfer", Recipient = "_" }, function(msg) ... end)

-- Function: custom matching
Handlers.add("BigTransfer", function(msg)
  return msg.Action == "Transfer" and tonumber(msg.Quantity) > 1000
end, function(msg) ... end)
```

## msg Object

| Field | Description |
|-------|-------------|
| `msg.From` | Sender address |
| `msg.To` | Target process ID |
| `msg.Id` | Message ID |
| `msg.Data` | Message data (string) |
| `msg.Tags` | All tags table |
| `msg.Action` | Action tag value |
| `msg.{TagName}` | Tags are promoted to direct properties (e.g. `msg.Quantity`, `msg.Recipient`) |
| `msg.Timestamp` | Block timestamp |
| `msg["Block-Height"]` | Current block height |
| `msg.reply({...})` | Send reply to sender |
| `msg.forward(target)` | Forward to another process |

### Reply

```lua
msg.reply({ Data = "response text" })
msg.reply({ Data = json.encode(result), Tags = { Status = "ok" } })
```

## ao Globals

| Global | Description |
|--------|-------------|
| `ao.id` | Current process ID |
| `ao.send({...})` | Send message to another process |
| `ao.spawn(...)` | Spawn new process |
| `ao.env` | Process environment |

### Send (Fire-and-Forget)

```lua
ao.send({
  Target = recipientPid,
  Action = "Transfer",
  Quantity = "100",
  Data = "optional data"
})
```

Also available as global:

```lua
Send({
  Target = pid,
  Action = "Notify",
  Data = "Hello"
})
```

**CRITICAL: `Send().receive()` does NOT work with genesis-wasm on HyperBEAM.**
The external CU is single-pass with no coroutine/yield support. Use fire-and-forget `Send()` and handle responses in separate Handlers. Note: `Send().receive()` may work in in-memory AOS (legacynet) but avoid it for portability.

### Spawn and Assign (Globals)

```lua
-- Spawn a new process
Spawn(module, { Tags = { ... } })

-- Assign a message to a process
Assign({ Processes = { pid1, pid2 }, Message = msgId })
```

## State Patterns

### Idempotent Init

Variables at the top level persist across messages. Use `or` for idempotent initialization:

```lua
State = State or {}
count = count or 0
balances = balances or {}
```

### Global Variables Persist

```lua
count = 0  -- persists across all messages

Handlers.add("Inc", "Inc", function(msg)
  count = count + 1
  msg.reply({ Data = tostring(count) })
end)
```

## Action Tag — Case Sensitivity

**CRITICAL**: The Action tag MUST be uppercase `Action`, NOT lowercase `action`.

```lua
-- CORRECT
Handlers.add("Inc", "Inc", function(msg) ... end)
-- Matched by JS: p.m("Inc")

-- In JS tags, use uppercase:
await p.msg("Transfer", { Recipient: addr })  -- Action = "Transfer"
```

## JSON

```lua
local json = require('json')

-- Encode
local str = json.encode({ key = "value", count = 42 })

-- Decode
local tbl = json.decode(msg.Data)
```

## bint (Big Integer)

For token math with 256-bit precision:

```lua
local bint = require('.bint')(256)

local amount = bint(100)
local total = bint(1000)
local remaining = tostring(total - amount)  -- "900"
```

## Common Handler Patterns

### Counter

```lua
count = count or 0

Handlers.add("Inc", "Inc", function(msg)
  count = count + 1
  msg.reply({ Data = tostring(count) })
end)

Handlers.add("Dec", "Dec", function(msg)
  count = count - 1
  msg.reply({ Data = tostring(count) })
end)

Handlers.add("Get", "Get", function(msg)
  msg.reply({ Data = tostring(count) })
end)
```

### Key-Value Store

```lua
Store = Store or {}

Handlers.add("Set", "Set", function(msg)
  local key = msg.Tags.Key
  local value = msg.Data
  Store[key] = value
  msg.reply({ Data = "ok" })
end)

Handlers.add("Get", "Get", function(msg)
  local key = msg.Tags.Key
  msg.reply({ Data = Store[key] or "" })
end)

Handlers.add("Delete", "Delete", function(msg)
  Store[msg.Tags.Key] = nil
  msg.reply({ Data = "ok" })
end)

Handlers.add("List", "List", function(msg)
  local json = require('json')
  msg.reply({ Data = json.encode(Store) })
end)
```

### Registry

**Note:** Use tags (not `msg.Data`) for structured input — the `p.m()` shorthand passes additional fields as tags.

```lua
local json = require('json')
Registry = Registry or {}

Handlers.add("Register", "Register", function(msg)
  local name = msg.Tags.Name
  if not name then
    msg.reply({ Data = "error", Tags = { Error = "Name required" } })
    return
  end
  Registry[msg.From] = { name = name, role = msg.Tags.Role or "" }
  msg.reply({ Data = "registered" })
end)

Handlers.add("Lookup", "Lookup", function(msg)
  local target = msg.Tags.Target or msg.From
  local entry = Registry[target]
  if entry then
    msg.reply({ Data = json.encode(entry) })
  else
    msg.reply({ Data = "not_found", Tags = { Status = "not_found" } })
  end
end)

Handlers.add("List", "List", function(msg)
  msg.reply({ Data = json.encode(Registry) })
end)
```

### Token (AO Standard)

```lua
local json = require('json')
local bint = require('.bint')(256)

Name = Name or "My Token"
Ticker = Ticker or "TKN"
Denomination = Denomination or 12
Logo = Logo or ""
Balances = Balances or { [ao.id] = tostring(bint(10000) * bint(10) ^ bint(Denomination)) }

Handlers.add("Info", "Info", function(msg)
  msg.reply({
    Data = json.encode({
      Name = Name,
      Ticker = Ticker,
      Denomination = Denomination,
      Logo = Logo
    })
  })
end)

Handlers.add("Balance", "Balance", function(msg)
  local target = msg.Tags.Target or msg.From
  local bal = Balances[target] or "0"
  msg.reply({ Data = bal, Tags = { Balance = bal, Target = target, Ticker = Ticker } })
end)

Handlers.add("Balances", "Balances", function(msg)
  msg.reply({ Data = json.encode(Balances) })
end)

Handlers.add("Transfer", "Transfer", function(msg)
  local qty = bint(msg.Tags.Quantity)
  local sender_bal = bint(Balances[msg.From] or "0")
  if sender_bal < qty then
    msg.reply({ Tags = { Error = "Insufficient Balance" } })
    return
  end
  Balances[msg.From] = tostring(sender_bal - qty)
  local recipient = msg.Tags.Recipient
  Balances[recipient] = tostring(bint(Balances[recipient] or "0") + qty)
  -- Notify sender
  msg.reply({
    Tags = {
      Action = "Debit-Notice",
      Quantity = tostring(qty),
      Recipient = recipient
    }
  })
  -- Notify recipient
  ao.send({
    Target = recipient,
    Action = "Credit-Notice",
    Quantity = tostring(qty),
    Sender = msg.From
  })
end)

Handlers.add("Mint", "Mint", function(msg)
  if msg.From ~= ao.id then
    msg.reply({ Tags = { Error = "Unauthorized" } })
    return
  end
  local qty = bint(msg.Tags.Quantity)
  Balances[ao.id] = tostring(bint(Balances[ao.id] or "0") + qty)
  msg.reply({ Data = "Minted " .. tostring(qty) })
end)
```

### Chat System

```lua
local json = require('json')
Messages = Messages or {}
Members = Members or {}

Handlers.add("Join", "Join", function(msg)
  Members[msg.From] = msg.Tags.Name or msg.From
  msg.reply({ Data = "joined" })
end)

Handlers.add("Say", "Say", function(msg)
  if not Members[msg.From] then
    msg.reply({ Tags = { Error = "Not a member" } })
    return
  end
  table.insert(Messages, {
    from = Members[msg.From],
    text = msg.Data,
    timestamp = msg.Timestamp
  })
  -- Broadcast to all members
  for addr, _ in pairs(Members) do
    if addr ~= msg.From then
      ao.send({
        Target = addr,
        Action = "New-Message",
        Data = json.encode({
          from = Members[msg.From],
          text = msg.Data
        })
      })
    end
  end
  msg.reply({ Data = "sent" })
end)

Handlers.add("History", "History", function(msg)
  msg.reply({ Data = json.encode(Messages) })
end)
```

## Authorization Patterns

### Owner-Only

```lua
Handlers.add("Admin", "Admin", function(msg)
  if msg.From ~= ao.id then
    msg.reply({ Tags = { Error = "Unauthorized" } })
    return
  end
  -- admin logic
end)
```

### Allowlist

```lua
Admins = Admins or { [ao.id] = true }

Handlers.add("AdminAction", "AdminAction", function(msg)
  if not Admins[msg.From] then
    msg.reply({ Tags = { Error = "Unauthorized" } })
    return
  end
  -- admin logic
end)
```

## Lua Source in Tests

Lua files are loaded as strings and passed as `src_data`:

```js
import { readFileSync } from "fs"
import { resolve } from "path"

const src_data = readFileSync(
  resolve(import.meta.dirname, "../src/counter.lua"),
  "utf8"
)
const { p } = await ao.deploy({ src_data })
```

## Error Handling

```lua
-- Return error via tags
msg.reply({ Tags = { Error = "Something went wrong" } })

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

## Tips

- State is global — declare at top level with idempotent init (`X = X or default`)
- `msg.reply()` sends back to caller. `ao.send()` sends to any target
- Always use `tostring()` when replying with numbers
- JSON: `require('json')` — always available in AOS
- bint: `require('.bint')(256)` — for token-precision math
- Tables are the only complex data type — use them for everything
- `Send().receive()` is broken on genesis-wasm — use separate handlers instead
- Action tags are case-sensitive — always uppercase `Action`
