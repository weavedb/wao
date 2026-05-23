# Payment System

## faff@1.0

`faff@1.0` restricts node access to whitelisted accounts. You can pass a list of allowed accounts to your test `HyperBEAM` node. It only restricts `POST` requests. `GET` still works for all accounts.

The node operator can update `faff_allow_list` via `/~meta@1.0/info` to manage the allowed accounts.

```js [/test/payment-system.test.js]
import assert from "assert"
import { describe, it, before, after } from "node:test"
import { HyperBEAM, acc } from "wao/test"
import { HB } from "wao"
import { rsaid, hmacid } from "hbsig"

describe("Payment System faff@1.0", function () {
  let hbeam, hb, operator
  let allowed_user = acc[0]
  let disallowed_user = acc[1]

  before(async () => {
    hbeam = await new HyperBEAM({
      reset: true,
      faff: [HyperBEAM.OPERATOR, allowed_user.addr],
    }).ready()
    operator = hbeam
    allowed_user.hb = new HB({ jwk: allowed_user.jwk })
    disallowed_user.hb = new HB({ jwk: disallowed_user.jwk })
  })
  after(async () => hbeam.kill())

  it("should test faff@1.0", async () => {
    const msg = ["/~message@1.0/set/hello", { hello: "world" }]

    // GET
    assert(await operator.hb.g(...msg))
    assert(await allowed_user.hb.g(...msg))
    assert(await disallowed_user.hb.g(...msg))

    // POST
    assert(await operator.hb.p(...msg))
    assert(await allowed_user.hb.p(...msg))
    await assert.rejects(disallowed_user.hb.p(...msg))

    const info = await operator.hb.g("/~meta@1.0/info")
    assert.deepEqual(info.faff_allow_list, [operator.addr, allowed_user.addr])

    // remove allowed_user
    await operator.hb.p("/~meta@1.0/info", { faff_allow_list: [operator.addr] })
    const info2 = await operator.hb.g("/~meta@1.0/info")
    assert.deepEqual(info2.faff_allow_list, [operator.addr])

    // now previously allowed_user fails too
    await assert.rejects(allowed_user.hb.p(...msg))
  })
})
```

## simple-pay@1.0

`simple-pay@1.0` allows you to set the base price for all requests.

You can set `simple_pay_price` and `simple_pay=true` on your test `HyperBEAM` node.

You also need to explicitly set the payment `operator` address who can change the payment settings.

The node operator can `topup` users and change the `simple_pay_price` via `/~meta@1.0/info`.

Users can view their own balances at `/~simple-pay@1.0/balance` with `POST`.

`simple-pay@1.0` uses `p4@1.0` underneath, which charges for all `POST` access except for the endpoints on the `p4_non_chargable_routes` list.

The `HyperBEAM` SDK automatically puts the following paths onto the `p4_non_chargable_routes` list, but you can also set it explicitly.

- `/~meta@1.0/*`
- `/~simple-pay@1.0/topup`
- `/~simple-pay@1.0/balance`

```js [/test/payment-system.test.js]
describe("Payment System simple-pay@1.0", function () {
  let hbeam, hb, operator
  let user = acc[0]
  before(async () => {
    hbeam = await new HyperBEAM({
      reset: true,
      operator: HyperBEAM.OPERATOR,
      simple_pay: true,
      simple_pay_price: 2,
    }).ready()
    operator = hbeam
    user.hb = await new HB({}).init(user.jwk)
  })
  after(async () => hbeam.kill())

  it("should test simple-pay@1.0", async () => {
    // cost = simplePayPrice * 3
    const msg = ["/~message@1.0/set/hello", { hello: "world" }]

    // balance is non_chargable
    const balance = "/~simple-pay@1.0/balance"

    // topup user
    await operator.hb.p("/~simple-pay@1.0/topup", {
      amount: 15,
      recipient: user.addr,
    })
    assert.equal(await user.hb.p(balance), "15")
    assert(await user.hb.p(...msg)) // cost = 2 * 3 = 6
    assert.equal(await user.hb.p(balance), "9")

    const info1 = await operator.hb.g("/~meta@1.0/info")
    assert.equal(info1.simple_pay_price, 2)

    // change simple_pay_price
    assert(await operator.hb.p("/~meta@1.0/info", { simple_pay_price: 3 }))

    const info2 = await operator.hb.g("/~meta@1.0/info")
    assert.equal(info2.simple_pay_price, 3)

    assert(await user.hb.p(...msg)) // cost = 3 * 3 = 9
    assert.equal(await user.hb.p(balance), "0")

    // this should fail for insufficient fund
    await assert.rejects(user.hb.p(...msg)) // cost = 3 * 3 = 9
  })
})
```

## p4@1.0

`p4@1.0` allows you to use Lua scripts with `node-process@1.0` to manage node access. The current v0.9-FINAL release uses the `hyper-token` script family, which provides a full token ledger with admin `charge` support for collecting fees.

### Required Configurations

`p4@1.0` requires the following when starting a HyperBEAM node:

- `operator` : the operator address who controls the node
- `p4_lua` : inline Lua scripts and configuration passed to the payment device

With the inline approach, you pass `p4_lua` an object containing:

- `processor` : an array of `{ body, name }` objects (the Lua scripts that run the ledger)
- `client` : a single `{ body, name }` object (the Lua script that marshals balance/charge requests)
- `admin` : the admin address authorized to charge accounts
- `balance` : a pre-loaded balance map of `{ [address]: amount }`

This eliminates the need to cache scripts on Arweave or spawn a separate process. The scripts are loaded inline when the node starts.

### Lua Scripts

`p4@1.0` uses 3 Lua scripts from the HyperBEAM repo. The processor is composed of 2 scripts loaded in order, and the client is a single script.

#### Processor Script 1: hyper-token.lua

- [hyper-token.lua](https://github.com/permaweb/HyperBEAM/blob/main/scripts/hyper-token.lua)

This is the main token ledger script. It implements a full AO token standard with support for sub-ledger networks. The key entry point is `compute()`, which routes actions like `transfer`, `credit-notice`, and `register` to the appropriate handlers.

```lua
--- ## HyperTokens: Networks of fungible, parallel ledgers.
--- An AO token standard implementation, with support for sub-ledger networks,
--- executed with the `~lua@5.3` device.

-- (utility functions: send, log_result, normalize_int, count_common, etc.)
-- (security functions: satisfies_list_constraints, is_trusted_compute, etc.)
-- (ledger functions: transfer, credit-notice, register, etc.)

--- Index function, called by the `~process@1.0` device for scheduled messages.
--- We route any `action' to the appropriate function based on the request path.
function compute(base, assignment)
    ao.event({ "compute called",
        { balance = base.balance, ledgers = base.ledgers } })

    assignment.body.action = string.lower(assignment.body.action or "")

    if assignment.body.action == "credit-notice" then
        return _G["credit-notice"](base, assignment)
    elseif assignment.body.action == "transfer" then
        return transfer(base, assignment)
    elseif assignment.body.action == "register" then
        return register(base, assignment)
    elseif assignment.body.action == "register-remote" then
        return _G["register-remote"](base, assignment)
    else
        -- Handle unknown `action' values.
        _, base = ensure_initialized(base, assignment)
        base.results = {
            status = "ok"
        }
        ao.event({ "Process initialized.", { slot = assignment.slot } })
        return "ok", base
    end
end
```

#### Processor Script 2: hyper-token-p4.lua

- [hyper-token-p4.lua](https://github.com/permaweb/HyperBEAM/blob/main/scripts/hyper-token-p4.lua)

This extends `hyper-token.lua` by adding a `charge` function that allows an `admin` account to debit a user and credit a recipient. This is how the node operator collects fees.

```lua
--- An extension to the `hyper-token.lua` script, for execution with the
--- `lua@5.3a` device. This script adds the ability for an `admin' account to
--- charge a user's account. This is useful for allowing a node operator to
--- collect fees from users, if they are running in a trusted execution
--- environment.
---
--- This script must be added as after the `hyper-token.lua` script in the
--- `process-definition`s `script` field.

-- Process an `admin' charge request:
-- 1. Verify the sender's identity.
-- 2. Ensure that the quantity and account are present in the request.
-- 3. Debit the source account.
-- 4. Increment the balance of the recipient account.
function charge(base, assignment)
    ao.event("debug_charge", { "Charge received: ", { assignment = assignment } })
    local admin = base.admin
    local status, res, request = validate_request(base, assignment)
    if status ~= "ok" then
        return status, res
    end

    -- Verify that the request is signed by the admin.
    local committers = ao.get("committers", {"as", "message@1.0", assignment.body})
    ao.event("debug_charge", { "Validating request: ", {
        committers = committers,
        admin = admin
    } })
    if count_common(committers, admin) ~= 1 then
        return "error", base
    end

    -- Ensure that the quantity and account are present in the request.
    if not request.quantity or not request.account then
        ao.event({ "Failure: Quantity or account not found in request.",
            { request = request } })
        base.result = {
            status = "error",
            error = "Quantity or account not found in request."
        }
        return "ok", base
    end

    -- Debit the source. Note: We do not check the source balance here, because
    -- the node is capable of debiting the source at-will -- even it puts the
    -- source into debt. This is important because the node may estimate the
    -- cost of an execution at lower than its actual cost. Subsequently, the
    -- ledger should at least debit the source, even if the source may not
    -- deposit to restore this balance.
    ao.event({ "Debit request validated: ", { assignment = assignment } })
    base.balance = base.balance or {}
    base.balance[request.account] =
        (base.balance[request.account] or 0) - request.quantity

    -- Increment the balance of the recipient account.
    base.balance[request.recipient] =
        (base.balance[request.recipient] or 0) + request.quantity

    ao.event("debug_charge", { "Charge processed: ", { balances = base.balance } })
    return "ok", base
end
```

#### Client Script: hyper-token-p4-client.lua

- [hyper-token-p4-client.lua](https://github.com/permaweb/HyperBEAM/blob/main/scripts/hyper-token-p4-client.lua)

The client script marshals `balance` and `charge` requests from `p4@1.0` to the local `node-process@1.0` ledger. Note that this client uses `charge` (not `debit`) to match the `hyper-token-p4.lua` processor.

```lua
--- A simple script that can be used as a `~p4@1.0` ledger device, marshalling
--- requests to a local process.

-- Find the user's balance in the current ledger state.
function balance(base, request)
    local status, res = ao.resolve({
        path =
            base["ledger-path"]
            .. "/now/balance/"
            .. request["target"]
    })
    ao.event({ "client received balance response",
        { status = status, res = res, target = request["target"] } }
    )
    -- If the balance request fails (most likely because the user has no balance),
    -- return a balance of 0.
    if status ~= "ok" then
        return "ok", 0
    end

    -- We have successfully retrieved the balance, so return it.
    return "ok", res
end

-- Charge the user's balance in the current ledger state.
function charge(base, request)
    ao.event("debug_charge", {
        "client starting charge",
        { request = request, base = base }
    })
    local status, res = ao.resolve({
        path = "(" .. base["ledger-path"] .. ")/push",
        method = "POST",
        body = request
    })
    ao.event("debug_charge", {
        "client received charge response",
        { status = status, res = res }
    })
    return "ok", res
end
```

### Starting a Node with p4@1.0

With the inline approach, you read the 3 Lua scripts, compute the operator address from the HyperBEAM wallet, and start a single HyperBEAM node with everything configured via `p4_lua`.

```js [/test/payment-system.test.js]
import { readFileSync } from "fs"
import { resolve } from "path"
import { HyperBEAM, acc, toAddr } from "wao/test"
import { HB } from "wao"

const user = acc[0]
const hbDir = resolve(import.meta.dirname, "../../HyperBEAM")

// Read the operator wallet to get admin address
const operatorJwk = JSON.parse(
  readFileSync(resolve(hbDir, ".wallet.json"), "utf8")
)
const operatorAddr = toAddr(operatorJwk.n)

// Read the 3 Lua scripts
const tokenScript = readFileSync(
  resolve(hbDir, "scripts/hyper-token.lua"), "utf8"
)
const p4Script = readFileSync(
  resolve(hbDir, "scripts/hyper-token-p4.lua"), "utf8"
)
const clientScript = readFileSync(
  resolve(hbDir, "scripts/hyper-token-p4-client.lua"), "utf8"
)

// Start a SINGLE HyperBEAM node with inline p4_lua config
const hbeam = await new HyperBEAM({
  reset: true,
  operator: HyperBEAM.OPERATOR,
  p4_lua: {
    processor: [
      { body: tokenScript, name: "hyper-token.lua" },
      { body: p4Script, name: "hyper-token-p4.lua" },
    ],
    client: { body: clientScript, name: "hyper-token-p4-client.lua" },
    admin: operatorAddr,
    balance: { [user.addr]: 1000 },
  },
}).ready()

user.hb = await new HB({ url: hbeam.url }).init(user.jwk)
```

The `processor` is an array of 2 script objects loaded in order: first `hyper-token.lua` (the base token ledger), then `hyper-token-p4.lua` (the charge extension). The `client` is a single script object. The `admin` address is derived from the HyperBEAM wallet and authorizes charge operations. The `balance` map pre-loads the user with 1000 tokens.

### Checking Balance and Executing Requests

Let's check the pre-loaded balance of the user via the ledger process.

```js [/test/payment-system.test.js]
const operator = hbeam
const balance = await operator.hb.g(
  `/ledger~node-process@1.0/now/balance/${user.addr}`
)
assert.equal(Number(balance), 1000)
```

Now try executing a POST request as the user. Each request costs tokens (default pricing).

```js [/test/payment-system.test.js]
// POST as user - this will be charged
assert(await user.hb.p("/~message@1.0/set/hello", { hello: "world" }))

// Check that balance decreased
const balance2 = await operator.hb.g(
  `/ledger~node-process@1.0/now/balance/${user.addr}`
)
assert(Number(balance2) < 1000)
```

The user's balance decreased after the POST request. The `p4@1.0` device automatically called the `charge` function in the client script, which forwarded the charge to the processor, debiting the user's account.

Congratulations on having come this far! The `p4@1.0` payment system with inline Lua scripts using `node-process@1.0` is one of the most advanced usages of HyperBEAM. With the inline approach in v0.9-FINAL, the setup is significantly simpler -- no need to cache scripts or start a second node. If you got this to work, most other things are less complex, so you should be ready to build anything on top of HyperBEAM now.

## Dynamic Pricing — `metering@1.0` (v0.9-FINAL)

v0.9-FINAL ships a new `metering@1.0` pricing device for `p4@1.0` that records actual resource usage (Arweave bytes, BEAM reductions, etc.) over the request/response lifecycle instead of charging a flat price.

The operator sets `metering-rates` in the node message — a map of resource name to AO token units per resource unit:

```js
hbeam = await new HyperBEAM({
  operator: HyperBEAM.OPERATOR,
  on: {
    request: { device: "p4@1.0", "pricing-device": "metering@1.0", "ledger-device": "simple-pay@1.0" },
    response: { device: "p4@1.0", "pricing-device": "metering@1.0", "ledger-device": "simple-pay@1.0" },
  },
  "metering-rates": { "arweave-bytes": 1, "beam-reductions": 0 },
}).ready()
```

`metering@1.0`'s `estimate/3` opens a process-local session, `consume/3` increments usage during evaluation, and `price/3` closes the session and returns the integer charge. Calls to `consume/3` outside an active session are no-ops, so device authors don't need to check whether metering is enabled.

## Running Tests

You can find the working test file for this chapter here:

- [payment-system.test.js](https://github.com/weavedb/wao/blob/master/dhfs-tutorial-app/test/payment-system.test.js)

Run tests:

```bash [Terminal]
yarn test test/payment-system.test.js
```

## References

##### Device Docs

- [Device: ~lua@5.3a](https://hyperbeam.ar.io/build/devices/lua-at-5-3a.html)

##### Device API

- [dev_faff.erl](https://hyperbeam.ar.io/build/devices/source-code/dev_faff.html)
- [dev_simple_pay.erl](https://hyperbeam.ar.io/build/devices/source-code/dev_simple_pay.html)
- [dev_p4.erl](https://hyperbeam.ar.io/build/devices/source-code/dev_p4.html)
- [dev_node_process.erl](https://hyperbeam.ar.io/build/devices/source-code/dev_node_process.html)
- [dev_lua.erl](https://hyperbeam.ar.io/build/devices/source-code/dev_lua.html)

##### WAO API

- [HyperBEAM Class API](/api/hyperbeam)
- [HB Class API](/api/hb)
- [HBSig API](/api/hbsig)
