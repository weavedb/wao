# Payment System

## faff@1.0

`faff@1.0` restricts node access to whitelisted accounts. You can pass a list of allowed accounts to your test `HyperBEAM` node. It only restricts `POST` requests. `GET` still works for all accounts.

The node operator can update `faff_allow_list` via `/~meta@1.0/info` to manage the allowed accounts.

```js [/test/payment-system.test.js]
import assert from "assert"
import { describe, it, before, after, beforeEach } from "node:test"
import { HyperBEAM, acc } from "wao/test"
import HB from "../../src/hb.js"
import { rsaid, hmacid } from "wao/utils"

const cwd = "../HyperBEAM"

describe("Payment System faff@1.0", function () {
  let hbeam, hb, operator
  let allowed_user = acc[0]
  let disallowed_user = acc[1]

  before(async () => {
    hbeam = await new HyperBEAM({
      cwd,
      reset: true,
      faff: [HyperBEAM.OPERATOR, allowed_user.addr],
    }).ready()
  })
  beforeEach(async () => {
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
      cwd,
      reset: true,
      operator: HyperBEAM.OPERATOR,
      simple_pay: true,
      simple_pay_price: 2,
    }).ready()
  })
  beforeEach(async () => {
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

`p4@1.0` allows you to use Lua scripts with `node-process@1.0` to manage node access.

`p4@1.0` requires a complex setup with a few hacks to test externally with JS code, so we'll go over it step by step.

### Required Configurations

`p4@1.0` requires a handful of configurations when starting up a HyperBEAM node with `hb:start_mainnet`.

- `on` : defines hooks
- `p4_non_chargable_routes` : defines free-of-charge endpoints
- `node_processes` : defines Lua scripts to be executed with hooks

Basically, we need to create 2 Lua scripts, one for the processor and the other for the clients, store them on Arweave or a local store to get the message IDs, then pass the IDs to the `on` settings of the hook device.

### Caching Lua Scripts

There are 3 ways to cache Lua scripts to use with `node-process@1.0`:

- upload to the production Arweave storage
- create a custom device method to internally cache with `hb_cache` on HyperBEAM
- create a process and upload with a message

We don't want to upload our test scripts to Arweave. So we're going with the 3rd hack since it produces the least conflict and the most flexibility without creating a custom HyperBEAM device. This only works with text-based scripts like Lua due to how messages are processed internally, but won't work for binary scripts like Wasm. For wasm modules for AOS processes, we need to go with the 2nd hack.

We need 2 Lua scripts (processor and client) for `p4@1.0` to work. For now, we can use the test Lua scripts HyperBEAM uses in their GitHub repo.

- [p4-payment-process.lua](https://github.com/permaweb/HyperBEAM/blob/main/scripts/p4-payment-process.lua)

```lua
--- A ledger that allows account balances to be debited and credited by a
--- specified address.

-- Check if the request is a valid debit/credit request by checking if one of
-- the committers is the operator.
local function is_valid_request(base, assignment)
    -- First, validate that the assignment is signed by the scheduler.
    local scheduler = base.scheduler
    local status, res = ao.resolve(assignment, "committers")
    ao.event({
        "assignment committers resp:",
        { status = status, res = res, scheduler = scheduler }
    })
    
    if status ~= "ok" then
        return false
    end

    local valid = false
    for _, committer in ipairs(res) do
        if committer == scheduler then
            valid = true
        end
    end

    if not valid then
        return false
    end

    -- Next, validate that the request is signed by the operator.
    local operator = base.operator
    status, res = ao.resolve(assignment.body, "committers")
    ao.event({
        "request committers resp:",
        { status = status, res = res, operator = operator }
    })

    if status ~= "ok" then
        return false
    end

    for _, committer in ipairs(res) do
        if committer == operator then
            return true
        end
    end

    return false
end

-- Debit the specified account by the given amount.
function debit(base, assignment)
    ao.event({ "process debit starting", { assignment = assignment } })
    if not is_valid_request(base, assignment) then
        base.result = { status = "error", error = "Operator signature required." }
        ao.event({ "debit error", base.result })
        return "ok", base
    end
    ao.event({ "process debit valid", { assignment = assignment } })
    base.balance = base.balance or {}
    base.balance[assignment.body.account] =
        (base.balance[assignment.body.account] or 0) - assignment.body.quantity
    
    ao.event({ "process debit success", { balances = base.balance } })
    return "ok", base
end

-- Credit the specified account by the given amount.
_G["credit-notice"] = function (base, assignment)
    ao.event({ "credit-notice", { assignment = assignment }, { balances = base.balance } })
    if not is_valid_request(base, assignment) then
        base.result = { status = "error", error = "Operator signature required." }
        return "ok", base
    end
    ao.event({ "is valid", { req = assignment.body } })
    base.balance = base.balance or {}
    base.balance[assignment.body.recipient] =
        (base.balance[assignment.body.recipient] or 0) + assignment.body.quantity
    ao.event({ "credit", { ["new balances"] = base.balance } })
    return "ok", base
end

--- Index function, called by the `~process@1.0` device for scheduled messages.
--- We route each to the appropriate function based on the request path.
function compute(base, assignment, opts)
    ao.event({ "compute", { assignment = assignment }, { balances = base.balance } })
    if assignment.body.path == "debit" then
        return debit(base, assignment.body)
    elseif assignment.body.path == "credit-notice" then
        return _G["credit-notice"](base, assignment.body)
    elseif assignment.body.path == "balance" then
        return balance(base, assignment.body)
    elseif assignment.slot == 0 then
        base.balance = base.balance or {}
        return "ok", base
    end
end
```

- [p4-payment-client.lua](https://github.com/permaweb/HyperBEAM/blob/main/scripts/p4-payment-client.lua)

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

-- Debit the user's balance in the current ledger state.
function debit(base, request)
    ao.event({ "client starting debit", { request = request, base = base } })
    local status, res = ao.resolve({
        path = "(" .. base["ledger-path"] .. ")/schedule",
        method = "POST",
        body = request
    })
    ao.event({ "client received schedule response", { status = status, res = res } })
    status, res = ao.resolve({
        path = base["ledger-path"] .. "/compute/balance/" .. request["account"],
        slot = res.slot
    })
    ao.event({ "confirmed balance", { status = status, res = res } })
    return "ok"
end

--- Poll an external ledger for credit events. If new credit noticess have been
--- sent by the external ledger, push them to the local ledger.
function poll(base, req)
    local status, local_last_credit = ao.resolve({
        path = base["ledger-path"] .. "/now/last-credit"
    })
    if status ~= "ok" then
        ao.event(
            { "error getting local last credit",
                { status = status, res = local_last_credit } }
        )
        return "error", base
    end

    local status, external_last_credit = ao.resolve({
        path = base["external-ledger"] .. "/now/last-credit"
    })
    if status ~= "ok" then
        ao.event({ "error getting external last credit",
            { status = status, res = external_last_credit } })
        return "error", base
    end

    ao.event({ "Retreived sync data. Last credit info:",
        {
            local_last_credit = local_last_credit,
            external_last_credit = external_last_credit }
        }
    )
    while local_last_credit < external_last_credit do
        status, res = ao.resolve({
            path = base["external-ledger"] .. "/push",
            slot = local_last_credit + 1
        })
        if status ~= "ok" then
            ao.event({ "error pushing slot", { status = status, res = res } })
            return "error", base
        end
        local_last_credit = local_last_credit + 1
    end

    return "ok", base
end
```

You can spawn a process and upload these 2 scripts as messages.

`schedule` only returns `slot` and not the message ID. You can get message IDs via `/~scheduler@1.0/schedule`, which returns a list of scheduled messages for the `target` process and contains IDs.

```js [/test/payment-system.test.js]
const process = hbeam.file("scripts/p4-payment-process.lua")
const { pid: cache_pid } = await hb.spawn({})
const { slot } = await hb.schedule({
  pid: cache_pid,
  data: process,
  "content-type": "application/lua",
})
const { body } = await hb.g("/~scheduler@1.0/schedule", {
  target: cache_pid,
  from: slot,
  accept: "application/aos-2",
})
const {
  edges: [msg],
} = JSON.parse(body)
const pid = msg.node.message.Id
assert(pid)
const client = hbeam.file("scripts/p4-payment-client.lua")
const { slot: slot2 } = await hb.schedule({
  pid: cache_pid,
  data: client,
  "content-type": "application/lua",
})
const { body: body2 } = await hb.g("/~scheduler@1.0/schedule", {
  target: cache_pid,
  from: slot2,
  accept: "application/aos-2",
})
const {
  edges: [msg2],
} = JSON.parse(body2)
const cid = msg2.node.message.Id
assert(cid)
```

`HB` has a convenient method for `/~scheduler@1.0/schedule`.

```js [/test/payment-system.test.js]
const msgs = await this.messages({ pid, from: slot, to: slot })
const pid = msgs.edges[0].node.message.Id

const msgs2 = await this.messages({ pid, from: slot2, to: slot2 })
const cid = msgs2.edges[0].node.message.Id
```

Indeed, it has a convenient method to cache scripts.

```js [/test/payment-system.test.js]
const process = readFileSync(`${hb_dir}/scripts/p4-payment-process.lua`)
const pid = await hb.cacheScript(process)

const client = readFileSync(`${hb_dir}/scripts/p4-payment-client.lua`)
const cid = await hb.cacheScript(client)
```

### Starting Another Node with p4@1.0

Once you get script IDs, you need to start another HyperBEAM node with the same store configurations. We can do this by simply instantiating another `HyperBEAM` with a different `port`, and without clearing the cache storage.

For `p4@1.0` to work, we need to pass the payment `operator` address and `p4_lua`, and the complex settings will be handled for you.

```js [/test/payment-system.test.js]
// comment out reset to use the same store where we cached Lua scripts
const hbeam2 = await new HyperBEAM({
  //reset: true,	
  cwd,
  port: 10002,
  operator: addr,
  p4_lua: { processor: pid, client: cid },
}).ready()
```

Now we have a new HyperBEAM node with the `p4@1.0` Lua scripts running at `http://localhost:10002`.

Let's set up 2 new HyperBEAM clients for the operator and a new user for the new node.

```js [/test/payment-system.test.js]
const operator = hbeam2
const user = acc[0]
user.hb = await new HB({ url: hbeam2.url }).init(user.jwk)
```

Now to topup the user account, we need to send `credit-notice` to the Lua script with the operator account. But this gets extra tricky since HyperBEAM first verifies the whole message sent to the node, then the Lua script extracts and verifies a nested message placed in `body`. So we need to somehow create a signed message with the correct commitment format internally used in HyperBEAM, then wrap that message in `body` and sign the parent message again. This is indeed extra complex, and the crux of this tutorial series where you need to put everything you learned so far together.

### Create Commitments

If you recall from the previous chapter, HyperBEAM internally signs and creates 2 commitments with sha256 hash of the signature and hmac-sha256 hash of the signed content. We can first construct this internal message to be passed to the Lua script.

```js [/test/payment-system.test.js]
const obj = {
  path: "credit-notice",
  quantity: 100,
  recipient: user.addr,
}
const lua_msg = await operator.hb.sign(obj)
```

Now we got a signed message with `path` included in `signature-input`.

```js
{
  url: 'http://localhost:10002/credit-notice',
  method: 'POST',
  headers: {
    quantity: '100',
    recipient: 'Rix7e0HB-8OAaimcoYkxTZB-dStgTOHWUik1DvKD5vM',
    'ao-types': 'quantity="integer"',
    path: 'credit-notice',
    signature: 'http-sig-bba7e22451416f77=:VhScT4geZWQi3JxJLUsMoH6qZlDgHjWiymRCu8aMZNgoDxodL3EiAdBUx9+8d6M45/gWejHIeRVjZCG414Xr9zEMsAIY1JUt6GIDB/frsxzK5arUAZ3BPJfmfMgZJ9a5MyWJ3OTtdcMoLgqSPlXOo+pDkFGVn0zyxqAkilgHVpAcFfnOcN+VPibWU0caSk5nt7iWYruAPRtcQVdG9xYqf8sj2ghlAQhoSZlhfkO8wNTA49EmzdPoYWcz7oTYXvqld64zIQLH0DlDPqcOXjFq4xFH9LWHvuglNQ2MNUBgDx0fUor4PXrBvXtR0uJDfhq5Y8gPwOmDf2wunA2mhRPKd8bVAJDkazIor5L2aXoE9SZxjOAb4VlmXEowRON5h4sm0+l1ARPjnWUbfrNZYweOp6YfvjnYE75QmO2MFBCrm6e9sbRf+/cdd+1SD533dkzEdQdGQ+eg6RRV5MGCBDMrZpfusws4C9M8/jxlExeQiI8CVLpQDFNWOJW7shwk4/KTuftgE023gKpEXDQL/b+RrIqeIAgevnrbmteb9htx8lFle9pv7g5UEACcLaY5os4Ocs9ZwsBSo52wLSXNAmtn5xQmxLlgOnsHZNMsar2jepLJHfKiS6VuTIj3k5KVBYaBjkQajcJWUttuJjHIxyfMr0mURHl4hcqXwfBFI00kerU=:',
    'signature-input': 'http-sig-bba7e22451416f77=("quantity" "recipient" "ao-types" "path");alg="rsa-pss-sha512";keyid="o1kvTqZQ0wbS_WkdwX70TFCk7UF76ldnJ85l8iRV7t6mSlzkXBYCecb-8RXsNEQQmO0KergtHOvhuBJmB6YXaYe_UftI_gendojfIa6jlTgw-qmH6g4_oErI8djDRbQSm-5nCfGVRuYxsNZLYDeqw4gFb9K3b1h7tuMoLd6-d5pkaLfTMUNcvs2OqpkLo0i_av746FieaURdWozwFqO0APtdA7pLHDqQZDMNdTmsUBJFszL6SOa1bKe5cUWnrq4uaW4NAN3JAQniILKGsKZENeKtfXwiKVaFJtriWWsbhOaNT0JLcuBAwXQAP59RXzcr8bRY6XFn8zBmEmZBGszOD9c9ssDENRFDa5uyVhk8XgIgQjErAWYd9T6edrYcIp3R78jhNK_nLiIBBz8_Oz3bLjL5i_aiV2gpfIbd44DCHihuuxSWRAPJxhEy9TS0_QbVOIWhcDTIeEJE3aRPTwSTMt1_Fec7i9HJWN0mvMbAAJw8k6HxjA3pFZiCowZJw7FBwMAeYgEwIeB82f-S2-PtFLwR9i0tExo36hEBHqaS4Y-O3NGgQ8mKnhT7Z1EfxEbA2BpR9oL8rJFEnPIrHHu7B88OHDDfnfRD3D79fKktnisC7XOuwbHG3TQo0_j4_mElH7xj_7IyAbmCUHDd-eRa482wOYXBB01DGnad901qaHU"'
  }
}
```

We can get the 2 hashes required to construct `commitments`.

```js [/test/payment-system.test.js]
import { rsaid, hmacid } from "wao/utils"

const hmacId = hmacid(lua_msg.headers)
const rsaId = rsaid(lua_msg.headers)
```

Now, we can construct `commitments` and the wrapped message.

```js [/test/payment-system.test.js]
const committed_lua_msg = {
	commitments: {
	  [ rsaId ]: {
        alg: "rsa-pss-sha512",
		"commitment-device": "httpsig@1.0",
		committer: operator.addr,
		signature: lua_msg.headers.signature,
		"signature-input": lua_msg.headers["signature-input"]
	  },
	  [ hmacId ]: {
        alg: "hmac-sha256",
		"commitment-device": "httpsig@1.0",
        signature: lua_msg.headers.signature,
		"signature-input": lua_msg.headers["signature-input"]
	  }
	},
	...obj
}
```

WAO has, of course, a convenient method to create commitment.

```js
const committed_lua_msg = await operator.hb.commit(obj, { path: true })
```

Finally, we can send it to `/ledger~node-process@1.0/schedule`.

```js [/test/payment-system.test.js]
await operator.hb.post({
  path: "/ledger~node-process@1.0/schedule",
  body: committed_lua_msg,
})
```

Let's check the balance of the user.

```js [/test/payment-system.test.js]
const { out: balance } = await operator.hb.get({
    path: `/ledger~node-process@1.0/now/balance/${user.addr}`,
})
assert.equal(balance, 100)
```	

Now try executing some messages with the user.

```js [/test/payment-system.test.js]
// this costs 3
const hello = { path: "/~message@1.0/set/hello", hello: "world" }
assert(await user.hb.post(hello))

const { out: balance2 } = await operator.hb.get({
    path: `/ledger~node-process@1.0/now/balance/${user.addr}`,
})
assert.equal(balance2, 97)

```
It works!!

Congratulations on having come this far!
The `p4@1.0` payment system with internal Lua scripts using `node-processes@1.0` is one of the most advanced usages of HyperBEAM to be tested externally. If you got this to work, most other things are less complex, so you should be ready to build anything on top of HyperBEAM now.

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
