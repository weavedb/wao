### AO

- [Instantiate](#instantiate-1)
- [deploy](#deploy)
- [msg](#msg)
- [dry](#dry)
- [res](#res)
- [ress](#ress)
- [asgn](#asgn)
- [load](#load)
- [eval](#eval)
- [spwn](#spwn)
- [aoconnect Functions](#aoconnect-functions)
- [postModule](#postmodule)
- [postScheduler](#postscheduler)
- [attest](#attest)
- [avail](#avail)
- [wait](#wait)
- [var](#var)

#### Instantiate

You can initialize AO in the same way as AR.

```js
import { AO } from "wao"
const ao = await new AO().init(jwk || arweaveWallet)
```
If you need to pass AR settings, use `ar`. `ao.ar` will be automatically available.

```js
const ao = await new AO({ ar: { port: 4000 }}).init(jwk || arweaveWallet)
const addr = ao.ar.addr
await ao.ar.post({ data, tags })
```

#### AO Core Functions

##### deploy

Spawn a process, get a Lua source, and eval the script. `src` is an Arweave txid of the Lua script.

```js
const { err, res, pid, p } = await ao.deploy({ data, tags, src, fills })
```

You can directly pass the Lua script with `src_data` instead of `src`.

```js
const { err, res, pid, p } = await ao.deploy({ data, tags, src_data, fills })
```

`boot` will use `On-Boot` tag to initialize the process instead of `Eval` action. You can set either `true` to use `src_data`, or set a txid of an existing script. In case of `true`, `data` should be undefined so the `src_data` can fill it with `spawn`.

```js
const { err, res, pid, p } = await ao.deploy({ boot: true, tags, src_data, fills })
```

`fills` replace the Lua source script from `src`.

```lua
local replace_me = '<REPLACE_ME>'
local replace_me_again = '<REPLACE_ME_AGAIN>'
local replace_me_with_hello_again = '<REPLACE_ME>'
```

```js
const fills = { REPLACE_ME: "hello", REPLACE_ME_AGAIN: "world" }
```
This will end up in the following lua script.

```lua
local replace_me = 'hello'
local replace_me_again = 'world'
local replace_me_with_hello_again = 'hello'
```

In case you have multiple scripts, use `loads` and pass `src` and `fills` respectively.

```js
await ao.deploy({ tags, loads: [ { src, fills }, { src: src2, fills: fills2 } ] })
```

You can also pass an array of string data to `loads`.


```js
const num = `num = 0`
const inc = `Handlers.add("Inc", "Inc", function () num = num + 1 end)`
const get = `Handlers.add("Get", "Get", function (m) m.reply({ Data = num }) end)`

const { p } = await ao.deploy({ tags, loads: [ num, inc, get ] })
await p.m("Inc")
assert.equal(await p.d("Get"), 1)
```

##### msg

Send a message.


```js
const { err, mid, res, out } = await ao.msg({
  pid, data, act, tags, check, get, mode, limit
})
```

`check` determins if the message call is successful by checking through `Tags` in `Messages` in `res`.

When using `from` either in `check` or `get`, `mode` needs to be set `gql`. `mode` defaults to `aoconnect` which uses the `aoconnect.results` function to track down results, which cannot tell where results come from. `gql` mode doesn't sometimes catch all results if used with AO/Arweave mainnet since there are lags due to the block finality time. `limit` specifies how many transactions or results to fetch for the `check`.

```js
const check = { Status : "Success" } // succeeds if Status tag is "Success"
const check2 = { Status : true } // succeeds if Status tag exists
```


Assigning either a string or boolean value checks `Data` field instead of `Tags`.

```js
const check3 = "Success"  // succeeds if Data field is "Success"
const check4 = true // succeeds if Data field exists
const check5 = /ok/ // succeeds if Data field is string containing "ok"
const check6 = (n)=> +n > 10 // succeeds if Data field is bigger than 10
const check7 = { json: { a: 3 } } // succeeds if Data field is JSON and a is 3
const check8 = { json: { a: 3, b: 4 }, eq: true } // deep equal JSON
const check9 = { data: true, tags: { Status: true, Balance: (n)=> +n > 10 } }
const check10 = { data: true, from: PID } // specify message sender process
```

Use an array to check multiple conditions.

```js
const check11 = ["Success", { Age: "30" }] // Data is Success and Age tag is 30
const check12 = [{ data: "Success", from: PID }, { Age: "30", from: PID2 }]
```

`get` will return specified data via `out`.

```js
const get = "ID" // returns the value of "ID" tag
const get2 = { name: "Profile", json: true } // "Profile" tag with JSON.parse()
const get3 = { data: true, json: true } // returns Data field with JSON.parse()
const get4 = true // => { data: true, json: true }
const get5 = false // => { data: true, json: false }
const get6 = { obj: { age: "Age", who: "Name" }} // => { age: 30, who: "Bob" }
const get7 = { age: "Age", who: "Name" } // same as get6
const get8 = { name: "Profile", json: true, from: PID } // specify sender process
const get9 = { age: { name: "Age", from: PID }, who: "Name" } // another example
const get10 = { data: true, json: true, match: (val, index, res)=> val.Age < 10 }
```

`check` and `get` lazy-evaluate tags and data by tracking down async messages. As soon as the conditions are met, they won't track further messages. With `receive()` added with AOS 2.0, you can only get spawned messages up to the `receive()` function from `result`. But WAO automatically tracks down further messages and determines if check conditions are met beyond the `receive()` function. 

For example, consider the following handler.

```js
Handlers.add("Hello", "Hello", function (msg)
  msg.reply({ Data = "Hello, World!" })
  local name = Send({ Target = msg.to, Action = "Reply" }).receive().Data
  msg.reply({ Data = "Hello, " .. name .. "!" })
end)
```

you can only get the first `Hello, World!`, but not the second `"Hello, " .. name .. "!"` from the aoconnect `result` function.

##### dry

Dryrun a message without writing to Arweave.


```js
const { err, res, out } = await ao.dry({ pid, data, action, tags, check, get })
```

##### res

`res` does the same thing as [msg](#msg) but for an existing result with `mid`.

```js
const { err, res, out } = await ao.res({ pid, mid, check, get })
```

##### ress

`ress` gets multiple results from a process. 
`next()` will be returned for pagenation if there are more messages.

```js
const { err, out: msgs, res, next } = await ao.ress({ pid, limit, asc, from })

if(next){
  const { out: msgs2 } = await next()
}
```

- `pid` : process ID
- `limit` : how many to get
- `asc` : messages are sorted descendingly by default, set `asc=true` to reverse
- `from` : cursor to get from

##### asgn

Assign an existing message to a process.

```js
const { err, mid, res, out } = await ao.asgn({ pid, mid, check, get })
```

##### load

Get a Lua source script from Arweave and eval it on a process.

```js
const { err, res, mid } = await ao.load({ src, fills, pid })
```

##### eval

Eval a Lua script on a process.

```js
const { err, res, mid } = await ao.eval({ pid, data })
```

##### spwn

Spawn a process. `module` and `scheduler` are auto-set if omitted.

```js
const { err, res, pid } = await ao.spwn({ module, scheduler, tags, data })
```

#### aoconnect Functions

The original aoconnect functions `message` | `spawn` | `result` | `assign` | `dryrun`  are also available.  
`createDataItemSigner` is available as `toSigner`.

```js
const signer = ao.toSigner(jwk)
const process = await ao.spawn({ module, scheduler, signer, tags, data })
const message = await ao.message({ process, signer, tags, data })
const result = await ao.result({ process, message })
```

#### Advanced Functions

##### postModule

`data` should be wasm binary. `overwrite` to replace the default module set to the AO instance.

```js
const { err, id: module } = await ao.postModule({ data, jwk, tags, overwrite })

```

##### postScheduler

This will post `Scheduler-Location` with the `jwk` address as the returning `scheduler`.

```js
const { err, scheduler } = await ao.postScheduler({ url, jwk, tags, overwrite })
```

##### attest

Attest Arweave transactions for WeaveDrive.

```js
const { err, res, id } = await ao.attest({ id, tags, jwk })

```

##### avail

Make Arweave transactions available for WeaveDrive.

```js
const { err, res, id } = await ao.avail({ ids, tags, jwk })

```

##### wait

`wait` until the process becomes available after `spwn`. This is mostly used internally with `deploy`.

```js
const { err } = await ao.wait({ pid })
```

##### var

`var` reads a Lua variable from the current state with `dryrun`.

```js
const { pid } = await ao.deploy({
  src_data: `Table = { String = "Hello", Array = { "str", 3, true } }`,
})

const table = await ao.var({ pid, data: "Table" })
```

It strips off pretty tags from the output and auto-converts Lua tables to JSON, you can disable it with `json` and `pretty`.

```js
const table = await ao.var({ pid, data: "Table", json: false, pretty: true })
```

---

<nav style="display:flex;justify-content:space-between;">
  <a href="./README.md">API Reference</a>
  <a href="./process.md">Process</a>
</nav>
