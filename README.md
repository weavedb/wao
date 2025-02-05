# WAO - Wizard AO SDK & Testing

![](./assets/cover.png)

WAO SDK streamlines Arweave/AO development with elegant syntax enhancements and seamless message piping for enjoyable coding experiences. GraphQL operations are also made super easy.

Additionally, it includes a drop-in replacement for `aoconnect`, allowing the testing of lua scripts 1000x faster than the mainnet by emulating AO units in memory. It's even 100x faster than testing with [arlocal](https://github.com/textury/arlocal) and [ao-localnet](https://github.com/permaweb/ao-localnet).

- [Quick Start](#quick-start)
  - [Installation](#installation)
  - [Drop-in aoconnect Replacement for Tests](#drop-in-aoconnect-replacement-for-tests)
  - [Setting up a Project](#setting-up-a-project)
  - [Writing Tests](#writing-tests)
  - [Using WAO SDK](#using-wao-sdk)
  - [Cherry-Picking Outputs](#cherry-picking-outputs)
  - [Determining Message Success](#determining-message-success)
  - [Async Message Tracking with receive()](#async-message-tracking-with-receive)
  - [Logging](#logging)
  - [Fork Wasm Memory](#fork-wasm-memory)
  - [WeaveDrive](#weavedrive)
  - [Local Persistent Server](#local-persistent-server)
- [API Reference](#api-reference)
  - [AO](#ao)
  - [Process](#process)
  - [Function Piping](#function-piping)
  - [AR](#ar)
  - [GQL](#gql)
  - [ArMem](#armem)


## Quick Start

*WAO is still actively being developed; please use it at your discretion.*

### Installation

```bash
yarn add wao
```

### Drop-in `aoconnect` Replacement for Tests

By replacing `aoconnect` with WAO connect, everything runs in memory with zero latency and your tests are executed 1000x faster. The APIs are identical. So, there's no need to change anything else in your code.

```js
//import { spawn, message, dryrun, assign, result } from "@permaweb/aoconnect"
import { connect } from "wao/test"
const { spawn, message, dryrun, assign, result } = connect()
```

### Setting up a Project

It's super easy to set up a test AO project manually.

```bash
mkdir wao-test && cd wao-test
yarn init && yarn add wao
```

Add `test` and `test-only` commands to your `package.json`.

```json
{
  "scripts": {
    "test": "node --experimental-wasm-memory64",
    "test-only": "node --experimental-wasm-memory64 --test-only"
  }
}
```

Create `test` directory and `test.js` file.

```bash
mkdir test && touch test/test.js
```

### Writing Tests

Write a simple test in `test.js`.

```js
import assert from "assert"
import { describe, it } from "node:test"
import { connect } from "wao/test"
const { acc, spawn, message, dryrun } = connect()
// import { wait } from "wao/utils"
const signer = acc[0].signer
const src_data = `
Handlers.add("Hello", "Hello", function (msg)
  msg.reply({ Data = "Hello, World!" })
end)
`
describe("WAO", function () {
  it("should spawn a process and send messages", async () => {
    const pid = await spawn({ signer })

    // on mainnet, you need to wait here till the process becomes available.
    // WAO automatically handles it. No need with in-memory tests.
    // await wait({ pid })

    await message({
      process: pid,
      tags: [{ name: "Action", value: "Eval" }],
      data: src_data,
      signer,
    })
    const res = await dryrun({
      process: pid,
      tags: [{ name: "Action", value: "Hello" }],
      signer,
    })
    assert.equal(res.Messages[0].Data, "Hello, World!")
  })
})
```
Note that generating random Arweave wallets for every test takes time and slows down your test executions, so Wao connect provides pre-generated accounts for your tests, which saves hours if you are to run your tests thousands of times.

- `acc[0] = { jwk, addr, signer }`

Run the test.

```js
yarn test test/test.js
```

### Using WAO SDK

WAO comes with elegant syntactic sugar and makes writing AO projects an absolute joy.

The same test can be written as follows.

```js
import assert from "assert"
import { describe, it } from "node:test"
import { AO, acc } from "wao/test"

const src_data = `
Handlers.add("Hello", "Hello", function (msg)
  msg.reply({ Data = "Hello, World!" })
end)
`
describe("WAO", function () {
  it("should spawn a process and send messages", async () => {
    const ao = await new AO().init(acc[0])
    const { p } = await ao.deploy({ src_data })
    assert.equal(await p.d("Hello", "Hello, World!")
  })
})
```

The `AO` class is not only for in-memory tests, but also for production code. You just need to import from a different path.

```js
import { AR, AO, GQL } from "wao"
````

### Cherry-Picking Outputs

You often need to pick a specific piece of data from returned results with multiple spawned messages. You need to go through all the returned messages and further go through tags and data to find it. That's too much code to write. `AO` comes with `get` parameter to simplify it.

Consider the following Lua handlers.

```lua
Handlers.add("Hello", "Hello", function (msg)
  msg.reply({ Data = json.encode({ Name = "Bob" }) })
end)

Handlers.add("Hello2", "Hello2", function (msg)
  msg.reply({ Data = "Hello, World!", Name = "Bob", Age = "30" })
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
const out5 = await p.d("Hello2", { get: { name: "Name", age: "Age" } })
assert.deepEqual(out5, { name: "Bob", age: "30" })
```

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

### WeaveDrive

The [WeaveDrive](https://hackmd.io/@ao-docs/H1JK_WezR) extension is fully emulated with WAO. You can use `attest` and `avail` functions from `AO`.

```js
import { blueprint, AO, acc } from "wao/test"
const attestor = acc[0]
const handler = `
apm.install('@rakis/WeaveDrive')
Drive = require('@rakis/WeaveDrive')
Handlers.add("Get", "Get", function (msg)
  msg.reply({ Data = Drive.getData(msg.id) })
end)`

describe("WeaveDrive", () => {
  it("should load Arweave tx data", async () => {
    const ao = await new AO().init(attestor)
	
    const { p } = await ao.deploy({
      tags: { Extension: "WeaveDrive", Attestor: attestor.addr },
      loads: [ await blueprint("apm"), handler ],
    })
	
    const { id } = await ao.ar.post({ data: "Hello" })
    await ao.attest({ id })
	
    assert.equal(await p.d("Get", { id }), "Hello")
  })
})
```
### Local Persistent Server

You can run a local WAO server with persistent storage, which enables connections with outside components such as frontend apps.

```bash
npx wao-esm
```

- `port` : Arweave port, the ports of AO units are based on this port (default to `4000`)
  - AR: [localhost:4000](http://localhost:4000)
  - MU: [localhost:4002](http://localhost:4002)
  - SU: [localhost:4003](http://localhost:4003)
  - CU: [localhost:4004](http://localhost:4004)  
- `db` : a directory to store data (default to `.cache`)
- `reset` : to reset the database

```bash
npx wao-esm --port 5000 --db .custom_cache_dir --reset
```
In this case, the ports will be, AR => `5000`, MU => `5002`, SU => `5003`, CU => `5004`.

You can use WAO SDK or AOConnect to connect with the WAO units, but the following tags will be automatically set with WAO SDK.

- AOS2.0.1 Module: `Do_Uc2Sju_ffp6Ev0AnLVdPtot15rvMjP-a9VVaA5fM`
- Scheduler: `_GQ33BkPtZrqxA84vM8Zk-N2aO0toNNu_C-l-rawrBA`
- Authority: `eNaLJLsMiWCSWvQKNbk_YT-9ydeWl9lrWwXxLVp9kcg`

```js
import { describe, it } from "node:test"
import assert from "assert"
import { AO } from "wao"

const src_data = `
Handlers.add("Hello", "Hello", function (msg)
  msg.reply({ Data = "Hello, World!" })
end)`

describe("WAO Server", ()=>{
  it("should connect with WAO SDK", async ()=>{
    const ao = await new AO(4000).init(YOUR_JWK)
    const { p } = await ao.deploy({ src_data })
    assert.equal(await p.d("Hello"), "Hello, World!")
  })
})
```
With AOConnect,

```js
import { describe, it } from "node:test"
import assert from "assert"
import { connect, createDataItemSigner } from "@permaweb/aoconnect"
const { spawn, message, dryrun, assign, result } = connect({
  MU_URL: `http://localhost:4002`,
  CU_URL: `http://localhost:4004`,
  GATEWAY_URL: `http://localhost:4000`
})

const src_data = `
Handlers.add("Hello", "Hello", function (msg)
  msg.reply({ Data = "Hello, World!" })
end)`

describe("WAO Server", () => {
  it("should connect with WAO SDK", async () => {
    const pid = await spawn({
      module: "Do_Uc2Sju_ffp6Ev0AnLVdPtot15rvMjP-a9VVaA5fM",
      scheduler: "_GQ33BkPtZrqxA84vM8Zk-N2aO0toNNu_C-l-rawrBA",
      tags: [
        {
          name: "Authority",
          value: "eNaLJLsMiWCSWvQKNbk_YT-9ydeWl9lrWwXxLVp9kcg",
        },
      ],
      signer: createDataItemSigner(YOUR_JWK),
    })

    // wait till the process becomes available

    const mid = await message({
      process: pid,
      tags: [{ name: "Action", value: "Eval" }],
      data: src_data,
      signer: createDataItemSigner(acc[0].jwk),
    })

    console.log(await result({ process: pid, message: mid }))

    const res = await dryrun({
      process: pid,
      data: "",
      tags: [{ name: "Action", value: "Hello" }],
      anchor: "1234",
    })

    assert.equal(res.Messages[0].Data, "Hello, World!")
  })
})
```

Connecting with the AOS terminal,

```bash
aos \
  --gateway-url http://localhost:4000 \
  --cu-url http://localhost:4004 \
  --mu-url http://localhost:4002 \
  --tag-name Authority \
  --tag-value eNaLJLsMiWCSWvQKNbk_YT-9ydeWl9lrWwXxLVp9kcg
```

## API Reference

### AO

- [Instantiate](#instantiate-1)
- [deploy](#deploy)
- [msg](#msg)
- [dry](#dry)
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
  data, action, tags, check, get, mode, limit
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
const { err, res, out } = await ao.dry({ data, action, tags, check, get })
```

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

### Process

You can go for even more concise syntax with `Process` class.

- [Instantiate](#instantiate-2)
- [msg](#msg-1)
- [m](#m)
- [dry](#dry-1)
- [d](#d)

#### Instantiate

```js
const p = ao.p(pid)
```

or

```js
const { p, pid } = await ao.deploy({ data, tags, src, fills })
```

#### msg

The first argument is `Action`, the second argument is `Tags`, and the third argument is the rest of the options.

```js
const { mid, res, out, err } = await p.msg(
  "Action", 
  { Tag1: "value1", Tag2: "value2" }, 
  { get: true, check: { TagA: "valueA" }, jwk }
)
```
The default third argument is `{ get: true }` to return the JSON decoded `Data`.

```js
const { mid, out } = await p.msg("Action", { Tag1: "value1", Tag2: "value2" })
```

The third parameter defaults to `get` if it's not an object.

```js
const { mid, out } = await p.msg("Action", { Tag1: "value1" }, "TagA")
```

is equivalent to

```js
const { mid, out } = await p.msg("Action", { Tag1: "value1" }, "TagA") 
```

You can omit the second argument if there is no tag to pass to.

```js
const { mid, out } = await p.msg("Action", { check: "success!" }}
```

#### m

You can only get `out` with `m`. This is the most extreme form.

```js
const out = await p.m("Action", { Tag1: "value1", Tag2: "value2" })
```

This is a quite common pattern during testing. Doing the same with `aoconnect` requires an enormous amount of code, especially if it involves async/await `receive()`.

```js
const { p } = await ao.deploy({ tags, src_data, fills })
const out = await p.m("Action", { Tag1: "value1", Tag2: "value2" }) // get Data
assert.equal(out, EXPECTED_JSON)
```

#### dry

```js
const { mid, out } = await p.dry("Action", { Tag1: "value1", Tag2: "value2" })
```

#### d

```js
const out = await p.d("Action", { Tag1: "value1", Tag2: "value2" })
```

### Function Piping

- [pipe](#pipe)
- [bind](#bind)
- [then](#then)
- [err](#err)
- [cb](#cb)

#### pipe

Most functions return in the format of `{ err, res, out, pid, mid, id }`, and these function can be chained with `pipe`, which makes executing multiple messages a breeze.

For example, the following is how `deploy` uses `pipe` internally. The execution will be immediately aborted if any of the functions in `fns` produces an error.

```js
let fns = [
  {
    fn: "spwn",
    args: { module, scheduler, tags, data },
    then: { "args.pid": "pid" },
   },
   { fn: "wait", then: { "args.pid": "pid" } },
   { fn: "load", args: { src, fills }, then: { "args.pid": "pid" } }
]
const { err, res, out, pid } = await this.pipe({ jwk, fns })
```

#### bind

If the function comes from other instances rather than `AO`, use `bind`.

```js
const fns = [{ fn: "post", bind: this.ar, args: { data, tags }}]
```

#### then

You can pass values between functions with `then`. For instance, passing the result from the previous functions to the next function's arguments is a common operation.

```js
const fns = [
  { fn: "post", bind: ao.ar, args: { data, tags }, then: ({ id, args, out })=>{
    args.tags.TxId = id // adding TxId tag to `msg` args
	out.txid = id // `out` will be returned at last with `pipe`
  }},
  { fn: "msg", args: { pid, tags }},
]
const { out: { txid } } = await ao.pipe({ fns, jwk })
```

If `then` returns a value, `pipe` will immediately return with that single value. You can also use `err` to abort `pipe` with an error.

```js
const fns = [
  { fn: "msg", args: { pid, tags }, then: ({ inp })=>{
     if(inp.done) return inp.val
  }},
  { fn: "msg", args: { pid, tags }, err: ({ inp })=>{
     if(!inp.done) return "something went wrong"
  }},
]
const val = await ao.pipe({ jwk, fns })
```

`then` has many useful parameters.

- `res` : `res` from the previous result
- `args` : `args` for the next function
- `out` : the final `out` result from the `pipe` sequence
- `inp` : `out` from the previous result
- `_` : if values are assigned to the `_` fields, `pipe` returns them as top-level fields in the end
- `pid` : `pid` will be passed if any previous functions return `pid` ( e.g. `deploy` )
- `mid` : `mid` will be passed if any previous functions return `mid` ( e.g. `msg` )
- `id` : `id` will be passed if any previous functions return `id` ( e.g. `post` )

`then` can be a simplified hashmap object.

```js
let fns = [
  {
    fn: "msg",
    args: { tags },
    then: { "args.mid": "mid", "out.key": "inp.a", "_.val": "inp.b" },
   }, 
   { fn: "some_func", args: {} } // args.mid will be set from the previous `then`
]
const { out: { key }, val } = await ao.pipe({ jwk, fns })
```

#### err

`err` has the same signature as `then`. If `err` returns a value, `pipe` will throw an `Error` with that value.

```js
const fns = [
  { fn: "msg", args: { pid, tags }, err: ({ inp })=>{
     if(!inp.done) return "something went wrong!"
  }}
]
const val = await ao.pipe({ jwk, fns })

```

#### cb

`cb` can report the current progress of `pipe` after every function execution.

```js
await ao.pipe({ jwk, fns, cb: ({ i, fns, inp })=>{
  console.log(`${i} / ${fns.length} functions executed`)
}})
```

### AR

`AR` handles operations on the base Arweave Storage layer as well as wallet connections.

- [Instantiate](#instantiate)
- [Set or Generate Wallet](#set-or-generate-wallet)
- [toAddr](#toaddr)
- [mine](#mine)
- [balance | toAR | toWinston](#balance--toar--towinston)
- [transfer](#transfer)
- [checkWallet](#checkwallet)
- [post](#post)
- [tx](#tx)
- [data](#data)
- [bundle](#bundle)

#### Instantiate

```js
import { AR } from "wao"
const ar = new AR()
```
`host`, `port`, and `protocol` can be set to access a specific gateway rather than `https://arweave.net`.

```js
const ar = new AR({ host: "localhost", port: 4000, protocol: "http" })
```

In the case of local gateways, you can only set `port` and the rest will be automatically figured out.
```js
const ar = new AR({ port: 4000 })
```

`AO` class auto-instantiates `AR` internally.

```js
import { AO } from "wao"
const ao = new AO()
const ar = ao.ar
```

#### Set or Generate Wallet

You can initialize AR with a wallet JWK or ArConnect.

```js
const ar = await new AR().init(jwk || arweaveWallet)
```

Or you can generate a new wallet. In case of ArLocal, you can mint AR at the same time.

```js
const { jwk, addr, pub, balance } = await ar.gen("100") // mint 100 AR
```

Once a wallet is set in one of these 3 ways, you cannot use the instance with another wallet unless you re-initialize it with another wallet. This is to prevent executing transactions with the wrong wallet when the browser connected active address has been changed unknowingly.

You can go on without calling `init` or `gen`, in this case, AR generates a random wallet when needed, and also using different wallets will be allowed. This is useful, if you are only calling `dryrun` with AO, since AO requires a signature for `dryrun` too, but you don't want to bother the user by triggering the browser extension wallet for read only calls.

Once a wallet is set, `ar.jwk` and `ar.addr` will be available.

#### Token Related Methods

##### toAddr

Convert a jwk to the corresponding address.

```js
const addr = await ar.toAddr(jwk)
```

##### mine

Mine pending blocks (only for arlocal).

```js
await ar.mine()
```

##### balance | toAR | toWinston

Get the current balance of the specified address in AR. `addr` will be `ar.addr` if omitted.

```js
const balance_AR = await ar.balance() // get own balance
const balance_Winston = ar.toWinston(balance_AR)
const balance_AR2 = ar.toAR(balance_Winston)
const balance_AR3 = await ar.balance(addr) // specify wallet address
```

##### transfer

Transfer AR token. `amount` is in AR, not in winston for simplicity.

```js
const { id } = await ar.transfer(amount, to)
```

You can set a jwk to the 3rd parameter as a sender. Otherwise, the sender is `ar.jwk`.

```js
const { id } = await ar.transfer(amount, to, jwk)
```

For most write functions, `jwk` can be specified as the last parameter or a field like `{ data, tags, jwk }`.


##### checkWallet

`checkWallet` is mostly used internally, but it returns `this.jwk` if a wallet has been assigned with `init`, or else it generates a random wallet to use. The following pattern is used in many places. With this pattern, if a wallet is set with `init` and the `jwk` the user is passing is different, `checkWallet` produces an error to prevent the wrong wallet. If no wallet has been set with `init` or `gen` and the `jwk` is not passed, it generates and returns a random wallet.

```js
some_class_method({ jwk }){
  let err = null
  ;({ err, jwk } = await ar.checkWallet({ jwk }))
  if(!err){
    // do something with the jwk
  }
}
```

#### Storage Related Methods

##### post 

Post a data to Arweave.

```js
const { err, id } = await ar.post({ data, tags })
```

`tags` are not an Array but a hash map Object for brevity.

```js
const tags = { "Content-Type": "text/markdown", Type: "blog-post" }
```

If you must use the same name for multiple tags, the value can be an Array.

```js
const tags = { Name: [ "name-tag-1", "name-tag-2" ] }
```


##### tx

Get a transaction.

```js
const tx = await ar.tx(txid)
```

##### data

Get a data.

```js
const data = await ar.data(txid, true) // true if string
```

##### bundle

Bundle ANS-104 dataitems.

```js
const { err, id } = await ar.bundle(dataitems)
```
`dataitems` are `[ [ data, tags ], [ data, tags ], [ data, tags ] ]`.
```js
const { err, id } = await ar.bundle([
  [ "this is text", { "Content-Type": "text/plain" }],
  [ "# this is markdown", { "Content-Type": "text/markdown" }],
  [ png_image, { "Content-Type": "image/png" }]
])
```

### GQL

`GQL` simplifies [the Arwave GraphQL](https://gql-guide.vercel.app/) operations to query blocks and transactions.

- [Instantiate](#instantiate-3)
- [Txs](#txs)
- [Blocks](#blocks)

#### Instantiate

You can instantiate the GQL class with an endpoint `url`.

```js
import { GQL } from "wao"
const gql = new GQL({ url: "https://arweave.net/graphql" }) // the default url
```

`AR` class auto-instantiates `GQL` internally.

```js
import { AO } from "wao"
const ao = new AO()
const gql = ao.ar.gql
```
```js
import { AR } from "wao"
const ar = new AR()
const gql = ar.gql
```

#### Txs

Get latest transactions.

```js
const txs = await gql.txs()
```

##### asc

Get transactions in ascending order.

```js
const txs = await gql.txs({ asc: true })
```

##### first

Get the firxt X transactions.

```js
const txs = await gql.txs({ first: 3 })
```

##### after

Get transactions after a specific one to paginate. Pass a `cursor`.

```js
const txs = await gql.txs({ first: 3 })
const txs2 = await gql.txs({ first: 3, after: txs[2].cursor })
```

##### next

Easier pagination with `next`.

```js
const { next, data: txs0_2 } = await gql.txs({ first: 3, next: true })
const { next: next2, data: txs3_5 } = await next()
const { next: next3, data: txs6_8 } = await next2()
```

`res.next` will be `null` if there's no more transactions to paginate.

##### block

Get transactions within a block height range.

```js
const txs = await gql.txs({ block: { min: 0, max: 10 } })
```

or


```js
const txs = await gql.txs({ block: [0, 10] })
```

You can also specify only `min` or `max`.

##### by Transaction IDs

Get transactions by transaction ids.

```js
const txs = await gql.txs({ id: TXID })
```
or

```js
const txs = await gql.txs({ ids: [ TXID1, TXID2, TXID3 ] })
```

##### by Recipients

Get transactions by recipients.

```js
const txs = await gql.txs({ recipient: ADDR })
```
or

```js
const txs = await gql.txs({ recipients: [ ADDR1, ADDR2, ADDR3 ] })
```

##### by Owners

Get transactions by owners.

```js
const txs = await gql.txs({ owner: ADDR })
```
or

```js
const txs = await gql.txs({ owners: [ ADDR1, ADDR2, ADDR3 ] })
```

##### by Tags

Get transactions that match tags.

```js
const txs = await gql.txs({ tags: { Name: "Bob", Age: "30" } })
```

##### fields

Choose fields to be returned.

```js
const txs = await gql.txs({ fields: ["id", "recipient"] })
```

For nested objects,

```js
const txs = await gql.txs({ fields: ["id", { owner: ["address", "key"] }] })
```

You can use a hashmap to specify fields too.

```js
const txs = await gql.txs({ 
  fields: { id: true, { owner: { address: true, key: true } } } 
})
```

If you assign `false`, the other fields will be returned.

```js
const txs = await gql.txs({ 
  fields: { id: true, { block: { previous: false } } } 
})
```

For example, the above will exclude `previous` from `block` and return `id`, `timestamp` and `height`.

The entire available fields for transactions as in a graphql query are as follows.

```js
const tx_fields = `{
  id 
  anchor 
  signature 
  recipient 
  owner { address key } 
  fee { winston ar } 
  quantity { winston ar } 
  data { size type } 
  tags { name value } 
  block { id timestamp height previous } 
  parent { id }
  bundledIn { id }
}`
```

#### Blocks

Get latest blocks.

```js
const blocks = await gql.blocks()
```

##### asc

Get blocks in ascending order.

```js
const blocks = await gql.blocks({ asc: true })
```

##### first

Get the firxt X blocks.

```js
const blocks = await gql.blocks({ first: 3 })
```

##### after

Get blocks after a specific one to paginate. Pass a `cursor`.

```js
const blocks = await gql.blocks({ first: 3 })
const blocks2 = await gql.blocks({ first: 3, after: blocks[2].cursor })
```

##### by Block IDs

Get blocks by block ids.

```js
const blocks = await gql.blocks({ id: BLCID })
```
or

```js
const blocks = await gql.blocks({ ids: [ BLKID1, BLKID2, BLKID3 ] })
```

##### height

Get blocks within a block height range.

```js
const blocks = await gql.blocks({ height: { min: 0, max: 10 } })
```

or


```js
const blocks = await gql.blocks({ height: [0, 10] })
```

##### next

Easier pagination with `next`.

```js
const { next, data: blocks0_2 } = await gql.blocks({ first: 3, next: true })
const { next: next2, data: blocks3_5 } = await next() 
const { next: next3, data: blocks6_8 } = await next2()
```

`res.next` will be `null` if there's no more blocks to paginate.

##### fields

```js
const blocks = await gql.blocks({ 
  fields: ["id", "timestamp", "height", "previous"]
})
```

The entire available fields for blocks as in a graphql query are as follows.

```js
const block_fields = `{ id timestamp height previous }`
```

### ArMem

`ArMem` stands for Arweave in memory and is a class to emulate an Arweave node and AO units in memory, which is internally used in the WAO testing framework. You can instantiate `ArMem` and control multiple emulators by passing it between other classes.

- [Instantiate](#instantiate-3)

#### Instantiate

When you instantiate WAO `connect` or `AO` from `wao/test`, it automatically and internally instantiates `ArMem`.

```js
import { connect } from "wao/test"
const { spawn, message, dryrun, assign, result, mem } = connect() // aoconnect APIs
```


```js
import { AO } from "wao/test"
const ao = new AO() // ao.mem
```

You can instantiate `ArMem` and pass it to other classes.

```js
import { ArMem, AO, AR, connect } = "wao/test"
const mem = new ArMem()
const { spawn, message, dryrun, assign, result } = connect(mem)
const ao = new AO({ mem })
const ar = new AR({ mem })
```

If you don't pass the same `ArMem` instance, the two AO instances will have different environments.

```js
import { AO } = "wao/test"
const ao = new AO() // ao.mem
const ao2 = new AO() // ao2.mem
```

`ao.mem` and `ao2.mem` are not connected. They are on different networks.

```js
import { AO } = "wao/test"
const ao = new AO()
const ao2 = new AO({ mem: ao.mem })
```

This will connect the two.


