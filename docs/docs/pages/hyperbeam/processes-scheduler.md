# Processes and Scheduler

Most things we've learned so far to do manually, such as device method composition, message caching, and commitments for message verification, are handled automatically by `message@1.0`, `process@1.0`, and `scheduler@1.0`.

You can `spawn` a process, `schedule` messages to process slots, and `compute` the process state going through the allocated messages using multiple execution devices with `stack@1.0`.

Let's create a new custom device `dev_inc.erl`. The minimum viable device to be compatible with `process@1.0` requires 4 methods:

`init`, `normalize`, `compute`, and `snapshot`.

```erlang [/HyperBEAM/src/dev_inc.erl]
-module(dev_inc).
-export([ compute/3, init/3, snapshot/3, normalize/3 ]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").
 
compute(Msg1, Msg2, Opts) ->
  Num = maps:get(<<"num">>, Msg1),
  {ok, hb_ao:set( Msg1, #{ <<"num">> => Num + 1 }, Opts )}.

init(Msg, Msg2, Opts) -> 
  {ok, hb_ao:set(Msg, #{ <<"num">> => 0 }, Opts)}.
 
snapshot(Msg, _Msg2, _Opts) -> {ok, Msg}.
 
normalize(Msg, _Msg2, _Opts) -> {ok, Msg}.
```

Don't forget to add it to `preloaded_devices` in `hb_opts.erl`.

```erlang [/HyperBEAM/src/hb_opts.erl]
preloaded_devices => [
  ...
  #{ <<"name">> => <<"inc@1.0">>, <<"module">> => dev_inc }
],
```

Add `message@1.0`, `process@1.0`, `scheduler@1.0`, and `inc@1.0` to our `HyperBEAM` node in the test file.

You'll also need a random `seed` generation function, since message IDs are deterministic content hashes and it generates the same message ID if you send the same initialization message.

To avoid this, you need to include a randomized value in the message content to spawn a new process.

Also you need to nest the message in `body` without specifying `path` in the inner message.

You can use the following paths to `spawn`, `schedule`, and `compute`:

- `/~process@1.0/schedule` : to spawn a process, requires
  - `scheduler=[operator_wallet_address]`
  - `body`
    - `device="process@1.0"`
    - `scheduler=[operator_wallet_address]`
    - `type="Process"`
    - `execution-device`
- `/[pid]/schedule` : to schedule a message to the `pid`
  - `body`
    - `type="Message"`
- `/[pid]/compute?slot=[slot]` : to compute the `pid` state up to the `slot`

`pid` is the process message ID returned by the `spawn` message.

At this point, you could simply remove the `devices` parameter and preload all existing devices in our test file.

```js [/test/processes-scheduler.test.js]
import assert from "assert"
import { describe, it, before, after, beforeEach } from "node:test"
import { HyperBEAM } from "wao/test"

const cwd = "../HyperBEAM"

const seed = num => {
  const array = new Array(num)
  for (let i = 0; i < num; i++) array[i] = Math.floor(Math.random() * 256)
  return Buffer.from(array).toString("base64")
}

describe("Processes and Scheduler", function () {
  let hbeam, hb
  before(async () => {
    hbeam = await new HyperBEAM({ cwd, reset: true }).ready()
  })
  beforeEach(async () => (hb = hbeam.hb))
  after(async () => hbeam.kill())

  it("should spawn a process", async () => {
    const { process: pid } = await hb.p("/~process@1.0/schedule", {
      scheduler: hb.addr,
      body: {
        device: "process@1.0",
        type: "Process",
        scheduler: hb.addr,
        "random-seed": seed(16),
        "execution-device": "inc@1.0",
      },
    })
    console.log(`Process ID: ${pid}`)
    const { slot } = await hb.p(`/${pid}/schedule`, {
      body: { type: "Message" },
    })
    console.log(`Allocated Slot: ${slot}`)

    const out = await hb.g(`/${pid}/compute`, { slot })
    assert.equal(out.num, 2)

    const { slot: slot2 } = await hb.p(`/${pid}/schedule`, {
      body: { type: "Message" },
    })
    console.log(`Allocated Slot: ${slot2}`)

    const out2 = await hb.g(`/${pid}/compute`, { slot: slot2 })
    assert.equal(out2.num, 3)
  })
})
```

## now

`/[pid]/now` gives you the latest process state.

```js [/test/processes-scheduler.test.js]
const { slot: slot3 } = await hb.p(`/${pid}/schedule`, {
  body: { type: "Message" },
})
console.log(`Allocated Slot: ${slot3}`)

const out3 = await hb.g(`/${pid}/now`)
assert.equal(out3.num, 4)
```

## WAO SDK

WAO has convenient APIs for process management.


```js [/test/processes-scheduler.test.js]
const { pid } = await hb.spawn({ "execution-device": "inc@1.0" })
const { slot } = await hb.schedule({ pid })
const { num } = await hb.compute({ pid, slot })
assert.equal(num, 2)

const {
  res: { num: num2 },
} = await hb.message({ pid }) // schedule + compute
assert.equal(num2, 3)

const { num: num3 } = await hb.now({ pid })
assert.equal(num3, 3)
```

## stack@1.0

Just like the previous chapter, you can stack multiple devices and let the state transition go through each `compute` method.

Let's create `double@1.0` and `square@1.0` devices.

```erlang [/HyperBEAM/src/dev_double.erl]
-module(dev_double).
-export([ compute/3, init/3, snapshot/3, normalize/3 ]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").
 
compute(Msg1, Msg2, Opts) ->
  Num = maps:get(<<"num">>, Msg1),
  {ok, hb_ao:set( Msg1, #{ <<"num">> => Num * 2 }, Opts )}.

init(Msg, Msg2, Opts) -> 
  {ok, hb_ao:set(Msg, #{ <<"num">> => 0 }, Opts)}.
 
snapshot(Msg, _Msg2, _Opts) -> {ok, Msg}.
 
normalize(Msg, _Msg2, _Opts) -> {ok, Msg}.
```

```erlang [/HyperBEAM/src/dev_square.erl]
-module(dev_square).
-export([ compute/3, init/3, snapshot/3, normalize/3 ]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").
 
compute(Msg1, Msg2, Opts) ->
  Num = maps:get(<<"num">>, Msg1),
  {ok, hb_ao:set( Msg1, #{ <<"num">> => Num * Num }, Opts )}.

init(Msg, Msg2, Opts) -> 
  {ok, hb_ao:set(Msg, #{ <<"num">> => 0 }, Opts)}.
 
snapshot(Msg, _Msg2, _Opts) -> {ok, Msg}.
 
normalize(Msg, _Msg2, _Opts) -> {ok, Msg}.
```

Don't forget to add `double@1.0` and `square@1.0` to `hb_opts.erl`.

Then test the `stack@1.0` process with multiple devices.

```js [/test/processes-scheduler.test.js]
const { pid } = await hb.spawn({
  "execution-device": "stack@1.0",
  "device-stack": ["inc@1.0", "double@1.0", "square@1.0"],
})

const { num } = await hb.now({ pid })
assert.equal(num, 4) // ((0 + 1) * 2) * ((0 + 1) * 2)

const { res: { num: num2 } } = await hb.message({ pid })
assert.equal(num2, 100) // ((4 + 1) * 2) * ((4 + 1) * 2)
```

## patch@1.0

`patch@1.0` allows you to cache any pieces of data to arbitrary URLs. You can pass `patch-from` to specify where the data to patch comes from in the resulting messages, and `patch-to` to specify a URL endpoint to expand the cache to. So let's set `patch-from="/results"` and `patch-to="/cache"`.

First, let's create a modified version of the inc device and call it `inc2@1.0`, which increments `num` and caches `double` and `square` values of `num`. We return these caches under `results:1`.

```erlang [/HyperBEAM/src/dev_inc2.erl]
-module(dev_inc2).
-export([ compute/3, init/3, snapshot/3, normalize/3 ]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").
 
compute(Msg1, Msg2, Opts) ->
  Num = maps:get(<<"num">>, Msg1) + 1,
  {ok, hb_ao:set( 
    Msg1,
    #{ 
      <<"num">> => Num,
      <<"results">> => #{ 
        <<"1">> => #{ 
          <<"method">> => <<"PATCH">>, 
          <<"double">> => Num * 2,
          <<"square">> => Num * Num 
        }
      } 
    }, 
    Opts
  )}.

init(Msg, Msg2, Opts) -> 
  {ok, hb_ao:set(Msg, #{ <<"num">> => 0 }, Opts)}.
 
snapshot(Msg, _Msg2, _Opts) -> {ok, Msg}.
 
normalize(Msg, _Msg2, _Opts) -> {ok, Msg}.
```

Now `/now/cache/double` and `/now/cache/square` will be accessible with the cached latest values.

```js [/test/processes-scheduler.test.js]
const { pid } = await hb.spawn({
  "execution-device": "stack@1.0",
  "device-stack": ["inc2@1.0", "patch@1.0"],
  "patch-from": "/results",
  "patch-to": "/cache",
})
await hb.schedule({ pid })
await hb.schedule({ pid })
const square = (await hb.now({ pid, path: "/cache/square" })).body
const double = (await hb.now({ pid, path: "/cache/double" })).body
assert.equal(square, 9)
assert.equal(double, 6)
```

## Running Tests

You can find the working test file for this chapter here:

- [processes-scheduler.test.js](https://github.com/weavedb/wao/blob/master/dhfs-tutorial-app/test/processes-scheduler.test.js)

Run tests:

```bash [Terminal]
yarn test test/processes-scheduler.test.js
```

## References

##### Device Docs

- [Device: ~message@1.0](https://hyperbeam.ar.io/build/devices/message-at-1-0.html)
- [Device: ~process@1.0](https://hyperbeam.ar.io/build/devices/process-at-1-0.html)
- [Device: ~scheduler@1.0](https://hyperbeam.ar.io/build/devices/scheduler-at-1-0.html)

##### Device API

- [dev_stack.erl](https://hyperbeam.ar.io/build/devices/source-code/dev_stack.html)
- [dev_message.erl](https://hyperbeam.ar.io/build/devices/source-code/dev_message.html)
- [dev_process.erl](https://hyperbeam.ar.io/build/devices/source-code/dev_process.html)
- [dev_scheduler.erl](https://hyperbeam.ar.io/build/devices/source-code/dev_scheduler.html)
- [dev_patch.erl](https://hyperbeam.ar.io/build/devices/source-code/dev_patch.html)

##### WAO API

- [HyperBEAM Class API](/api/hyperbeam)
- [HB Class API](/api/hb)
