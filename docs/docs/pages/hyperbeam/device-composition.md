# Device Composition

So far, we've learned about HyperBEAM devices and URL pathing, the core codecs, HTTP message signatures, and hashpaths. You already know the fundamentals of how HyperBEAM works.

## Chaining Device Methods with URL Path

Let's play around with device composition to build something powerful. We can access any cached messages with an ID or a hashpath at `/[id | hashpath]`. And we can also chain device methods like `/~meta@1.0/info/~json@1.0/serialize`.

Could we chain our own device methods like the following?

- `/[hashpath]/~wao@1.0/inc/~wao@1.0/double/~wao@1.0/square`

Let's find out!

Our goal is to pass an existing message with `num`, and compute `num` through the device method chaining. So if the initial message with a hashpath has `num=6`,

- => `/~wao@1.0/inc` => 6 + 1 => `num=7`
- => `/~wao@1.0/double` => 7 * 2 => `num=14`
- => `/~wao@1.0/square` => 14 * 14 => `num=196`

is what we need to end up with.

```erlang
inc(Msg1, Msg2, Opts)->
  Num = maps:get(<<"num">>, Msg1),
  {ok, #{ <<"num">> => Num + 1 }}.

double(Msg1, Msg2, Opts)->
  Num = maps:get(<<"num">>, Msg1),
  {ok, #{ <<"num">> => Num * 2 }}.

square(Msg1, Msg2, Opts)->
  Num = maps:get(<<"num">>, Msg1),
  {ok, #{ <<"num">> => Num * Num }}.
```

We can use the `resolve` method from the previous chapter to create the base `num` with the `hashpath` cached. `/~wao@1.0/resolve` returns `num=6` with `out.hashpath_7`.

```js
const { out, hashpath } = await hb.post({ path: "/~wao@1.0/resolve" })
const { out: msg8 } = await hb.get({
  path: `/${out.hashpath_7}/~wao@1.0/inc/~wao@1.0/double/~wao@1.0/square`,
})
assert.equal(msg8.num, 196)
```
Voila! It works! But there are 3 caveats to this.

First of all, during this pipeline, the `Msg2` passed to each device method of `inc/3`, `double/3`, and `square/3` stays the same and is the original committed `Msg2` to the first method in the chain, which in this case is deviceless since we start the pipeline with `/${out.hashpath_7}`.

If we were to start the chain with `/~wao@1.0/inc/~wao@1.0/double/~wao@1.0/square`, the `Msg2` would always be the same as what is passed to `/~wao@1.0/inc`. So to evolve the state, you need to use the values from `Msg1`.

Secondly, as we learned in an earlier chapter, `Msg1` contains inter-decoded values, and not the final decoded values, which means even if you pass `integer`, `Msg1` will have stringified `num`. You need to take the initial values from `Msg2`.

Lastly, during the pipeline, you cannot overwrite the fields initially passed to `Msg2`. So you cannot pass `num` and update `num` during the pipeline. The initial `Msg2` always overwrites the updated `num` and it ends up unchanged. So you need to pass something other than `num`, then update `num` during the pipeline. The case with `/[hashpath]/~wao@1.0/inc/~wao@1.0/double/~wao@1.0/square` works since we're not passing `num` to the initial `/[hashpath]` execution.

One way to solve this is to create an entry method like `calc` to take a different field such as `init_num` from `Msg2`, then pass it down to the pipeline as `num`.

```erlang
calc(Msg1, Msg2, Opts)->
  Num = maps:get(<<"init_num">>, Msg2),
  {ok, #{ <<"num">> => Num}}.
```

Now we can do `/~wao@1.0/calc/~wao@1.0/inc/~wao@1.0/double/~wao@1.0/square`, and get the correct output.

```js
const { out: { num } } = await hb.get({
  path: "/~wao@1.0/calc/~wao@1.0/inc/~wao@1.0/double/~wao@1.0/square",
  init_num: 1
})
assert.equal(num, 16)
```

## Stacking Devices

There is a built-in device called `stack@1.0` to make device composition easy. It's supposed to be used with `process@1.0`, so it's limited in a certain way, but we can still use it without processes.

Let's modify our methods to make them compatible with `stack@1.0`. We just need to forward `device-stack` from `Msg1`.

```erlang
inc(Msg1, Msg2, Opts)->
  io:format("Inc: ~p~n", [Msg1]),
  Num = maps:get(<<"num">>, Msg1),
  {ok, #{ 
    <<"num">> => Num + 1, 
    <<"device-stack">> => maps:get(<<"device-stack">>, Msg1)
  }}.

double(Msg1, Msg2, Opts)->
  Num = maps:get(<<"num">>, Msg1),
  {ok, #{ 
    <<"num">> => Num * 2,
    <<"device-stack">> => maps:get(<<"device-stack">>, Msg1)
  }}.

square(Msg1, Msg2, Opts)->
  Num = maps:get(<<"num">>, Msg1),
  {ok, #{ 
    <<"num">> => Num * Num,
    <<"device-stack">> => maps:get(<<"device-stack">>, Msg1)
   }}.
```

Add the `stack@1.0` device to the `HyperBEAM` class in our test file.

```js
import assert from "assert"
import { describe, it, before, after, beforeEach } from "node:test"
import { HyperBEAM, toAddr } from "wao/test"
import { HB } from "wao"
import { resolve } from "path"
import { readFileSync } from "fs"
const cwd = "../HyperBEAM"
const wallet = resolve(process.cwd(), cwd, ".wallet.json")
const jwk = JSON.parse(readFileSync(wallet, "utf8"))
const addr = toAddr(jwk.n)
const devices_base = [ "json", "structured", "httpsig", "flat", "meta" ]

describe("HyperBEAM", function () {
  let hbeam, hb
  before(async () => {
    hbeam = await new HyperBEAM({
      cwd,
      devices: [ ...devices_base, "wao", "stack" ],
    }).ready()
  })

  beforeEach(async () => {
    hb = await new HB({}).init(jwk)
  })

  after(async () => hbeam.kill())
  
  it("should test resolve", async () => {
    const msg_base = {
      device: "stack@1.0",
      "device-stack": { 1: "wao@1.0", 2: "wao@1.0", 3: "wao@1.0" },
      mode: "Fold",
      num: 3,
    }
    const { out } = await hb.post({ ...msg_base, path: "inc" })
	assert.equal(out.num, 6) // 3 + 1 + 1 + 1
	
    const { out: out2 } = await hb.post({ ...msg_base, path: "double" })
	assert.equal(out2.num, 24) // 3 * 2 * 2 * 2
	
    const { out: out3 } = await hb.post({ ...msg_base, path: "square" })
	assert.equal(out3.num, 6561) // 3 * 3 * 9 * 81
  })
})
```

You can stack multiple devices in `device-stack`, but the limitation is it executes the same method on each device specified in `path`. With `process@1.0`, it executes the `compute` method, which we'll talk about in the next chapter.
