---
name: create-device
description: "Scaffold a new HyperBEAM Erlang device with its test file. Use when user says 'create a device', 'new Erlang device', 'scaffold a device', or 'add a device'. Not for building full devices with iteration — use /build-device."
argument-hint: "<device-name>"
disable-model-invocation: true
---

Scaffold a new HyperBEAM Erlang device with its test file.

The argument `$ARGUMENTS` is the device name (required). It should be lowercase and will be used as the module name.

## Steps

1. Read `docs/hyperbeam-dev.md` for the device template and protocol.

2. Create the Erlang device at `HyperBEAM/src/dev_{name}.erl`:

```erlang
-module(dev_{name}).
-export([info/3, init/3, compute/3]).
-include("include/hb.hrl").

-define(STATE_KEY, <<"{name}-state">>).

info(_M1, _M2, _Opts) ->
    {ok, #{
        <<"name">> => <<"{name}">>,
        <<"version">> => <<"1.0">>,
        <<"endpoints">> => [<<"compute">>]
    }}.

init(M1, _M2, Opts) ->
    State = #{},
    {ok, ID} = hb_cache:write(State, Opts),
    M1Updated = hb_private:set(M1, #{?STATE_KEY => ID}, Opts),
    {ok, M1Updated}.

compute(M1, M2, Opts) ->
    Action = hb_maps:get(<<"action">>, M2, <<"default">>, Opts),
    case Action of
        <<"get">> -> handle_get(M1, M2, Opts);
        _ -> {error, #{<<"status">> => 400, <<"error">> => <<"Unknown action">>}}
    end.

handle_get(M1, _M2, _Opts) ->
    {ok, maps:merge(M1, #{<<"result">> => <<"ok">>})}.
```

Where `{name}` is the argument (e.g., `counter`).

3. Create the test at `test/{name}.device.test.js`:

```js
import assert from "assert"
import { describe, it, before, after } from "node:test"
import { HyperBEAM, HB, acc } from "wao/test"

describe("{Name} Device", function () {
  let hbeam, hb

  before(async () => {
    hbeam = await new HyperBEAM({ reset: true }).ready()
    hb = await new HB({ url: hbeam.url }).init(acc[0].jwk)
  })

  after(async () => hbeam.kill())

  it("should respond to get", async () => {
    const result = await hb.g("/~{name}@1.0/compute", { action: "get" })
    assert.ok(result)
  })
})
```

4. Compile HyperBEAM:

```bash
cd HyperBEAM && rebar3 compile
```

5. Run the test:

```bash
yarn test test/{name}.device.test.js
```

6. Report whether compilation and test pass. If either fails, fix and re-run.
