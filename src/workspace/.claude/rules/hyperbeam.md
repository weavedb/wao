---
paths:
  - "HyperBEAM/**/*.erl"
---

# HyperBEAM Erlang Device Rules

For full reference, read `docs/hyperbeam-dev.md`.

## Device Protocol

Every device exports functions with arity/3:

```erlang
function(Msg1, Msg2, Opts) → {ok, Result} | {error, ErrorMap}
```

- **Msg1** — Base message / device state
- **Msg2** — Request message / user input
- **Opts** — Options map (store, cache, wallet)

### Required Exports

- `info/3` — Device metadata (name, version, endpoints)
- `compute/3` — Main computation handler

### Optional Exports

- `init/3` — Initialize state (called once on process creation)
- `snapshot/3` — Serialize state for persistence
- `normalize/3` — Restore from snapshot
- `get/3` — HTTP GET access to state

## State Management

```erlang
% Private state (in-memory, fast)
Val = hb_private:get(<<"key">>, M1, Default, Opts),
M1Updated = hb_private:set(M1, #{<<"key">> => Val}, Opts).

% Cache (persistent, content-addressed)
{ok, ID} = hb_cache:write(Data, Opts),
{ok, Data} = hb_cache:read(ID, Opts).

% Read request params (from M2)
Value = hb_maps:get(<<"param">>, M2, Default, Opts).
```

## Authentication

```erlang
case hb_message:signers(M2, Opts) of
    [] -> {error, #{<<"status">> => 401}};
    [Signer | _] -> handle_authenticated(M1, M2, Signer, Opts)
end.
```

## Action Routing

```erlang
compute(M1, M2, Opts) ->
    Action = hb_maps:get(<<"action">>, M2, <<"default">>, Opts),
    case Action of
        <<"transfer">> -> transfer(M1, M2, Opts);
        <<"balance">> -> balance(M1, M2, Opts);
        _ -> {error, #{<<"status">> => 400}}
    end.
```

## Compilation

```bash
cd HyperBEAM && rebar3 compile
```

## Testing

### Erlang eunit

```js
await hbeam.eunit("dev_mydevice", "basic_test")
```

### JS integration

```js
const hbeam = await new HyperBEAM({ reset: true }).ready()
const hb = await new HB({ url: hbeam.url }).init(acc[0].jwk)
await hb.p("/~mydevice@1.0/compute", { action: "set", key: "k", value: "v" })
const result = await hb.g("/~mydevice@1.0/get", { key: "k" })
hbeam.kill()
```

## Registration

Add to `preloaded_devices` in config or runtime:

```erlang
hb:init(#{
    preloaded_devices => [
        #{name => <<"mydevice@1.0">>, module => dev_mydevice}
    ]
}).
```
