# Building Custom HyperBEAM Devices

Guide for building Erlang devices that plug into HyperBEAM.

## Device Protocol

Every device is an Erlang module that exports functions with arity/3:

```erlang
function(Msg1, Msg2, Opts) → {ok, Result} | {error, ErrorMap}
```

- **Msg1** — Base message (device state / process state)
- **Msg2** — Request message (user input, assignment)
- **Opts** — Options map (store, cache, wallet, etc.)

### Required Exports

| Function | Purpose |
|----------|---------|
| `info/3` | Device metadata: name, version, endpoints |
| `init/3` | Initialize device state (optional) |
| `compute/3` | Main computation handler |

### Optional Exports

| Function | Purpose |
|----------|---------|
| `snapshot/3` | Serialize state for persistence |
| `normalize/3` | Restore from snapshot |
| `terminate/3` | Cleanup on shutdown |
| `get/3` | HTTP GET access to state |

---

## Device Template

```erlang
-module(dev_mydevice).
-export([info/3, init/3, compute/3, get/3]).
-include("include/hb.hrl").

%% Device metadata
info(_M1, _M2, _Opts) ->
    {ok, #{
        <<"name">> => <<"mydevice">>,
        <<"version">> => <<"1.0">>,
        <<"endpoints">> => [<<"compute">>, <<"get">>]
    }}.

%% Initialize state (called once on process creation)
init(M1, _M2, Opts) ->
    InitialState = #{},
    M1Updated = save_state(M1, InitialState, Opts),
    {ok, M1Updated}.

%% Main computation (called for each message)
compute(M1, M2, Opts) ->
    Action = hb_maps:get(<<"action">>, M2, <<"default">>, Opts),
    case Action of
        <<"set">> -> handle_set(M1, M2, Opts);
        <<"get">> -> handle_get(M1, M2, Opts);
        _ -> {error, #{<<"status">> => 400, <<"error">> => <<"Unknown action">>}}
    end.

%% HTTP GET access (for /~mydevice@1.0/get?key=name)
get(M1, M2, Opts) ->
    handle_get(M1, M2, Opts).

%% Internal handlers
handle_set(M1, M2, Opts) ->
    Key = hb_maps:get(<<"key">>, M2, undefined, Opts),
    Value = hb_maps:get(<<"value">>, M2, undefined, Opts),
    State = load_state(M1, Opts),
    NewState = maps:put(Key, Value, State),
    M1Updated = save_state(M1, NewState, Opts),
    {ok, maps:merge(M1Updated, #{<<"result">> => <<"ok">>})}.

handle_get(M1, M2, Opts) ->
    Key = hb_maps:get(<<"key">>, M2, undefined, Opts),
    State = load_state(M1, Opts),
    Value = maps:get(Key, State, <<"not_found">>),
    {ok, maps:merge(M1, #{<<"result">> => Value})}.

%% State helpers
load_state(M1, Opts) ->
    case hb_private:get(<<"state-id">>, M1, not_found, Opts) of
        not_found -> #{};
        ID ->
            case hb_cache:read(ID, Opts) of
                {ok, State} -> hb_cache:ensure_all_loaded(State, Opts);
                not_found -> #{}
            end
    end.

save_state(M1, State, Opts) ->
    {ok, ID} = hb_cache:write(State, Opts),
    hb_private:set(M1, #{<<"state-id">> => ID}, Opts).
```

---

## Key Erlang Modules

### hb_maps — Read Request Parameters

```erlang
% Get a value from request message (M2)
Value = hb_maps:get(<<"key">>, M2, DefaultValue, Opts)

% NOTE: Use hb_maps:get NOT hb_ao:get for reading parameters
% hb_ao:get does full resolution which may be undesirable for simple reads
```

### hb_private — In-Memory Private State

```erlang
% Get private state
Val = hb_private:get(<<"key">>, M1, Default, Opts)

% Set private state (returns updated M1)
M1Updated = hb_private:set(M1, #{<<"key">> => Value}, Opts)
```

### hb_cache — Content-Addressed Storage

```erlang
% Write data (returns content-addressed ID)
{ok, ID} = hb_cache:write(Data, Opts)

% Read data by ID
{ok, Data} = hb_cache:read(ID, Opts)

% Ensure nested data is loaded
Data = hb_cache:ensure_all_loaded(Data, Opts)
```

### hb_message — Signing & Verification

```erlang
% Sign a message
{ok, Signed} = hb_message:commit(Msg, Opts)

% Verify message signatures
{ok, true} = hb_message:verify(Msg, Opts)

% Get signers of a message
Signers = hb_message:signers(M2, Opts)
% Returns list of addresses: [<<"addr1">>, <<"addr2">>]

% Get message ID
ID = hb_message:id(Msg, unsigned, Opts)
```

### ar_wallet — Crypto

```erlang
% Generate new wallet
Wallet = ar_wallet:new()

% Get address
Address = ar_wallet:to_address(Wallet)
% or from JWK:
Address = ar_wallet:to_address(JWK)
```

### hb_ao — Resolution

```erlang
% Resolve a message through a device (used in testing)
{ok, Result} = hb_ao:resolve(
    {as, dev_mydevice, #{}},        % Device + initial state
    #{<<"path">> => <<"get">>,       % Request
      <<"key">> => <<"name">>},
    #{}                              % Options
)

% Get a value (full resolution)
Value = hb_ao:get(<<"key">>, M1, Default, Opts)

% Set a value
M1Updated = hb_ao:set(<<"key">>, Value, M1, Opts)
```

---

## State Management Patterns

### Pattern 1: Private State (In-Memory)

Fast but lost on restart. Good for caches and counters.

```erlang
compute(M1, M2, Opts) ->
    Count = hb_private:get(<<"count">>, M1, 0, Opts),
    NewCount = Count + 1,
    M1Updated = hb_private:set(M1, #{<<"count">> => NewCount}, Opts),
    {ok, maps:merge(M1Updated, #{<<"count">> => NewCount})}.
```

### Pattern 2: Cache (Persistent)

Content-addressed, survives restarts. Good for important state.

```erlang
compute(M1, M2, Opts) ->
    State = load_state(M1, Opts),           % Read from cache
    NewState = process(State, M2),           % Modify
    M1Updated = save_state(M1, NewState, Opts), % Save back
    {ok, M1Updated}.
```

### Pattern 3: Direct Priv Map

Directly manipulate the private section of M1.

```erlang
compute(M1, _M2, _Opts) ->
    Priv = maps:get(<<"priv">>, M1, #{}),
    Count = maps:get(<<"count">>, Priv, 0),
    NewPriv = Priv#{<<"count">> => Count + 1},
    {ok, M1#{<<"priv">> => NewPriv}}.
```

---

## Authentication

Extract signer from message attestations:

```erlang
compute(M1, M2, Opts) ->
    case hb_message:signers(M2, Opts) of
        [] ->
            {error, #{<<"status">> => 401, <<"error">> => <<"Not signed">>}};
        [Signer | _] ->
            %% Signer is authenticated address
            handle_authenticated(M1, M2, Signer, Opts)
    end.
```

### Operator Check

```erlang
is_operator(M2, Opts) ->
    Operator = maps:get(<<"operator">>, Opts, undefined),
    Signers = hb_message:signers(M2, Opts),
    lists:member(Operator, Signers).
```

---

## Action Routing

Route computation based on Action tag:

```erlang
compute(M1, M2, Opts) ->
    Action = hb_maps:get(<<"action">>, M2, <<"default">>, Opts),
    case Action of
        <<"transfer">> -> transfer(M1, M2, Opts);
        <<"balance">> -> balance(M1, M2, Opts);
        <<"mint">> -> mint(M1, M2, Opts);
        _ -> {error, #{<<"status">> => 400, <<"error">> => <<"Unknown action">>}}
    end.
```

---

## HTTP Path Resolution

The `get/3` function handles direct HTTP GET access:

```erlang
%% GET /~mydevice@1.0/balance/addr123
get(M1, M2, Opts) ->
    Path = hb_maps:get(<<"path">>, M2, <<>>, Opts),
    case binary:split(Path, <<"/">>, [global, trim_all]) of
        [<<"balance">>, Addr] ->
            State = load_state(M1, Opts),
            Bal = maps:get(Addr, maps:get(<<"balances">>, State, #{}), 0),
            {ok, #{<<"balance">> => Bal}};
        _ ->
            {error, #{<<"status">> => 404}}
    end.
```

---

## Registration

### In sys.config (compile-time)

```erlang
{hb, [
    {preloaded_devices, [
        #{name => <<"mydevice@1.0">>, module => dev_mydevice}
    ]}
]}
```

### At Runtime

```erlang
hb:init(#{
    preloaded_devices => [
        #{name => <<"mydevice@1.0">>, module => dev_mydevice}
    ]
}).
```

### In HyperBEAM Constructor (JS)

```js
new HyperBEAM({
  devices: [
    { name: "mydevice@1.0", module: "dev_mydevice" }
  ],
})
```

---

## Compilation

```bash
cd HyperBEAM && rebar3 compile
```

Place device source in `HyperBEAM/src/dev_mydevice.erl`.

---

## Testing

### Erlang Unit Tests

```erlang
-module(dev_mydevice_tests).
-include_lib("eunit/include/eunit.hrl").

basic_test() ->
    application:ensure_all_started(hb),
    Store = hb_test_utils:test_store(hb_store_fs),
    Opts = #{store => [Store]},
    {ok, Result} = hb_ao:resolve(
        {as, dev_mydevice, #{}},
        #{<<"path">> => <<"compute">>,
          <<"action">> => <<"set">>,
          <<"key">> => <<"name">>,
          <<"value">> => <<"Alice">>},
        Opts
    ),
    ?assertEqual(<<"ok">>, maps:get(<<"result">>, Result)).
```

Run from JS:

```js
const hbeam = await new HyperBEAM({ reset: true }).ready()
await hbeam.eunit("dev_mydevice", "basic_test")
hbeam.kill()
```

### JavaScript Integration Tests

```js
import { HyperBEAM, HB, acc } from "wao/test"

describe("MyDevice", function () {
  let hbeam, hb

  before(async () => {
    hbeam = await new HyperBEAM({ reset: true }).ready()
    hb = await new HB({ url: hbeam.url }).init(acc[0].jwk)
  })

  after(async () => hbeam.kill())

  it("should set and get", async () => {
    await hb.p("/~mydevice@1.0/compute", {
      action: "set",
      key: "name",
      value: "Alice",
    })
    const result = await hb.g("/~mydevice@1.0/get", { key: "name" })
    assert.equal(result, "Alice")
  })
})
```

---

## Erlang Quick Reference

### Pattern Matching

```erlang
case hb_maps:get(<<"key">>, M2, not_found, Opts) of
    not_found -> {error, #{<<"status">> => 400}};
    Value when is_binary(Value) -> {ok, Value};
    Value -> {ok, Value}
end.
```

### Maps

```erlang
maps:get(Key, Map, Default)
maps:put(Key, Value, Map)
maps:merge(Map1, Map2)             % Map2 overrides Map1
maps:filter(fun(K, V) -> bool end, Map)
maps:keys(Map)
maps:values(Map)
maps:is_key(Key, Map)
Map#{key => value}                  % Update syntax
```

### Lists

```erlang
[H | T] = [1, 2, 3]               % H=1, T=[2,3]
[X * 2 || X <- List]              % Comprehension
lists:map(fun(X) -> X + 1 end, List)
lists:filter(fun(X) -> X > 0 end, List)
lists:foldl(fun(X, Acc) -> Acc + X end, 0, List)
lists:member(X, List)
lists:nth(N, List)                 % 1-indexed
length(List)
```

### Binaries (Strings)

```erlang
<<"hello">>                        % Binary string
<<A/binary, B/binary>>            % Concatenation
binary:split(Bin, Sep)             % Split
byte_size(Bin)                     % Length
binary_to_integer(<<"42">>)        % Parse int
integer_to_binary(42)              % Int to string
```

### Guards

```erlang
is_binary(X)    is_integer(X)    is_map(X)
is_list(X)      is_atom(X)       is_float(X)
X > 0           X =:= true       X =/= undefined
```

### Error Handling

```erlang
case dangerous_operation() of
    {ok, Result} -> {ok, Result};
    {error, Reason} -> {error, #{<<"error">> => Reason}};
    not_found -> {error, #{<<"status">> => 404}}
end.

% Try/catch (use sparingly)
try
    risky_function()
catch
    error:Reason -> {error, #{<<"error">> => Reason}};
    throw:Reason -> {error, #{<<"error">> => Reason}}
end.
```

---

## Example: ERC20 Token Device

Complete working device implementing a token.

```erlang
-module(dev_token).
-export([info/3, init/3, compute/3, get/3]).
-include("include/hb.hrl").

-define(STATE_KEY, <<"token-state">>).

info(_M1, _M2, _Opts) ->
    {ok, #{
        <<"name">> => <<"token">>,
        <<"version">> => <<"1.0">>,
        <<"endpoints">> => [<<"compute">>, <<"get">>]
    }}.

init(M1, M2, Opts) ->
    Creator = case hb_message:signers(M2, Opts) of
        [Addr | _] -> Addr;
        [] -> <<"unknown">>
    end,
    State = #{
        <<"name">> => hb_maps:get(<<"token-name">>, M2, <<"Token">>, Opts),
        <<"symbol">> => hb_maps:get(<<"token-symbol">>, M2, <<"TKN">>, Opts),
        <<"supply">> => 1000000,
        <<"balances">> => #{Creator => 1000000},
        <<"allowances">> => #{}
    },
    M1Updated = save_state(M1, State, Opts),
    {ok, M1Updated}.

compute(M1, M2, Opts) ->
    Action = hb_maps:get(<<"action">>, M2, <<"default">>, Opts),
    case Action of
        <<"transfer">> -> transfer(M1, M2, Opts);
        <<"approve">> -> approve(M1, M2, Opts);
        <<"transfer_from">> -> transfer_from(M1, M2, Opts);
        <<"balance_of">> -> balance_of(M1, M2, Opts);
        _ -> {error, #{<<"status">> => 400, <<"error">> => <<"Unknown action">>}}
    end.

transfer(M1, M2, Opts) ->
    [From | _] = hb_message:signers(M2, Opts),
    To = hb_maps:get(<<"recipient">>, M2, undefined, Opts),
    Amount = binary_to_integer(hb_maps:get(<<"amount">>, M2, <<"0">>, Opts)),
    State = load_state(M1, Opts),
    Balances = maps:get(<<"balances">>, State, #{}),
    FromBal = maps:get(From, Balances, 0),
    case FromBal >= Amount of
        false ->
            {error, #{<<"error">> => <<"Insufficient balance">>}};
        true ->
            ToBal = maps:get(To, Balances, 0),
            NewBalances = Balances#{From => FromBal - Amount, To => ToBal + Amount},
            NewState = State#{<<"balances">> => NewBalances},
            M1Updated = save_state(M1, NewState, Opts),
            {ok, maps:merge(M1Updated, #{<<"status">> => <<"ok">>})}
    end.

approve(M1, M2, Opts) ->
    [Owner | _] = hb_message:signers(M2, Opts),
    Spender = hb_maps:get(<<"spender">>, M2, undefined, Opts),
    Amount = binary_to_integer(hb_maps:get(<<"amount">>, M2, <<"0">>, Opts)),
    State = load_state(M1, Opts),
    Allowances = maps:get(<<"allowances">>, State, #{}),
    OwnerAllowances = maps:get(Owner, Allowances, #{}),
    NewAllowances = Allowances#{Owner => OwnerAllowances#{Spender => Amount}},
    NewState = State#{<<"allowances">> => NewAllowances},
    M1Updated = save_state(M1, NewState, Opts),
    {ok, maps:merge(M1Updated, #{<<"status">> => <<"ok">>})}.

transfer_from(M1, M2, Opts) ->
    [Spender | _] = hb_message:signers(M2, Opts),
    From = hb_maps:get(<<"from">>, M2, undefined, Opts),
    To = hb_maps:get(<<"recipient">>, M2, undefined, Opts),
    Amount = binary_to_integer(hb_maps:get(<<"amount">>, M2, <<"0">>, Opts)),
    State = load_state(M1, Opts),
    Allowances = maps:get(<<"allowances">>, State, #{}),
    Allowed = maps:get(Spender, maps:get(From, Allowances, #{}), 0),
    Balances = maps:get(<<"balances">>, State, #{}),
    FromBal = maps:get(From, Balances, 0),
    case Allowed >= Amount andalso FromBal >= Amount of
        false ->
            {error, #{<<"error">> => <<"Not allowed or insufficient balance">>}};
        true ->
            ToBal = maps:get(To, Balances, 0),
            NewBalances = Balances#{From => FromBal - Amount, To => ToBal + Amount},
            OwnerAllowances = maps:get(From, Allowances, #{}),
            NewOwnerAllowances = OwnerAllowances#{Spender => Allowed - Amount},
            NewAllowances = Allowances#{From => NewOwnerAllowances},
            NewState = State#{<<"balances">> => NewBalances, <<"allowances">> => NewAllowances},
            M1Updated = save_state(M1, NewState, Opts),
            {ok, maps:merge(M1Updated, #{<<"status">> => <<"ok">>})}
    end.

balance_of(M1, M2, Opts) ->
    Target = hb_maps:get(<<"target">>, M2, undefined, Opts),
    State = load_state(M1, Opts),
    Balances = maps:get(<<"balances">>, State, #{}),
    Balance = maps:get(Target, Balances, 0),
    {ok, maps:merge(M1, #{<<"balance">> => Balance})}.

get(M1, M2, Opts) ->
    Path = hb_maps:get(<<"path">>, M2, <<>>, Opts),
    State = load_state(M1, Opts),
    case binary:split(Path, <<"/">>, [global, trim_all]) of
        [<<"balance">>, Addr] ->
            Bal = maps:get(Addr, maps:get(<<"balances">>, State, #{}), 0),
            {ok, maps:merge(M1, #{<<"balance">> => Bal})};
        [<<"info">>] ->
            {ok, maps:merge(M1, #{
                <<"name">> => maps:get(<<"name">>, State),
                <<"symbol">> => maps:get(<<"symbol">>, State),
                <<"supply">> => maps:get(<<"supply">>, State)
            })};
        _ ->
            {error, #{<<"status">> => 404}}
    end.

%% State helpers
load_state(M1, Opts) ->
    case hb_private:get(?STATE_KEY, M1, not_found, Opts) of
        not_found -> #{};
        ID ->
            case hb_cache:read(ID, Opts) of
                {ok, State} -> hb_cache:ensure_all_loaded(State, Opts);
                not_found -> #{}
            end
    end.

save_state(M1, State, Opts) ->
    {ok, ID} = hb_cache:write(State, Opts),
    hb_private:set(M1, #{?STATE_KEY => ID}, Opts).
```
