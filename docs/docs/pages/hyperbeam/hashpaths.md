# Hashpaths

Hashpath is a mechanism to make compute steps verifiable with chained hashes. 

## Message ID

Each message has an ID.

For signed messages, the ID is the sha256 hash of all commitment IDs except for `hmac` joined with `, `.

For unsigned messages, the ID is the hmac-sha256 hash of the message content with `ao` as the key.

You can use `hb_message:id` on HyperBEAM.

```erlang
ID = hb_message:id(Msg)
```

Or you can use `id` from `wao/utils`.

```js
import { id } from "wao/utils"
const msg_id = id(msg)
```

## Message Resolving

As we've learned so far, URLs like `http://localhost:10001/~mydev@1.0/forward` are automatically resolved to the `forward` method of the `mydev@1.0` device with 3 arguments `(Msg1, Msg2, Opts)`.

We can internally do the same with `hb_ao:resolve(Msg1, Msg2, Opts)` to result in a new message `Msg3`.

But recall from the previous chapter, `Msg1` needs to contain `device`, and `Msg2` has to contain `path` to be resolved.

Let's create an `add/3` method, which takes a message with `num` and `plus`, then executes `num = num + plus`. It returns `device` in addition to the new `num` since this will be chained and the first message to `hb_ao:resolve` needs to contain `device`.

```erlang [/HyperBEAM/src/dev_mydev.erl]
-export([ add/3 ]).

add(Msg1, Msg2, Opts)->
  Num = maps:get(<<"num">>, Msg1),
  Plus = maps:get(<<"plus">>, Msg2),
  {ok, #{ <<"device">> => <<"mydev@1.0">>, <<"num">> => Num + Plus }}.
```

Also, create a `resolve` method that chains messages and resolves to `add` 3 times with incremental `plus`.

```erlang [/HyperBEAM/src/dev_mydev.erl]
-export([ resolve/3 ]).

resolve(_, _, Opts)->
  Msg1 = #{ <<"device">> => <<"mydev@1.0">>, <<"num">> => 0 },
  io:format("Msg1 ID: ~p~n", [hb_message:id(Msg1)]),

  Msg2 = #{ <<"path">> => <<"add">>, <<"plus">> => 1 },
  io:format("Msg2 ID: ~p~n", [hb_message:id(Msg2)]),
  {ok, Msg3} = hb_ao:resolve(Msg1, Msg2, Opts),
  io:format("Msg3: ~p~n", [Msg3]),
  io:format("Msg3 ID: ~p~n", [hb_message:id(Msg3)]),

  Msg4 = #{ <<"path">> => <<"add">>, <<"plus">> => 2 },
  io:format("Msg4 ID: ~p~n", [hb_message:id(Msg4)]),

  {ok, Msg5} = hb_ao:resolve(Msg3, Msg4, Opts),
  io:format("Msg5: ~p~n", [Msg5]),
  io:format("Msg5 ID: ~p~n", [hb_message:id(Msg5)]),

  Msg6 = #{ <<"path">> => <<"add">>, <<"plus">> => 3 },
  io:format("Msg6 ID: ~p~n", [Msg6]),

  {ok, Msg7} = hb_ao:resolve(Msg5, Msg6, Opts),
  io:format("Msg7: ~p~n", [Msg7]),
  io:format("Msg7 ID: ~p~n", [Msg7]),

  {ok, Msg7}.
```

It's supposed to go...

- `Msg1` : `{ device: "mydev@1.0", num: 0 }`
- `Msg2` : `{ path: "add", plus: 1 }`
- `Msg3 = resolve(Msg1, Msg2, Opts)` : `{ device: "mydev@1.0", num: 1 }`
- `Msg4` : `{ path: "add", plus: 2 }`
- `Msg5 = resolve(Msg3, Msg4, Opts)` : `{ device: "mydev@1.0", num: 3 }`
- `Msg6` : `{ path: "add", plus: 3 }`
- `Msg7 = resolve(Msg5, Msg6, Opts)` : `{ device: "mydev@1.0", num: 6 }`

Let's execute it:

```js [/test/hashpaths.test.js]
await hb.p("/~mydev@1.0/resolve")
```

and we get the logs.

```erlang
Msg1 ID: <<"M3yMP4CqvdUkLXMM2tWQN8PnbT8iy0y70Pf3Mv_x2oA">>
 
Msg2 ID: <<"MFHRUaRJM96_-rtuJ5fEvQXZB5upG1FEQTnWAVoSLOc">>
 
Msg3: #{<<"device">> => <<"mydev@1.0">>,<<"num">> => 1,
        <<"priv">> =>
            #{<<"hashpath">> =>
                  <<"M3yMP4CqvdUkLXMM2tWQN8PnbT8iy0y70Pf3Mv_x2oA/MFHRUaRJM96_-rtuJ5fEvQXZB5upG1FEQTnWAVoSLOc">>}}
 
Msg3 ID: <<"SGXsgupRFDL40G5-rQQBIVRQ9eIJUoxx6g3xfMbASqE">>
 
Msg4 ID: <<"Yf4umWKkjUe4MaBN_ya7DOixRCUrSTG2jqE0DlicC2Q">>
 
Msg5: #{<<"device">> => <<"mydev@1.0">>,<<"num">> => 3,
        <<"priv">> =>
            #{<<"hashpath">> =>
                  <<"20vIiC-SsCktGvR3UeU6zrYkBS8GALoL5jRWKcB1QTo/Yf4umWKkjUe4MaBN_ya7DOixRCUrSTG2jqE0DlicC2Q">>}}
 
Msg5 ID: <<"IEb0vP4sXNGmsTgL1cvN-uo4ulD9uAz7y9cSUiD7Yxw">>
 
Msg6 ID: #{<<"path">> => <<"add">>,<<"plus">> => 3}
 
Msg7: #{<<"device">> => <<"mydev@1.0">>,<<"num">> => 6,
        <<"priv">> =>
            #{<<"hashpath">> =>
                  <<"McDwn8fpdVA4UORmdqvhzYxIn3sEytmK4IrNeE-aDrk/02-Vjx59gbI2vdl5fJNfCSlKDmmj9p0KKGZGn0fi7V4">>}}
 
Msg7 ID: #{<<"device">> => <<"mydev@1.0">>,<<"num">> => 6,
           <<"priv">> =>
               #{<<"hashpath">> =>
                     <<"McDwn8fpdVA4UORmdqvhzYxIn3sEytmK4IrNeE-aDrk/02-Vjx59gbI2vdl5fJNfCSlKDmmj9p0KKGZGn0fi7V4">>}}
```

`Msg7` gets `num = 6`, so it's working as expected, but what's interesting is each resolved message got `hashpath` under `priv`. These hashpaths are the core of the AO Core protocol and HyperBEAM, which keep track of compute steps and make execution verifiable.

You can observe the pattern with hashpaths.

- `Msg3_Hashpath` : `Msg1_ID` + `/` + `Msg2_ID`
- `Msg5_Hashpath` : `20vIiC-SsCktGvR3UeU6zrYkBS8GALoL5jRWKcB1QTo` + `/` + `Msg4_ID`
- `Msg7_Hashpath` : `McDwn8fpdVA4UORmdqvhzYxIn3sEytmK4IrNeE-aDrk` + `/` + `Msg6_ID`

`20vIiC-SsCktGvR3UeU6zrYkBS8GALoL5jRWKcB1QTo` is actually the sha256 hash of `Msg1_ID/Msg2_ID (Msg3_Hashpath)`, and `McDwn8fpdVA4UORmdqvhzYxIn3sEytmK4IrNeE-aDrk` is the sha256 hash of `Msg3_hashpath/Msg5_ID (Msg5_Hashpath)`.

So the hashpaths evolve like the following.

- `Msg3_Hashpath` : `Msg1_ID` + `/` + `Msg2_ID`
- `Msg5_Hashpath` : `hash(Msg3_Hashpath)` + `/` + `Msg4_ID`
- `Msg7_Hashpath` : `hash(Msg5_Hashpath)` + `/` + `Msg6_ID`

The formula is:

- `New_Hashpath = hash(Prev_Hashpath)/New_Msg_ID`

Now, if you sign the 2nd messages to `hb_ao:resolve`, a new hashpath contains the previous hashpath and the new message ID, which usually contains commitments with the sha256 hash of the signatures, which verifies the message content.

You can sign a message with the operator wallet using `hb_message:commit(Msg, Opts)`.

```erlang [/HyperBEAM/src/dev_mydev.erl]
-export([ resolve2/3 ]).

resolve2(_, _, Opts)->
  Msg1 = #{ <<"device">> => <<"mydev@1.0">>, <<"num">> => 0 },
  io:format("Msg1 ID: ~p~n", [hb_message:id(Msg1)]),

  Msg2 = hb_message:commit(#{ <<"path">> => <<"add">>, <<"plus">> => 1 }, Opts),
  io:format("Msg2 ID: ~p~n", [hb_message:id(Msg2)]),
  
  {ok, Msg3} = hb_ao:resolve(Msg1, Msg2, Opts),
  io:format("Msg3: ~p~n", [Msg3]),
  io:format("Msg3 ID: ~p~n", [hb_message:id(Msg3)]),

  Msg4 = hb_message:commit(#{ <<"path">> => <<"add">>, <<"plus">> => 2 }, Opts),
  io:format("Msg4 ID: ~p~n", [hb_message:id(Msg4)]),

  {ok, Msg5} = hb_ao:resolve(Msg3, Msg4, Opts),
  io:format("Msg5: ~p~n", [Msg5]),
  io:format("Msg5 ID: ~p~n", [hb_message:id(Msg5)]),

  Msg6 = hb_message:commit(#{ <<"path">> => <<"add">>, <<"plus">> => 3 }, Opts),
  io:format("Msg6 ID: ~p~n", [Msg6]),

  {ok, Msg7} = hb_ao:resolve(Msg5, Msg6, Opts),
  io:format("Msg7: ~p~n", [Msg7]),
  io:format("Msg7 ID: ~p~n", [Msg7]),

  {ok, Msg7}.
```

External messages passed via URLs are automatically cached with their IDs and readable with `hb_cache:read(ID)`.

You can also internally cache messages with their hashpaths using

- `hb_cache:write_hashpath(Msg, Opts)`

or with their IDs using

- `hb_cache:write(Msg, Opts)`.

```erlang [/HyperBEAM/src/dev_mydev.erl]
-export([ resolve3/3 ]).

resolve3(_, _, Opts)->
  Msg1 = #{ <<"device">> => <<"mydev@1.0">>, <<"num">> => 0 },
  io:format("Msg1 ID: ~p~n", [hb_message:id(Msg1)]),

  Msg2 = hb_message:commit(#{ <<"path">> => <<"add">>, <<"plus">> => 1 }, Opts),
  io:format("Msg2 ID: ~p~n", [hb_message:id(Msg2)]),
  
  {ok, Msg3} = hb_ao:resolve(Msg1, Msg2, Opts),
  io:format("Msg3: ~p~n", [Msg3]),
  io:format("Msg3 ID: ~p~n", [hb_message:id(Msg3)]),
  hb_cache:write_hashpath(Msg3, Opts),

  Msg4 = hb_message:commit(#{ <<"path">> => <<"add">>, <<"plus">> => 2 }, Opts),
  io:format("Msg4 ID: ~p~n", [hb_message:id(Msg4)]),

  {ok, Msg5} = hb_ao:resolve(Msg3, Msg4, Opts),
  io:format("Msg5: ~p~n", [Msg5]),
  io:format("Msg5 ID: ~p~n", [hb_message:id(Msg5)]),
  hb_cache:write_hashpath(Msg5, Opts),
  
  Msg6 = hb_message:commit(#{ <<"path">> => <<"add">>, <<"plus">> => 3 }, Opts),
  io:format("Msg6 ID: ~p~n", [Msg6]),

  {ok, Msg7} = hb_ao:resolve(Msg5, Msg6, Opts),
  io:format("Msg7: ~p~n", [Msg7]),
  io:format("Msg7 ID: ~p~n", [Msg7]),
  hb_cache:write_hashpath(Msg7, Opts),
  
  {ok, Msg7#{ 
    <<"hashpath_3">> => maps:get(<<"hashpath">>, maps:get(<<"priv">>, Msg3)),
    <<"hashpath_5">> => maps:get(<<"hashpath">>, maps:get(<<"priv">>, Msg5)),
    <<"hashpath_7">> => maps:get(<<"hashpath">>, maps:get(<<"priv">>, Msg7))
  }}.
```

## Reading Cached Messages with ID / Hashpath

You can internally read any cached messages with `hb_cache:read(ID, Opts)` or `hb_cache:read(Hashpath, Opts)`.

But also, HyperBEAM makes cached messages accessible from external URL paths with `/[msg_id]` and `/[hashpath]` format.

```js [/test/hashpaths.test.js]
const out = await hb.p("/~mydev@1.0/resolve3")
const msg3 = await hb.g(`/${out.hashpath_3}`)
const msg5 = await hb.g(`/${out.hashpath_5}`)
const msg7 = await hb.g(`/${out.hashpath_7}`)

assert.deepEqual({ device: "mydev@1.0", num: 1 }, msg3)
assert.deepEqual({ device: "mydev@1.0", num: 3 }, msg5)
assert.deepEqual({ device: "mydev@1.0", num: 6 }, msg7)

```
You can chain compute steps using this URL schema, but we'll talk about it in the next chapter.

## Hashpath of Signed Requests

Any signed request to a HyperBEAM node returns a hashpath as `tag` in `signature-input` in the HTTP headers, which is the hashpath of the passed messages to the resolved device method.

`hb.post` automatically extracts it from the response for you.

```js [/test/hashpaths.test.js]
import { id } from "wao/utils"

const { out, hashpath } = await hb.post({ path: "/~mydev@1.0/forward" })
const { msg1, msg2 } = JSON.parse(out)
assert.equal(`${id(msg1)}/${id(msg2)}`, hashpath)
```

This hashpath contains the IDs of the messages passed to our `forward` method in this case.

- `hashpath` : `_msg1_id/_msg2_id`

This would be extremely useful if the hashpaths were automatically cached as they contain the compute results, but unfortunately, this doesn't seem to be the case with the current HyperBEAM implementation.
