# Custom Devices and Codecs

We are going to learn the core codecs such as path flattening, HTTP message signatures, and AO types while building a custom HyperBEAM device.

## Accessible Device Methods

If you look into any `dev_` prefixed Erlang files under `HyperBEAM/src`, device methods are defined and exported in `method_name/arity` format. HyperBEAM automatically routes HTTP requests to device methods defined with arity of 3. So any methods exported as `method_name/3` are automatically accessible.

For instance, the `dev_meta.erl` file has these lines, which make `/~meta@1.0/info` and `/~meta@1.0/build` accessible via URL endpoints.

```erlang
-export([info/1, info/3, build/3, handle/2, adopt_node_message/2, is/2, is/3]).

info(_, Request, NodeMsg) ->

build(_, _, _NodeMsg) ->
```

Another example is `dev_codec_json.erl` with `deserialize/3` and `serialize/3` exposed.

```erlang
-export([deserialize/3, serialize/3]).

deserialize(Base, Req, Opts) ->

serialize(Base, _Msg, _Opts) ->
```

The device names are defined in `hb_opt.erl` under `preloaded_devices`.

```erlang
preloaded_devices => [
  ...
  #{<<"name">> => <<"json@1.0">>, <<"module">> => dev_codec_json},
  ...
  #{<<"name">> => <<"meta@1.0">>, <<"module">> => dev_meta},
  ...
],
```

This is exactly how you can build your own custom devices.

## Building Custom Devices

Create `wao@1.0.erl` under `HyperBEAM/src`, and define the `info/3` method.

The following is the minimum viable HyperBEAM device implementation.

```erlang
-module(dev_wao).
-export([ info/3 ]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

info(Msg1, Msg2_, Opts) ->
    {ok, #{ <<"version">> => <<"1.0">> }}.
```

Also, add the device to `preloaded_devices` in `hb_opts.erl`.

```erlang
preloaded_devices => [
  ...
  #{<<"name">> => <<"json@1.0">>, <<"module">> => dev_codec_json},
  ...
  #{<<"name">> => <<"meta@1.0">>, <<"module">> => dev_meta},
  ...
  #{<<"name">> => <<"wao@1.0">>, <<"module">> => dev_wao}
],
```

Now you can test your device using WAO. Don't forget to preload your `wao` device.

```js
import assert from "assert"
import { describe, it, before, after, beforeEach } from "node:test"
import { HyperBEAM, toAddr } from "wao/test"
import { HB } from "wao"
import { resolve } from "path"
import { readFileSync } from "fs"

const cwd = "../dev/wao/HyperBEAM"
const wallet = resolve(process.cwd(), cwd, ".wallet.json")
const jwk = JSON.parse(readFileSync(wallet, "utf8"))
const addr = toAddr(jwk.n)

describe("HyperBEAM", function () {
  let hbeam, hb
  before(async () => {
    hbeam = await new HyperBEAM({
      devices: ["json", "structured", "httpsig", "flat", "meta", "wao"],
      cwd,
    }).ready()
  })
  beforeEach(async () => (hb = await new HB({}).init(jwk)))
  after(async () => hbeam.kill())

  it("should test wao@1.0 device", async () => {
	const { out } = await hb.get({ path: "/~wao@1.0/info"})
	console.log(out)
    assert.equal(out.version, "1.0")
  })
})
```

You receive what you return from the `info` method, but it also comes with extra metadata since it's just an HTTP message and goes through a pipeline of data mutation before it gets back to you.

The following is what you would get in the response.

```js
{
  accept: '*/*',
  'accept-encoding': 'gzip, deflate',
  'accept-language': '*',
  connection: 'keep-alive',
  device: 'wao@1.0',
  host: 'localhost:10001',
  'sec-fetch-mode': 'cors',
  'user-agent': 'node',
  version: '1.0'
}
```

## TABM <-> JSON

One way to purify the return value is to return stringified JSON using the `json@1.0` device internally. The device has a `dev_codec_json:to/1` method to convert `TABM` to `JSON`.

The internal device names are defined with `preloaded_devices` in `hb_opts.erl`.

- `#{<<"name">> => <<"json@1.0">>, <<"module">> => dev_codec_json}`

And you can internally execute all exposed methods.

```erlang
-module(dev_codec_json).
-export([to/1, from/1, commit/3, verify/3, committed/1, content_type/1]).
-export([deserialize/3, serialize/3]).
```

When you define a new method, don't forget to export it from `wao@1.0`.

From this point on, we'll always assume you're correctly exporting new methods.

```erlang
-export([ info/3, info_json/3 ]).

info_json(Msg1, Msg2_, Opts) ->
    JSON = dev_codec_json:to(#{ <<"version">> => <<"1.0">> }),
    {ok, JSON}.
```

Now you can just `JSON.parse(out)` to get exactly what you return.

```js
const { out } = await hb.get({ path: "/~wao@1.0/info_json" })
const json = JSON.parse(out)
assert.deepEqual(json, { version: "1.0" })
```
You can do the opposite with `dev_codec_json:from/1` too. Let's create an `inc/3` method to convert stringified JSON in `body` to TABM, increment the `num` field, and return it.

```erlang
inc(Msg1, Msg2_, Opts) ->
    Body = maps:get(<<"body">>, Msg1),
    TABM = dev_codec_json:from(Body),
    Num = maps:get(<<"num">>, TABM),
    {ok, #{ <<"num">> => Num + 1 }}.
```	
When you send data in `body`, you need the `POST` method.

```js
const { out: { num }} = await hb.post({
  path: "/~wao@1.0/inc",
  body: JSON.stringify({ num: 1 }),
})
assert.equal(num, 2)
```

An HTTP message consists of `headers` and `body`, and AO Core and HyperBEAM utilize both of them in messages. `GET` method doesn't have `body` and headers only support flat string values. So when you need to send a message with complex data, you almost always need `POST`. The AO codecs with `flat`, `structured`, and `httpsig` devices exist to circumvent these HTTP header limitations.

## Device Methods

You can create any arbitrary methods in a device, but methods to expose via URL endpoints need to be in a specific format.

```erlang
method(Msg1, Msg2_, Opts) ->
  % write method logic here
  {ok, Ret}.
```

Let's create a `forward` method to just forward `Msg1`, `Msg2`, and `Opts` in JSON format and examine what they are. We filter out private keys from `Opts` with `hb_private:reset` as it contains sensitive data and incompatible data types to convert to JSON. We can also log these objects with `io:format`.

```erlang
forward(Msg1, Msg2, Opts) ->
  io:format("Msg1: ~p~n~nMsg2: ~p~n~nOpts: ~p~n", [Msg1, Msg2, Opts]),
  JSON = dev_codec_json:to(#{
    <<"msg1">> => Msg1,
    <<"msg2">> => Msg2,
    <<"opts">> => hb_private:reset(Opts)
  }),
  {ok, JSON}.
```
Let's send something very simple with `GET`. You can only send string parameters with `GET`.

```js
const { out } = await hb.get({ path: "/~wao@1.0/forward", key: "abc" })
console.log(JSON.parse(out))
```

This is the response.

```js
{
  "msg1": {
    "accept": "*/*",
    "accept-encoding": "gzip, deflate",
    "accept-language": "*",
    "connection": "keep-alive",
    "device": "wao@1.0",
    "host": "localhost:10001",
    "key": "abc",
    "method": "GET",
    "sec-fetch-mode": "cors",
    "user-agent": "node"
  },
  "msg2": {
    "accept": "*/*",
    "accept-encoding": "gzip, deflate",
    "accept-language": "*",
    "commitments": {
      "jNI0FLgi9Lz2UT_l1sK2TCPZMCIwFpPcOK3e3cqdRwo": {
        "alg": "hmac-sha256",
        "commitment-device": "httpsig@1.0"
      }
    },
    "connection": "keep-alive",
    "host": "localhost:10001",
    "key": "abc",
    "method": "GET",
    "path": "forward",
    "sec-fetch-mode": "cors",
    "user-agent": "node"
  },
  "opts": {
    "mode": "debug",
    "node_history": [],
    ...
   }
}
```

You can tell `opts` is the node configuration, and `msg1` and `msg2` are very similar except that `msg1` has `device`, and `msg2` has `commitments` and `path`. They both have `key="abc"`, which is what we sent as a parameter. Other fields in `msg1` and `msg2` are the same metadata about the HTTP protocol.

We can strip the metadata down to these minimum differences.

```js
{
  "msg1": {
    "device": "wao@1.0",
    "key": "abc",
  },
  "msg2": {
    "commitments": {
      "jNI0FLgi9Lz2UT_l1sK2TCPZMCIwFpPcOK3e3cqdRwo": {
        "alg": "hmac-sha256",
        "commitment-device": "httpsig@1.0"
      }
    },
    "key": "abc",
    "path": "forward",
  }
}
```

Things will be clearer when we send complex data with `POST`.

```js

const { out } = await hb.post({
  path: "/~wao@1.0/forward",
  key: "abc",
  list: [1, 2, 3],
  map: { abc: "123" },
  bool: true,
  body: "test_body",
})

```

And this is the response.

```js
{
 "msg1": {
    "body": "test_body",
    "bool": "\"true\"",
    "content-length": "253",
    "content-type": "multipart/form-data; boundary=\"eyja4UA4reu5SLEKVqY67NG8Q-jMdqEFmleD-hhSJKM\"",
    "device": "wao@1.0",
    "key": "abc",
    "list": "\"(ao-type-integer) 1\", \"(ao-type-integer) 2\", \"(ao-type-integer) 3\"",
    "map": { "abc": "123" },
    "method": "POST",
    "num": "123"
  },
  "msg2": {
    "body": "test_body",
    "bool": true,
    "commitments": {
      "R_w4EInkh2sw2sVCdqgY8xIbm-MmqOXDdWYYtw3dmeI": {
        "alg": "hmac-sha256",
        "commitment-device": "httpsig@1.0",
        "signature": "http-sig-bba7e22451416f77=:XzfPi6oBHsbO/yPqbVXkcuVBaNP5EomB29mwV+sVmjXg1wcZK0Ca3VT/mVjL3Wz4hj4ykPgLFsvE/P8HN5wQTE5haAYUh7lgEA62TGAg542jM9QRc8ujJono6l19pGJVv4oDDkVdO1PkPtyHgYslopF87zXBwF+EcfFicxVz5Px2ERy44IEkJWZpb/z0FrWwqiu/qhqypwocOPAgack2AD7r1/IJ2DWQmfdQSWqjzWiCJcaFkm1yoOciyw8yhgOUcBALqCnl0HnH5+utOcql+F5WZvYbl8l5XdvwRIZsl6mcmGwqF1uG1MCRU1qVrwMSuUMiDArgUyfzMVu1y8kqh4UE0qOjp9LVmjvyJEPuyei0/BmJ9uibi2JvV9q/TZn92bKYC0C/JaHVLfs1W/esZDdZgsifIJaAs/W2eCnhis0vB1Rd6KCguTEbRWxiD7K8VzKhOIWfWU0QEvrsNqgpMJI1G7UwvqDQPgeS2eSl95NdjoagriDF5ZRf2pSQsAcCaIqp5hP47JXaPwVVgI0C02rgV4X6Cx8LXAn1K/ZeVBdCjlNN621iPwdFAygRXTiNv5Cp3LJ5XsYPcbyFbbCzeN+B0HUpcQthzqX5xBGTKiMVJZdF52GvX9v3yxHxytGczJI0aiBUmyWCAW3gfWMi6yNOiUJsBNz1aQpYFOivOOA=:",
        "signature-input": "http-sig-bba7e22451416f77=(\"key\" \"num\" \"list\" \"bool\" \"ao-types\" \"content-type\" \"content-digest\" \"content-length\");alg=\"rsa-pss-sha512\";keyid=\"o1kvTqZQ0wbS_WkdwX70TFCk7UF76ldnJ85l8iRV7t6mSlzkXBYCecb-8RXsNEQQmO0KergtHOvhuBJmB6YXaYe_UftI_gendojfIa6jlTgw-qmH6g4_oErI8djDRbQSm-5nCfGVRuYxsNZLYDeqw4gFb9K3b1h7tuMoLd6-d5pkaLfTMUNcvs2OqpkLo0i_av746FieaURdWozwFqO0APtdA7pLHDqQZDMNdTmsUBJFszL6SOa1bKe5cUWnrq4uaW4NAN3JAQniILKGsKZENeKtfXwiKVaFJtriWWsbhOaNT0JLcuBAwXQAP59RXzcr8bRY6XFn8zBmEmZBGszOD9c9ssDENRFDa5uyVhk8XgIgQjErAWYd9T6edrYcIp3R78jhNK_nLiIBBz8_Oz3bLjL5i_aiV2gpfIbd44DCHihuuxSWRAPJxhEy9TS0_QbVOIWhcDTIeEJE3aRPTwSTMt1_Fec7i9HJWN0mvMbAAJw8k6HxjA3pFZiCowZJw7FBwMAeYgEwIeB82f-S2-PtFLwR9i0tExo36hEBHqaS4Y-O3NGgQ8mKnhT7Z1EfxEbA2BpR9oL8rJFEnPIrHHu7B88OHDDfnfRD3D79fKktnisC7XOuwbHG3TQo0_j4_mElH7xj_7IyAbmCUHDd-eRa482wOYXBB01DGnad901qaHU\""
      },
      "_wihx0O9rbQFVBQT4OdEqYx8Vyp48y_PXiN66Pyzfgw": {
        "alg": "rsa-pss-sha512",
        "commitment-device": "httpsig@1.0",
        "committer": "Tbun4iRRQW93gUiSAmTmZJ2PGI-_yYaXsX69ETgzSRE",
        "signature": "http-sig-bba7e22451416f77=:XzfPi6oBHsbO/yPqbVXkcuVBaNP5EomB29mwV+sVmjXg1wcZK0Ca3VT/mVjL3Wz4hj4ykPgLFsvE/P8HN5wQTE5haAYUh7lgEA62TGAg542jM9QRc8ujJono6l19pGJVv4oDDkVdO1PkPtyHgYslopF87zXBwF+EcfFicxVz5Px2ERy44IEkJWZpb/z0FrWwqiu/qhqypwocOPAgack2AD7r1/IJ2DWQmfdQSWqjzWiCJcaFkm1yoOciyw8yhgOUcBALqCnl0HnH5+utOcql+F5WZvYbl8l5XdvwRIZsl6mcmGwqF1uG1MCRU1qVrwMSuUMiDArgUyfzMVu1y8kqh4UE0qOjp9LVmjvyJEPuyei0/BmJ9uibi2JvV9q/TZn92bKYC0C/JaHVLfs1W/esZDdZgsifIJaAs/W2eCnhis0vB1Rd6KCguTEbRWxiD7K8VzKhOIWfWU0QEvrsNqgpMJI1G7UwvqDQPgeS2eSl95NdjoagriDF5ZRf2pSQsAcCaIqp5hP47JXaPwVVgI0C02rgV4X6Cx8LXAn1K/ZeVBdCjlNN621iPwdFAygRXTiNv5Cp3LJ5XsYPcbyFbbCzeN+B0HUpcQthzqX5xBGTKiMVJZdF52GvX9v3yxHxytGczJI0aiBUmyWCAW3gfWMi6yNOiUJsBNz1aQpYFOivOOA=:",
        "signature-input": "http-sig-bba7e22451416f77=(\"key\" \"num\" \"list\" \"bool\" \"ao-types\" \"content-type\" \"content-digest\" \"content-length\");alg=\"rsa-pss-sha512\";keyid=\"o1kvTqZQ0wbS_WkdwX70TFCk7UF76ldnJ85l8iRV7t6mSlzkXBYCecb-8RXsNEQQmO0KergtHOvhuBJmB6YXaYe_UftI_gendojfIa6jlTgw-qmH6g4_oErI8djDRbQSm-5nCfGVRuYxsNZLYDeqw4gFb9K3b1h7tuMoLd6-d5pkaLfTMUNcvs2OqpkLo0i_av746FieaURdWozwFqO0APtdA7pLHDqQZDMNdTmsUBJFszL6SOa1bKe5cUWnrq4uaW4NAN3JAQniILKGsKZENeKtfXwiKVaFJtriWWsbhOaNT0JLcuBAwXQAP59RXzcr8bRY6XFn8zBmEmZBGszOD9c9ssDENRFDa5uyVhk8XgIgQjErAWYd9T6edrYcIp3R78jhNK_nLiIBBz8_Oz3bLjL5i_aiV2gpfIbd44DCHihuuxSWRAPJxhEy9TS0_QbVOIWhcDTIeEJE3aRPTwSTMt1_Fec7i9HJWN0mvMbAAJw8k6HxjA3pFZiCowZJw7FBwMAeYgEwIeB82f-S2-PtFLwR9i0tExo36hEBHqaS4Y-O3NGgQ8mKnhT7Z1EfxEbA2BpR9oL8rJFEnPIrHHu7B88OHDDfnfRD3D79fKktnisC7XOuwbHG3TQo0_j4_mElH7xj_7IyAbmCUHDd-eRa482wOYXBB01DGnad901qaHU\""
      }
    },
    "content-length": "253",
    "content-type": "multipart/form-data; boundary=\"eyja4UA4reu5SLEKVqY67NG8Q-jMdqEFmleD-hhSJKM\"",
    "key": "abc",
    "list": [1, 2, 3],
    "map": { "abc": "123" },
    "method": "POST",
    "num": 123,
    "path": "forward"
  },
  "opts": {
    "mode": "debug",
    "node_history": [],
    "stack_print_prefixes": [[104, 98], [100, 101, 118], [97, 114]],
    "only": "local",
    ...
  }
}

```

## Encoding / Decoding Steps

We'll explain the commitments later, but now, if you strip down the 2 msgs:

```js
{
 "msg1": {
    "body": "test_body",
    "bool": "\"true\"",
    "device": "wao@1.0",
    "key": "abc",
    "list": "\"(ao-type-integer) 1\", \"(ao-type-integer) 2\", \"(ao-type-integer) 3\"",
    "map": { "abc": "123" },
    "method": "POST",
    "num": "123"
  },
  "msg2": {
    "body": "test_body",
    "bool": true,
    "key": "abc",
    "list": [1, 2, 3],
    "map": { "abc": "123" },
    "method": "POST",
    "num": 123,
    "path": "forward"
  }
}
```
You can observe that `bool`, `list`, and `num` are encoded in some string form. The `msg1` fields are encoded by `structured@1.0` and `msg2` fields are decoded. FYI, `msg1` is also different from the form we sent; `msg1` is already decoded by `httpsig@1.0` device.


So a client encodes a message with `httpsig@1.0`, then `structured@1.0`, then signs it with `http-message-signatures`, then sends it to a HyperBEAM node.

1. `data = { path: "/~wao@1.0/forward", key: "abc" }`
2. `structured_encoded = encode_by_structured(data)`
3. `httpsig_encoded = encode_by_httpsig(structured_encoded)`
4. `signed = sign(httpsig_encoded)`
5. `send(signed)`

The node receives it, verifies the signature, then decodes it first with `structured@1.0` (msg1), then with `httpsig@1.0` (msg2).

1. `Msg0 + Commitments = dev_codec_httpsig:verify(HTTP_MSG)`
2. `Msg1 = dev_codec_httpsig:to(Msg0) + Device`
3. `Msg2 = dev_codec_structured:to(Msg1) + Commitments + Path`

`flat@1.0` is used to flatten and unflatten nested object paths in the `httpsig@1.0` device since HTTP headers and body can only handle string values, not nested structures.
