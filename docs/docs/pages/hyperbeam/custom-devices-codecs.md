# Custom Devices and Codecs

We are going to learn the core codecs such as path flattening, HTTP message signatures, and AO types while building a custom HyperBEAM device.

## Accessible Device Methods

If you look into any `dev_` prefixed Erlang files under `HyperBEAM/src`, device methods are defined and exported in `method_name/arity` format. HyperBEAM automatically routes HTTP requests to device methods defined with arity of 3. So any methods exported as `method_name/3` are automatically accessible.

For instance, the `dev_meta.erl` file has these lines, which make `/~meta@1.0/info` and `/~meta@1.0/build` accessible via URL endpoints.

```erlang [/HyperBEAM/src/dev_meta.erl]
-export([info/1, info/3, build/3, handle/2, adopt_node_message/2, is/2, is/3]).

info(_, Request, NodeMsg) ->

build(_, _, _NodeMsg) ->
```

Another example is `dev_codec_json.erl` with `deserialize/3` and `serialize/3` exposed.

```erlang [/HyperBEAM/src/dev_codec_json.erl]
-export([deserialize/3, serialize/3]).

deserialize(Base, Req, Opts) ->

serialize(Base, _Msg, _Opts) ->
```

The device names are defined in `hb_opt.erl` under `preloaded_devices`.

```erlang [/HyperBEAM/src/hb_opts.erl]
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

Create `dev_mydev.erl` under `/HyperBEAM/src`, and define the `info/3` method.

The following is the minimum viable HyperBEAM device implementation.

```erlang [/HyperBEAM/src/dev_mydev.erl]
-module(dev_mydev).
-export([ info/3 ]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

info(Msg1, Msg2_, Opts) ->
    {ok, #{ <<"version">> => <<"1.0">> }}.
```

Also, add the device to `preloaded_devices` in `hb_opts.erl`.

```erlang [/HyperBEAM/src/hb_opts.erl]
preloaded_devices => [
  ...
  #{<<"name">> => <<"json@1.0">>, <<"module">> => dev_codec_json},
  ...
  #{<<"name">> => <<"meta@1.0">>, <<"module">> => dev_meta},
  ...
  #{<<"name">> => <<"mydev@1.0">>, <<"module">> => dev_mydev}
],
```

Now you can test your device using WAO. Don't forget to preload your `mydev` device.

```js [/test/custom-devices-codecs.test.js]
import assert from "assert"
import { describe, it, before, after } from "node:test"
import { HyperBEAM } from "wao/test"

const mydev = { name: "mydev@1.0", module: "dev_mydev" }
const devices = ["json", "structured", "httpsig", "flat", "meta", mydev]

describe("Custom Devices and Codecs", function () {
  let hbeam, hb
  before(async () => {
    hbeam = await new HyperBEAM({ devices, reset: true }).ready()
	hb = hbeam.hb
  })
  after(async () => hbeam.kill())

  it("should get info on mydev", async () => {
    const { headers, body } = await hb.get({ path: "/~mydev@1.0/info" })
    console.log(headers)
    console.log(body)
    assert.equal(headers.version, "1.0")
  })
})
```

WAO can handle what we are going to learn and decode through the next few chapters, but we will intentionally work with the standard HTTP `headers` and `body`.

Let's look into the returned `headers`. You receive what you return from the `info` method, but it also comes with extra metadata since it's just an HTTP message and goes through a pipeline of data mutation before it gets back to you.

The following is what you would get in the response headers.

```js [HTTP Headers]
{
  'access-control-allow-methods': 'GET, POST, PUT, DELETE, OPTIONS',
  'access-control-allow-origin': '*',
  'ao-types': 'status="integer"',
  date: 'Wed, 16 Jul 2025 14:56:22 GMT',
  server: 'Cowboy',
  signature: 'http-sig-bba7e22451416f77=:HeX5CGPYMkyE703j09uMkQktWSA4sL+xeHJjNPveOJ1eaF3KdyeASmHmH/Iwz66xb1a+JJAr+9Uga8FuxfcM/yvs8Axcwp6lZCkhosnknh47hC2Hq0/85kpTu9gDtioNs0Sn/DMQGMZDcGfDpl8qOALG3zEWg5xaW2oj62jhY3uI9qEzfnHQvZStvOXUdwRnEUof1zzBfdfgpRdpvKmX2sTTTd9qmyQZCvBBEI4x7M+br2xFLOAbLSfz01TpSKQ3cXk610eHkI8gEepzoEReEfeVcm3RLkxSbfZICsF08CIlnExx4qCDFZJSOuaKJ98G04GC2V9b5yuDebaudqTrP0P0ok64wWSJ5OMLC93Br+dzx9VilOa9fShkObT/6gpfaqT+tqda4p1xxEtlHhFqmhMuvu97LT3ODwMG7hR1r1HChVtFdBeBr9udHvuaUi85I8aXCOkduaJYzLB2gKAFUvJ0voTuO3jL32lTELOAWZmZN7p5+hZdRo8PCRV9LmW+P3jU8IPAHsLXC3/DadNra+9ovx13QbwdIjkBOhXqgEHGmxdeCwU7QPlF4JSBw3Nx/UA6CmMLKyN7+qhUVrdpD3Wwl2ywUXlsGUNqhzYItR5YBT7xfuhSl20K0QxTrfi1XQozmOqMYB+fYjde+Et7HV0n5/4eAvxh+8wIG+pxtj4=:',
  'signature-input': 'http-sig-bba7e22451416f77=("ao-types" "@status" "version");alg="rsa-pss-sha512";keyid="o1kvTqZQ0wbS_WkdwX70TFCk7UF76ldnJ85l8iRV7t6mSlzkXBYCecb-8RXsNEQQmO0KergtHOvhuBJmB6YXaYe_UftI_gendojfIa6jlTgw-qmH6g4_oErI8djDRbQSm-5nCfGVRuYxsNZLYDeqw4gFb9K3b1h7tuMoLd6-d5pkaLfTMUNcvs2OqpkLo0i_av746FieaURdWozwFqO0APtdA7pLHDqQZDMNdTmsUBJFszL6SOa1bKe5cUWnrq4uaW4NAN3JAQniILKGsKZENeKtfXwiKVaFJtriWWsbhOaNT0JLcuBAwXQAP59RXzcr8bRY6XFn8zBmEmZBGszOD9c9ssDENRFDa5uyVhk8XgIgQjErAWYd9T6edrYcIp3R78jhNK_nLiIBBz8_Oz3bLjL5i_aiV2gpfIbd44DCHihuuxSWRAPJxhEy9TS0_QbVOIWhcDTIeEJE3aRPTwSTMt1_Fec7i9HJWN0mvMbAAJw8k6HxjA3pFZiCowZJw7FBwMAeYgEwIeB82f-S2-PtFLwR9i0tExo36hEBHqaS4Y-O3NGgQ8mKnhT7Z1EfxEbA2BpR9oL8rJFEnPIrHHu7B88OHDDfnfRD3D79fKktnisC7XOuwbHG3TQo0_j4_mElH7xj_7IyAbmCUHDd-eRa482wOYXBB01DGnad901qaHU";tag="PBF4RF2dpjDPZ_uujtBb--DcOx_Z4EyeUdjAerMpsAw/jNI0FLgi9Lz2UT_l1sK2TCPZMCIwFpPcOK3e3cqdRwo"',
  status: '200',
  'transfer-encoding': 'chunked',
  version: '1.0'
}
```

## Erlang \<-\> JSON

One way to purify the return value is to return stringified JSON using the `json@1.0` device internally. The device has a `dev_codec_json:to/1` method to convert an `Erlang` object to `JSON`.

The internal device names are defined with `preloaded_devices` in `hb_opts.erl`.

- `#{<<"name">> => <<"json@1.0">>, <<"module">> => dev_codec_json}`

And you can internally execute all exposed methods.

```erlang [/HyperBEAM/src/dev_codec_json.erl]
-module(dev_codec_json).
-export([to/1, from/1, commit/3, verify/3, committed/1, content_type/1]).
-export([deserialize/3, serialize/3]).
```

When you define a new method, don't forget to export it from `mydev@1.0`.

From this point on, we'll always assume you're correctly exporting new methods.

```erlang  [/HyperBEAM/src/dev_mydev.erl]
-export([ info_json/3 ]).

info_json(Msg1, Msg2_, Opts) ->
    JSON = dev_codec_json:to(#{ <<"version">> => <<"1.0">> }),
    {ok, JSON}.
```

Now stringified JSON is returned in `body`, and you can just `JSON.parse(body)` to get exactly what you return.

```js [/test/custom-devices-codecs.test.js]
const { body } = await hb.get({ path: "/~mydev@1.0/info_json" })
const json = JSON.parse(body)
assert.deepEqual(json, { version: "1.0" })
```
You can also pass JSON using `dev_codec_json:from/1`, too. Let's create an `hello/3` method to convert stringified JSON in `body` to an `Erlang` object, add `Hello` to the `name` field, and return it as JSON again.

```erlang  [/HyperBEAM/src/dev_mydev.erl]
-export([ hello/3 ]).

hello(Msg1, Msg2_, Opts) ->
    Body = maps:get(<<"body">>, Msg1),
    OBJ = dev_codec_json:from(Body),
    Name = maps:get(<<"name">>, OBJ),
    Hello = <<<<"Hello, ">>/binary, Name/binary, <<"!">>/binary>>,
    JSON = dev_codec_json:to(#{ <<"hello">> => Hello }),
    {ok, JSON}.
```	

When you send data in `body`, you need the `POST` method.

```js [/test/custom-devices-codecs.test.js]
const { body } = await hb.post({
  path: "/~mydev@1.0/hello",
  body: JSON.stringify({ name: "Wao" }),
})
const { hello } = JSON.parse(body)
assert.equal(hello, "Hello, Wao!")
```

An HTTP message consists of `headers` and `body`, and AO Core and HyperBEAM utilize both of them in messages. `GET` method doesn't have `body` and headers only support flat string values. So when you need to send a message with complex data, you almost always need `POST`. The AO codecs with `flat`, `structured`, and `httpsig` devices exist to circumvent these HTTP header limitations.

## Device Methods

You can create any arbitrary methods in a device, but methods to expose via URL endpoints need to be in a specific format.

```erlang [Minimum Viable URL Exposed Method]
method(Msg1, Msg2_, Opts) ->
  % write method logic here
  {ok, Ret}.
```

Let's create a `forward` method to just forward `Msg1`, `Msg2`, and `Opts` in JSON format and examine what they are. We filter out private keys from `Opts` with `hb_private:reset` as it contains sensitive data and incompatible data types to convert to JSON. We can also log these objects with `io:format`.


```erlang [/HyperBEAM/src/dev_mydev.erl]
-export([ forward/3 ]).

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

```js [/test/custom-devices-codecs.test.js]
const { body } = await hb.get({ path: "/~mydev@1.0/forward", key: "abc" })
console.log(JSON.parse(body))
```

This is the response.

```json [JSON Response]
{
  "msg1": {
    "accept": "*/*",
    "accept-encoding": "gzip, deflate",
    "accept-language": "*",
    "connection": "keep-alive",
    "device": "mydev@1.0",
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
    "hb_config_location": "config.flat",
    "http_server": "Tbun4iRRQW93gUiSAmTmZJ2PGI-_yYaXsX69ETgzSRE",
    ...
  }
}
```

You can tell `opts` is the node configuration, and `msg1` and `msg2` are very similar except that `msg1` has `device`, and `msg2` has `commitments` and `path`. They both have `key="abc"`, which is what we sent as a parameter. Other fields in `msg1` and `msg2` are the same metadata about the HTTP protocol.

We can strip the metadata down to these minimum differences.

```json
{
  "msg1": {
    "device": "mydev@1.0",
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

```js [/test/custom-devices-codecs.test.js]
const { out } = await hb.post({
  path: "/~wao@1.0/forward",
  key: "abc",
  list: [1, 2, 3],
  map: { abc: "123" },
  bool: true,
  body: "test_body",
})
console.log(JSON.parse(out))
```

And this is the response.

```json [JSON Response]
{
  "msg1": {
    "body": "test_body",
    "bool": "\"true\"",
    "content-length": "253",
    "content-type": "multipart/form-data; boundary=\"eyja4UA4reu5SLEKVqY67NG8Q-jMdqEFmleD-hhSJKM\"",
    "device": "mydev@1.0",
    "key": "abc",
    "list": "\"(ao-type-integer) 1\", \"(ao-type-integer) 2\", \"(ao-type-integer) 3\"",
    "map": { "abc": "123" },
    "method": "POST"
  },
  "msg2": {
    "body": "test_body",
    "bool": true,
    "commitments": {
      "ovgxZflZZcY_kXWpV6yWf_ilaGuMDh7PUGC1YNhJQ90": {
        "alg": "hmac-sha256",
        "commitment-device": "httpsig@1.0",
        "signature": "http-sig-bba7e22451416f77=:kqtatfcnCrmmJiEc1GT3hKKU6tUVRy34hDN6z1vN5UHCwNZ4f+tu9FafZ/mOx8loq11DgdV8S7Xvxk5LzytMAtV1SmAArEQ1VbMJkS+bIiNksd4qmU13JkQjz+a90FYVUDKn0uU+cRUDx+7wVh4Rco27WEBj/E5yVreKcmG0fpORHi4DMV219cb0zUAdDEqY/FdvdlC+Xr91xQzDwcwS8goeHS879P6FLRo5BubLWx/bbJXoS2BEGowkWAORP1jooWe+oNIcWbMWA1CUpPTih2VXbUQcdRto5DDjwXw90nxD3UVPLegweGOZrASuccG9oFg/++mJeFFz3W6cy3Eg84WmrMjfbzsUb6WVcKti83YZYTo7onUDwad2wVUe2WDCuMLm8TFhwP8zwU/MHSfcahRnZasnroPwxvYRjFNWa5USyqGaZ7uM/wqArGKL/2dlh3bIphEmTjtYBc9q2tlosNgPngwnPu6qbEFQpcDEzsODQQBYrnP9HA6HIqsC+dWPINw0xueFqnhu5bv3Y+Y17vFc4zOraBpVCZUEMKEDPgNHe2vMjFVfgIbKp9I9Xu+8vYd8sL+2p+lgkrXVjNCS07XYjVHj855GKCmIZHs9fZa0dfLghOdDfnbexzCpSDIVnfstZx5yniXh00Rk3g2wUAWB7Sq7nRG86BiOQP+EcHY=:",
        "signature-input": "http-sig-bba7e22451416f77=(\"key\" \"list\" \"bool\" \"ao-types\" \"content-type\" \"content-digest\" \"content-length\");alg=\"rsa-pss-sha512\";keyid=\"o1kvTqZQ0wbS_WkdwX70TFCk7UF76ldnJ85l8iRV7t6mSlzkXBYCecb-8RXsNEQQmO0KergtHOvhuBJmB6YXaYe_UftI_gendojfIa6jlTgw-qmH6g4_oErI8djDRbQSm-5nCfGVRuYxsNZLYDeqw4gFb9K3b1h7tuMoLd6-d5pkaLfTMUNcvs2OqpkLo0i_av746FieaURdWozwFqO0APtdA7pLHDqQZDMNdTmsUBJFszL6SOa1bKe5cUWnrq4uaW4NAN3JAQniILKGsKZENeKtfXwiKVaFJtriWWsbhOaNT0JLcuBAwXQAP59RXzcr8bRY6XFn8zBmEmZBGszOD9c9ssDENRFDa5uyVhk8XgIgQjErAWYd9T6edrYcIp3R78jhNK_nLiIBBz8_Oz3bLjL5i_aiV2gpfIbd44DCHihuuxSWRAPJxhEy9TS0_QbVOIWhcDTIeEJE3aRPTwSTMt1_Fec7i9HJWN0mvMbAAJw8k6HxjA3pFZiCowZJw7FBwMAeYgEwIeB82f-S2-PtFLwR9i0tExo36hEBHqaS4Y-O3NGgQ8mKnhT7Z1EfxEbA2BpR9oL8rJFEnPIrHHu7B88OHDDfnfRD3D79fKktnisC7XOuwbHG3TQo0_j4_mElH7xj_7IyAbmCUHDd-eRa482wOYXBB01DGnad901qaHU\""
      },
      "we4Z3weGpJUUEwgeWmkIQsRJBTCfaB1s75LfgudSC1I": {
        "alg": "rsa-pss-sha512",
        "commitment-device": "httpsig@1.0",
        "committer": "Tbun4iRRQW93gUiSAmTmZJ2PGI-_yYaXsX69ETgzSRE",
        "signature": "http-sig-bba7e22451416f77=:kqtatfcnCrmmJiEc1GT3hKKU6tUVRy34hDN6z1vN5UHCwNZ4f+tu9FafZ/mOx8loq11DgdV8S7Xvxk5LzytMAtV1SmAArEQ1VbMJkS+bIiNksd4qmU13JkQjz+a90FYVUDKn0uU+cRUDx+7wVh4Rco27WEBj/E5yVreKcmG0fpORHi4DMV219cb0zUAdDEqY/FdvdlC+Xr91xQzDwcwS8goeHS879P6FLRo5BubLWx/bbJXoS2BEGowkWAORP1jooWe+oNIcWbMWA1CUpPTih2VXbUQcdRto5DDjwXw90nxD3UVPLegweGOZrASuccG9oFg/++mJeFFz3W6cy3Eg84WmrMjfbzsUb6WVcKti83YZYTo7onUDwad2wVUe2WDCuMLm8TFhwP8zwU/MHSfcahRnZasnroPwxvYRjFNWa5USyqGaZ7uM/wqArGKL/2dlh3bIphEmTjtYBc9q2tlosNgPngwnPu6qbEFQpcDEzsODQQBYrnP9HA6HIqsC+dWPINw0xueFqnhu5bv3Y+Y17vFc4zOraBpVCZUEMKEDPgNHe2vMjFVfgIbKp9I9Xu+8vYd8sL+2p+lgkrXVjNCS07XYjVHj855GKCmIZHs9fZa0dfLghOdDfnbexzCpSDIVnfstZx5yniXh00Rk3g2wUAWB7Sq7nRG86BiOQP+EcHY=:",
        "signature-input": "http-sig-bba7e22451416f77=(\"key\" \"list\" \"bool\" \"ao-types\" \"content-type\" \"content-digest\" \"content-length\");alg=\"rsa-pss-sha512\";keyid=\"o1kvTqZQ0wbS_WkdwX70TFCk7UF76ldnJ85l8iRV7t6mSlzkXBYCecb-8RXsNEQQmO0KergtHOvhuBJmB6YXaYe_UftI_gendojfIa6jlTgw-qmH6g4_oErI8djDRbQSm-5nCfGVRuYxsNZLYDeqw4gFb9K3b1h7tuMoLd6-d5pkaLfTMUNcvs2OqpkLo0i_av746FieaURdWozwFqO0APtdA7pLHDqQZDMNdTmsUBJFszL6SOa1bKe5cUWnrq4uaW4NAN3JAQniILKGsKZENeKtfXwiKVaFJtriWWsbhOaNT0JLcuBAwXQAP59RXzcr8bRY6XFn8zBmEmZBGszOD9c9ssDENRFDa5uyVhk8XgIgQjErAWYd9T6edrYcIp3R78jhNK_nLiIBBz8_Oz3bLjL5i_aiV2gpfIbd44DCHihuuxSWRAPJxhEy9TS0_QbVOIWhcDTIeEJE3aRPTwSTMt1_Fec7i9HJWN0mvMbAAJw8k6HxjA3pFZiCowZJw7FBwMAeYgEwIeB82f-S2-PtFLwR9i0tExo36hEBHqaS4Y-O3NGgQ8mKnhT7Z1EfxEbA2BpR9oL8rJFEnPIrHHu7B88OHDDfnfRD3D79fKktnisC7XOuwbHG3TQo0_j4_mElH7xj_7IyAbmCUHDd-eRa482wOYXBB01DGnad901qaHU\""
      }
    },
    "content-length": "253",
    "content-type": "multipart/form-data; boundary=\"eyja4UA4reu5SLEKVqY67NG8Q-jMdqEFmleD-hhSJKM\"",
    "key": "abc",
    "list": [ 1, 2, 3 ],
    "map": {
      "abc": "123"
    },
    "method": "POST",
    "path": "forward"
  },
  "opts": {
    "mode": "debug",
    "store": {
      "prefix": "cache-mainnet",
      "store-module": "hb_store_fs"
    },
    "hb_config_location": "config.flat",
    ...
  }
}

```

## TABM (Type Annotated Binary Message)

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

The `msg1` object type is called `TABM (Type Annotated Binary Message)` and this is what HyperBEAM internally uses to circumvent the limitation that we can only pass flattened strings in the HTTP headers.

## Encoding / Decoding Steps

So a client encodes a message with `httpsig@1.0`, then `structured@1.0` into `TABM`, then signs it with `http-message-signatures`, then sends it to a HyperBEAM node.

1. `DATA = { path: "/~wao@1.0/forward", key: "abc" }`
2. `TABM = encode_by_structured(DATA)`
3. `HTTP_MSG = encode_by_httpsig(TABM) = Headers + Body`
4. `SIGNED_HTTP_MSG = sign(HTTP_MSG)`
5. `send(SIGNED_HTTP_MSG)`

The node receives it, verifies the signature, then decodes it first with `structured@1.0` (msg1), then with `httpsig@1.0` (msg2).

1. `Msg0 + Commitments = dev_codec_httpsig:verify(SIGNED_HTTP_MSG)`
2. `Msg1(TABM) = dev_codec_httpsig:to(Msg0) + Device`
3. `Msg2 = dev_codec_structured:to(Msg1) + Commitments + Path`

`flat@1.0` is used to flatten and unflatten nested object paths in the `httpsig@1.0` device since HTTP headers and body can only handle string values, not nested structures.

## Running Tests

You can find the working test file for this chapter here:

- [custom-devices-codecs.test.js](https://github.com/weavedb/wao/blob/master/dhfs-tutorial-app/test/custom-devices-codecs.test.js)

Run tests:

```bash [Terminal]
yarn test test/custom-devices-codecs.test.js
```

## Reference

##### General

- [Extending HyperBEAM with Devices](https://hyperbeam.ar.io/build/devices/building-devices.html)

##### Device Docs

- [Device: ~json@1.0](https://hyperbeam.ar.io/build/devices/json-at-1-0.html)

##### Device API

- [dev_codec_json.erl](https://hyperbeam.ar.io/build/devices/source-code/dev_codec_json.html)

##### WAO API

- [HyperBEAM Class API](/api/hyperbeam)
- [HB Class API](/api/hb)
