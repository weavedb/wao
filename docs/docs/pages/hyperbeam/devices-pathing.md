# Devices and Pathing

The first thing to understand is that HyperBEAM consists of a collection of devices, and you can access specific methods on specific devices via URL endpoints.

For instance, the `meta@1.0` device lets you get and set node configurations with its `info` method.

Let's get the info with bare JS `fetch`. `http://localhost:10001` is the default hostname when running a node with WAO.

The minimum viable devices to run a HyperBEAM node are `flat@1.0`, `httpsig@1.0`, `structured@1.0`, `json@1.0`, and `meta@1.0`. These are codec devices except for `meta@1.0`, which handles node configuration and is the starting point of device resolution in the system. To gain deep understanding of AO Core and HyperBEAM, we should understand these codecs first. 

You can pass these device names to the WAO `HyperBEAM` class to preload only specific devices.

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
    // only preload 5 devices instead of the default 40 devices
    hbeam = await new HyperBEAM({
      devices: ["json", "structured", "httpsig", "flat", "meta"],
      cwd,
    }).ready()
  })
  beforeEach(async () => (hb = await new HB({}).init(jwk)))
  after(async () => hbeam.kill())

  it("should run a test case", async () => {
    /* write your tests here */
  })
})
```

## HyperBEAM Responses

From this point on, we'll assume you're writing any scripts within a test case code block.

You can execute a method on a device by `NODE_URL/~DEVICE/METHOD`. Don't forget the `~` before the device name. So to execute `info` on the `meta@1.0` device on a node running at `http://localhost:10001`, the URL will be `http://localhost:10001/~meta@1.0/info`.

```js
const res = await fetch("http://localhost:10001/~meta@1.0/info")
const headers = res.headers
const body = await res.text()
console.log(headers)
console.log(body)
```

This is the headers you get:

```js
Headers {
  host: 'localhost',
  debug_print_indent: '2',
  mode: '"debug"',
  load_remote_devices: '"false"',
  port: '10001',
  debug_committers: '"false"',
  commitment_device: 'httpsig@1.0',
  ans104_trust_gql: '"true"',
  'access-control-allow-methods': 'GET, POST, PUT, DELETE, OPTIONS',
  store_all_signed: '"true"',
  wasm_allow_aot: '"false"',
  debug_stack_depth: '40',
  relay_http_client: '"httpc"',
  debug_ids: '"true"',
  'transfer-encoding': 'chunked',
  debug_print_binary_max: '60',
  http_server: 'Tbun4iRRQW93gUiSAmTmZJ2PGI-_yYaXsX69ETgzSRE',
  signature: 'http-sig-bba7e22451416f77=:Cmv+kNvUX7qNHwpeW/bCZ98V3LO95fjio3lS1JnzsIN1nqSu+yUm5DR1+hOtNyYU1fuGirfN4zYXgK9CCTEj/Cbc+MBdK24DpCarng2LK+fbuPPc/QFR8posZDbYiOxuokzH/qpwSpcH5ctf9Ss0NbDTv27vKJkZnQHa1bRIk3Qh7GjXUxaXdRzTWbrxbOFno9CTEBH1GzCDmrpE3QFek4gwFopnREtZ0B6bQEAmClfvVo9XoQtIMix0h+Ba6PiczBPmGurHdT1Fy2aZlZt9v7yAUGa8+rvdlIwgBNPnG/1agZvSIeqUYTBr8Rb4D5ai4wHeCYrUhlp9rPS+SNZUnyNcck9KkpEocz9RrF7G8Donr7JVfi/NTT3OtQwYdtRFnzu56ggVUwrWlYSQHhVqzoLVYlpVPBtlTMhYeSahIwfbymB/9bRbybIFxWQr6QJWw/NBoV3OSiy2yC9bcE52A2OFbK1uxO092d/KEav7b9b/O5sh3KbsNkWZP7hjyM5G5urcyR6nholwNVVhqYxbQPzGJN1BvbJhqLjXIA2OReJwqylDjLnMKO1XuHVDPUJ5XnTBJWE8Xet3XdmAm98VITW2hiUfyOR6k/PZugPv9QM5E2rY1fIl5F8ZPjCX/RRU4i6azyxhcCvGaURxsmmmpFuNvq8tFt8ABWQgNajEChk=:',
  scheduling_mode: '"local_confirmation"',
  status: '200',
  'access-control-allow-origin': '*',
  http_connect_timeout: '5000',
  'body-keys': '"http_extra_opts", "preloaded_devices/1", "preloaded_devices/2", "preloaded_devices/3", "preloaded_devices/4", "preloaded_devices/5", "routes/1", "routes/1/node", "routes/2", "routes/2/nodes/1", "routes/2/nodes/1/opts", "routes/2/nodes/2", "routes/2/nodes/2/opts", "routes/3", "routes/3/node", "routes/3/node/opts", "stack_print_prefixes", "store"',
  await_inprogress: '"named"',
  'content-digest': 'sha-256=:4J6aEWuLwZg2wbsns7pEnKDAy0JfdLfU4SLen3z2fgQ=:',
  access_remote_cache_for_client: '"false"',
  server: 'Cowboy',
  'ao-types': 'access_remote_cache_for_client="atom", ans104_trust_gql="atom", await_inprogress="atom", cache_lookup_hueristics="atom", client_error_strategy="atom", compute_mode="atom", debug_committers="atom", debug_ids="atom", debug_metadata="atom", debug_print="atom", debug_print_binary_max="integer", debug_print_indent="integer", debug_print_map_line_threshold="integer", debug_print_trace="atom", debug_show_priv="atom", debug_stack_depth="integer", force_signed="atom", http_client="atom", http_connect_timeout="integer", http_keepalive="integer", http_request_send_timeout="integer", initialized="atom", load_remote_devices="atom", mode="atom", node_history="empty-list", only="atom", port="integer", preloaded_devices="list", process_now_from_cache="atom", process_workers="atom", relay_http_client="atom", routes="list", scheduler_location_ttl="integer", scheduling_mode="atom", short_trace_len="integer", snp_trusted="empty-list", stack_print_prefixes="list", status="integer", store_all_signed="atom", trusted_device_signers="empty-list", wasm_allow_aot="atom"',
  force_signed: '"true"',
  http_request_send_timeout: '60000',
  client_error_strategy: '"throw"',
  bundler_ans104: 'https://up.arweave.net:443',
  debug_print: '"false"',
  debug_show_priv: '"false"',
  address: 'Tbun4iRRQW93gUiSAmTmZJ2PGI-_yYaXsX69ETgzSRE',
  cache_lookup_hueristics: '"false"',
  only: '"local"',
  initialized: '"true"',
  process_workers: '"false"',
  hb_config_location: 'config.flat',
  'content-type': 'multipart/form-data; boundary="Z9Om0uN1Z81Q-Sp3G0Mptr4AewbZNf_drHF9PkmdB8A"',
  'signature-input': 'http-sig-bba7e22451416f77=("access_remote_cache_for_client" "address" "ans104_trust_gql" "ao-types" "await_inprogress" "bundler_ans104" "cache_lookup_hueristics" "client_error_strategy" "commitment_device" "compute_mode" "content-digest" "content-type" "debug_committers" "debug_ids" "debug_metadata" "debug_print" "debug_print_binary_max" "debug_print_indent" "debug_print_map_line_threshold" "debug_print_trace" "debug_show_priv" "debug_stack_depth" "force_signed" "gateway" "hb_config_location" "host" "http_client" "http_connect_timeout" "http_keepalive" "http_request_send_timeout" "http_server" "initialized" "load_remote_devices" "mode" "only" "port" "process_now_from_cache" "process_workers" "relay_http_client" "scheduler_location_ttl" "scheduling_mode" "short_trace_len" "@status" "store_all_signed" "wasm_allow_aot");alg="rsa-pss-sha512";keyid="o1kvTqZQ0wbS_WkdwX70TFCk7UF76ldnJ85l8iRV7t6mSlzkXBYCecb-8RXsNEQQmO0KergtHOvhuBJmB6YXaYe_UftI_gendojfIa6jlTgw-qmH6g4_oErI8djDRbQSm-5nCfGVRuYxsNZLYDeqw4gFb9K3b1h7tuMoLd6-d5pkaLfTMUNcvs2OqpkLo0i_av746FieaURdWozwFqO0APtdA7pLHDqQZDMNdTmsUBJFszL6SOa1bKe5cUWnrq4uaW4NAN3JAQniILKGsKZENeKtfXwiKVaFJtriWWsbhOaNT0JLcuBAwXQAP59RXzcr8bRY6XFn8zBmEmZBGszOD9c9ssDENRFDa5uyVhk8XgIgQjErAWYd9T6edrYcIp3R78jhNK_nLiIBBz8_Oz3bLjL5i_aiV2gpfIbd44DCHihuuxSWRAPJxhEy9TS0_QbVOIWhcDTIeEJE3aRPTwSTMt1_Fec7i9HJWN0mvMbAAJw8k6HxjA3pFZiCowZJw7FBwMAeYgEwIeB82f-S2-PtFLwR9i0tExo36hEBHqaS4Y-O3NGgQ8mKnhT7Z1EfxEbA2BpR9oL8rJFEnPIrHHu7B88OHDDfnfRD3D79fKktnisC7XOuwbHG3TQo0_j4_mElH7xj_7IyAbmCUHDd-eRa482wOYXBB01DGnad901qaHU";tag="bU-F-WCfOMjeKPG4yVd4BIZIvR-ZRDXLi6AW5Da5kDo/jNI0FLgi9Lz2UT_l1sK2TCPZMCIwFpPcOK3e3cqdRwo"',
  compute_mode: '"lazy"',
  scheduler_location_ttl: '604800000',
  date: 'Sat, 12 Jul 2025 08:15:38 GMT',
  debug_print_trace: '"short"',
  short_trace_len: '5',
  http_keepalive: '120000',
  http_client: '"gun"',
  debug_print_map_line_threshold: '30',
  process_now_from_cache: '"false"',
  gateway: 'https://arweave.net',
  debug_metadata: '"true"'
}
```
And this is the body you get:

```js
--Z9Om0uN1Z81Q-Sp3G0Mptr4AewbZNf_drHF9PkmdB8A
ao-types: cache_control="list", force_message="atom"
cache_control: "always"
content-disposition: form-data;name="http_extra_opts"
force_message: "true"
--Z9Om0uN1Z81Q-Sp3G0Mptr4AewbZNf_drHF9PkmdB8A
ao-types: module="atom"
content-disposition: form-data;name="preloaded_devices/1"
module: "dev_codec_json"
name: json@1.0
--Z9Om0uN1Z81Q-Sp3G0Mptr4AewbZNf_drHF9PkmdB8A
ao-types: module="atom"
content-disposition: form-data;name="preloaded_devices/2"
module: "dev_codec_structured"
name: structured@1.0
--Z9Om0uN1Z81Q-Sp3G0Mptr4AewbZNf_drHF9PkmdB8A
ao-types: module="atom"
content-disposition: form-data;name="preloaded_devices/3"
module: "dev_codec_httpsig"
name: httpsig@1.0
--Z9Om0uN1Z81Q-Sp3G0Mptr4AewbZNf_drHF9PkmdB8A
ao-types: module="atom"
content-disposition: form-data;name="preloaded_devices/4"
module: "dev_codec_flat"
name: flat@1.0
--Z9Om0uN1Z81Q-Sp3G0Mptr4AewbZNf_drHF9PkmdB8A
ao-types: module="atom"
content-disposition: form-data;name="preloaded_devices/5"
module: "dev_meta"
name: meta@1.0
--Z9Om0uN1Z81Q-Sp3G0Mptr4AewbZNf_drHF9PkmdB8A
content-disposition: form-data;name="routes/1"
template: /result/.*
--Z9Om0uN1Z81Q-Sp3G0Mptr4AewbZNf_drHF9PkmdB8A
content-disposition: form-data;name="routes/1/node"
prefix: http://localhost:6363
--Z9Om0uN1Z81Q-Sp3G0Mptr4AewbZNf_drHF9PkmdB8A
ao-types: nodes="list"
content-disposition: form-data;name="routes/2"
template: /graphql
--Z9Om0uN1Z81Q-Sp3G0Mptr4AewbZNf_drHF9PkmdB8A
content-disposition: form-data;name="routes/2/nodes/1"
prefix: https://arweave-search.goldsky.com
--Z9Om0uN1Z81Q-Sp3G0Mptr4AewbZNf_drHF9PkmdB8A
ao-types: http_client="atom", protocol="atom"
content-disposition: form-data;name="routes/2/nodes/1/opts"
http_client: "httpc"
protocol: "http2"
--Z9Om0uN1Z81Q-Sp3G0Mptr4AewbZNf_drHF9PkmdB8A
content-disposition: form-data;name="routes/2/nodes/2"
prefix: https://arweave.net
--Z9Om0uN1Z81Q-Sp3G0Mptr4AewbZNf_drHF9PkmdB8A
ao-types: http_client="atom", protocol="atom"
content-disposition: form-data;name="routes/2/nodes/2/opts"
http_client: "gun"
protocol: "http2"
--Z9Om0uN1Z81Q-Sp3G0Mptr4AewbZNf_drHF9PkmdB8A
content-disposition: form-data;name="routes/3"
template: /raw
--Z9Om0uN1Z81Q-Sp3G0Mptr4AewbZNf_drHF9PkmdB8A
content-disposition: form-data;name="routes/3/node"
prefix: https://arweave.net
--Z9Om0uN1Z81Q-Sp3G0Mptr4AewbZNf_drHF9PkmdB8A
ao-types: http_client="atom", protocol="atom"
content-disposition: form-data;name="routes/3/node/opts"
http_client: "gun"
protocol: "http2"
--Z9Om0uN1Z81Q-Sp3G0Mptr4AewbZNf_drHF9PkmdB8A
1: "(ao-type-integer) 104", "(ao-type-integer) 98"
2: "(ao-type-integer) 100", "(ao-type-integer) 101", "(ao-type-integer) 118"
3: "(ao-type-integer) 97", "(ao-type-integer) 114"
ao-types: 1="list", 2="list", 3="list"
content-disposition: form-data;name="stack_print_prefixes"
--Z9Om0uN1Z81Q-Sp3G0Mptr4AewbZNf_drHF9PkmdB8A
ao-types: store-module="atom"
content-disposition: form-data;name="store"
prefix: cache-mainnet
store-module: "hb_store_fs"
--Z9Om0uN1Z81Q-Sp3G0Mptr4AewbZNf_drHF9PkmdB8A--
```

Neither reading `headers` nor `body` alone gives you the complete picture of the response. These two components need to be combined to decode the message. By decoding responses from HyperBEAM nodes, you'll gain deep understanding of AO Core and HyperBEAM. So we'll decipher this together in the next chapters.

But for now, let's say WAO handles everything behind the scenes for you so you can get the final output with the following snippet. You only need to pass `path` without the node hostname.

```js
const { out } = await hb.get({ path: '/~meta@1.0/info' })
console.log(out)
```

This is the decoded output. HyperBEAM internally uses `TABM (Type Annotated Binary Message)` and it contains the Erlang `atom` type. So we convert it to `Symbol` when dealing with JS. Erlang doesn't have `boolean` and `null` types, so JS `true`, `false`, and `null` are all `atom` on the Erlang side.

```js
{
  access_remote_cache_for_client: false,
  address: 'Tbun4iRRQW93gUiSAmTmZJ2PGI-_yYaXsX69ETgzSRE',
  ans104_trust_gql: true,
  await_inprogress: Symbol(named),
  bundler_ans104: 'https://up.arweave.net:443',
  cache_lookup_hueristics: false,
  client_error_strategy: Symbol(throw),
  commitment_device: 'httpsig@1.0',
  compute_mode: Symbol(lazy),
  debug_committers: false,
  debug_ids: true,
  debug_metadata: true,
  debug_print: false,
  debug_print_binary_max: 60,
  debug_print_indent: 2,
  debug_print_map_line_threshold: 30,
  debug_print_trace: Symbol(short),
  debug_show_priv: false,
  debug_stack_depth: 40,
  force_signed: true,
  gateway: 'https://arweave.net',
  hb_config_location: 'config.flat',
  host: 'localhost',
  http_client: Symbol(gun),
  http_connect_timeout: 5000,
  http_extra_opts: { cache_control: [ 'always' ], force_message: true },
  http_keepalive: 120000,
  http_request_send_timeout: 60000,
  http_server: 'Tbun4iRRQW93gUiSAmTmZJ2PGI-_yYaXsX69ETgzSRE',
  initialized: true,
  load_remote_devices: false,
  mode: Symbol(debug),
  node_history: [],
  only: Symbol(local),
  port: 10001,
  preloaded_devices: [
    { module: Symbol(dev_codec_json), name: 'json@1.0' },
    { module: Symbol(dev_codec_structured), name: 'structured@1.0' },
    { module: Symbol(dev_codec_httpsig), name: 'httpsig@1.0' },
    { module: Symbol(dev_codec_flat), name: 'flat@1.0' },
    { module: Symbol(dev_meta), name: 'meta@1.0' }
  ],
  process_now_from_cache: false,
  process_workers: false,
  relay_http_client: Symbol(httpc),
  routes: [
    { template: '/result/.*', node: [Object] },
    { template: '/graphql', nodes: [Array] },
    { template: '/raw', node: [Object] }
  ],
  scheduler_location_ttl: 604800000,
  scheduling_mode: Symbol(local_confirmation),
  short_trace_len: 5,
  snp_trusted: [],
  stack_print_prefixes: [ [ 104, 98 ], [ 100, 101, 118 ], [ 97, 114 ] ],
  store: { prefix: 'cache-mainnet', 'store-module': Symbol(hb_store_fs) },
  store_all_signed: true,
  trusted_device_signers: [],
  wasm_allow_aot: false
}
```


## meta@1.0

You can change the node configuration with `POST` method on `/~meta@1.0/info`. You must be the node operator to sign the message. 

To send a POST message, you need to construct the encoded headers and body just like above, and sign it with the HTTP message signature scheme, which is a web standard specification ([RFC 9421](https://datatracker.ietf.org/doc/html/draft-ietf-httpbis-message-signatures)).

We'll dig into it later, but it's too complex for now, so WAO handles the complex message signing for you.

```js
// set a new config
await hb.post({ 
  path: '/~meta@1.0/info',
  test_config: "abc",
  test_config2: 123,
  test_config3: { abc: 123 }
})

// get info
const { out } = await hb.get({ path: '/~meta@1.0/info' })
assert.equal(out.test_config, "abc")
assert.equal(out.test_config2, 123)
assert.deepEqual(out.test_config3, { abc: 123 })
```

You can also get a specific key only:

```js
// getting the node operator wallet address
const { out: address } = await hb.get({ path: "/~meta@1.0/info/address" })
assert.equal(address, hb.addr)
```

When a certain path like `/~meta@1.0/info` returns an object, you can chain a key like `address` and access the value at `/~meta@1.0/info/address`.

Once you set `initialized` to `permanent`, you'll no longer be able to change any config.

```js
await hb.post({ path: "/~meta@1.0/info", initialized: "permanent" })
try{
  await hb.post({ path: "/~meta@1.0/info", test_config: "def" })
}catch(e){
  console.log(e)
}

const { out: test_config } = await hb.get({ 
  path: '/~meta@1.0/info/test_config'
})
assert.equal(test_config, "abc")
```
## json@1.0

You can also chain another method and device in the URL. For instance, `json@1.0` converts TABM to JSON with `serialize`.

```js
const { out: { body: json }  } = await hb.get({ 
  path: "/~meta@1.0/info/~json@1.0/serialize"
})
console.log(JSON.parse(json))
```

Chaining `/~json@1.0/serialize` comes in handy in certain cases, but sometimes the response comes back malformed with additional data attached due to complex message mutation and HTTP transport. `hb.get` and `hb.post` produce cleaner results, so you won't need `/~json@1.0/serialize` externally. It's still an essential codec device used internally on HyperBEAM.

## Basic Pathing Summary

In this chapter, you saw three basic URL schemes to access HyperBEAM:

### 1. Device and Method

/~device_name@version/method_name

- `/~meta@1.0/info`
- `/~meta@1.0/build`

### 2. Accessing Key

- `/~meta@1.0/info/address`
- `/~meta@1.0/info/preloaded_devices`

### 3. Chaining Another Device

- `/~meta@1.0/info/~json@1.0/serialize`


These patterns are the same with any other devices.
