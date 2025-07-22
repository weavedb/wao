# Httpsig Codec

`httpsig@1.0` turns structured encoded objects into HTTP signature-ready objects. It flattens map structures into strings with the `flat@1.0` device, and puts complex structures into the multipart `body` format.

Create custom methods to expose `dev_codec_httpsig:from` and `dev_codec_httpsig:to`.

```erlang [/HyperBEAM/src/dev_mydev.erl]
-export([ httpsig_to/3, httpsig_from/3 ]).

httpsig_to(Msg1, Msg2, Opts) ->
    Body = maps:get(<<"body">>, Msg1),
    TABM = dev_codec_json:from(Body),
    HTTPSIG = dev_codec_httpsig:to(TABM),
    JSON = dev_codec_json:to(HTTPSIG),
    {ok, JSON}.
 
httpsig_from(Msg1, Msg2, Opts) ->
    Body = maps:get(<<"body">>, Msg1),
    HTTPSIG = dev_codec_json:from(Body),
    TABM = dev_codec_httpsig:from(HTPSIHG),
    JSON = dev_codec_json:to(TABM),
    {ok, JSON}.
```

Let's convert one complex object.

- `{ a: { b: [1, 2, 3]}, c: { d: [3.14, true, "str"] } }`

The structured encoded representation is the following.

```js [/test/codec-httpsig.test.js]
const cases = [{
  a: {
    "ao-types": 'b="list"',
    b: '"(ao-type-integer) 1", "(ao-type-integer) 2", "(ao-type-integer) 3"',
  },
  c: {
    "ao-types": 'd="list"',
    d: '"(ao-type-float) 3.14", "(ao-type-atom) \\"true\\"", "str"',
  },
}]

for (const v of cases) {
  const { body } = await hb.post({
    path: "/~mydev@1.0/httpsig_to",
    body: JSON.stringify(v),
  })
  console.log(JSON.parse(body))
}
```
We get an httpsig-encoded value ready to be signed.

```js
{
  body: '--rqDK_isKBhMozuATy4K6NFgdADGNHedXoUEDN10AANo\r\n' +
    'ao-types: b="list"\r\n' +
    'b: "(ao-type-integer) 1", "(ao-type-integer) 2", "(ao-type-integer) 3"\r\n' +
    'content-disposition: form-data;name="a"\r\n' +
    '--rqDK_isKBhMozuATy4K6NFgdADGNHedXoUEDN10AANo\r\n' +
    'ao-types: d="list"\r\n' +
    'content-disposition: form-data;name="c"\r\n' +
    'd: "(ao-type-float) 3.14", "(ao-type-atom) \\"true\\"", "str"\r\n' +
    '--rqDK_isKBhMozuATy4K6NFgdADGNHedXoUEDN10AANo--',
  'body-keys': '"a", "c"',
  'content-digest': 'sha-256=:mv08FUN7TpjmiHhagrxwqgjS7kQ/HY2+If2hIUq/y54=:',
  'content-type': 'multipart/form-data; boundary="rqDK_isKBhMozuATy4K6NFgdADGNHedXoUEDN10AANo"'
}
```

The encoding gives you 3 pieces of metadata. Fields other than `body` go into the HTTP headers.

- `content-digest` : the sha256 hash of `body` content, only required if `body` exists
- `content-type` : `multipart/form-data` with `boundary`
- `body-keys` : allocated key of each `body` part

So you can split the body by the boundary of `rqDK_isKBhMozuATy4K6NFgdADGNHedXoUEDN10AANo`.

- `content-disposition: form-data;` : tells which path the part falls into  
  `name` could be a flattened path like `a/b/c`

```js
const parts = {
  a: 'ao-types: b="list"\r\n' +
	 'b: "(ao-type-integer) 1", "(ao-type-integer) 2", "(ao-type-integer) 3"\r\n' +
     'content-disposition: form-data;name="a"\r\n',
  c: 'ao-types: d="list"\r\n' +
     'content-disposition: form-data;name="c"\r\n' +
     'd: "(ao-type-float) 3.14", "(ao-type-atom) \\"true\\"", "str"\r\n`
}
```

You can decode the encoded value with `dev_codec_structured:to`.

```js [/test/codec-httpsig.test.js]
const cases = [
  {
    body: '--rqDK_isKBhMozuATy4K6NFgdADGNHedXoUEDN10AANo\r\n' +
      'ao-types: b="list"\r\n' +
      'b: "(ao-type-integer) 1", "(ao-type-integer) 2", "(ao-type-integer) 3"\r\n' +
      'content-disposition: form-data;name="a"\r\n' +
      '--rqDK_isKBhMozuATy4K6NFgdADGNHedXoUEDN10AANo\r\n' +
      'ao-types: d="list"\r\n' +
      'content-disposition: form-data;name="c"\r\n' +
      'd: "(ao-type-float) 3.14", "(ao-type-atom) \\"true\\"", "str"\r\n' +
      '--rqDK_isKBhMozuATy4K6NFgdADGNHedXoUEDN10AANo--',
    'body-keys': '"a", "c"',
    'content-digest': 'sha-256=:mv08FUN7TpjmiHhagrxwqgjS7kQ/HY2+If2hIUq/y54=:',
    'content-type': 'multipart/form-data; boundary="rqDK_isKBhMozuATy4K6NFgdADGNHedXoUEDN10AANo"'
  }
]
for (const v of cases) {
  const { body } = await hb.post({
    path: "/~mydev@1.0/httpsig_from",
    body: JSON.stringify(v),
  })
  console.log(JSON.parse(body))
}
```

Another example reveals a couple of special fields.

- `{ data: "abc", "Tbun4iRRQW93gUiSAmTmZJ2PGI-_yYaXsX69ETgzSRE": 123 }`

The encoded value is:

```js
{
  'ao-ids': 'Tbun4iRRQW93gUiSAmTmZJ2PGI-_yYaXsX69ETgzSRE="123"',
  'ao-types': '%54bun4i%52%52%51%5793g%55i%53%41m%54m%5a%4a2%50%47%49-_y%59a%58s%5869%45%54gz%53%52%45="integer"',
  body: 'abc',
  'content-digest': 'sha-256=:ungWv48Bz+pBQUDeXa4iI7ADYaOWF3qctBD/YfIAFa0=:',
  'inline-body-key': 'data'
}
```
- `ao-ids` : all keys get lower-cased during the encoding, but Arweave addresses are kept case-sensitive
- `inline-body-key` : the entire `body` will become the value of the specified key

## Encoding / Decoding Pipeline

You can validate the encoding-decoding of any value with the following pipeline.

```js [/test/codec-httpsig.test.js]
const cases = [
  { list: [1, true, "abc"] },
  { nested_list: [1, [2, 3]] },
  { a: { b: [1, 2, 3] } },
  { a: [1, 2], b: [3, 4] },
  { empty_list: [], empty_binary: "", empty_message: {} },
  { data: "abc", [hb.addr]: 123 },
  { list: [1, 2, 3], map: { a: { b: { c: 4 } } } },
]
for(const json of cases){
  const res = await hb.post({
    path: "/~mydev@1.0/structured_from",
    body: JSON.stringify(json),
  })
  const structured = JSON.parse(res.body)
  console.log(structured)
  const res2 = await hb.post({
    path: "/~mydev@1.0/httpsig_to",
    body: JSON.stringify(structured),
  })
  const encoded = JSON.parse(res2.body)
  console.log(encoded)
  const res3 = await hb.post({
    path: "/~mydev@1.0/httpsig_from",
    body: JSON.stringify(encoded),
  })
  
  // omit: body-keys, content-type, inline-body-key
  const {
    "body-keys": _,
    "content-type": __,
	"inline-body-key": ___,
    ...decoded
  } = JSON.parse(res3.body)
  console.log(decoded)
  const res4 = await hb.post({
    path: "/~mydev@1.0/structured_to",
    body: JSON.stringify(decoded),
  })
  const json2 = JSON.parse(res4.body)
  assert.deepEqual(json,json2)
}
````

## Running Tests

You can find the working test file for this chapter here:

- [codec-httpsig.test.js](https://github.com/weavedb/wao/blob/master/dhfs-tutorial-app/test/codec-httpsig.test.js)

Run tests:

```bash [Terminal]
yarn test test/codec-httpsig.test.js
```

## References

##### Device API

- [dev_codec_httpsig.erl](https://hyperbeam.ar.io/build/devices/source-code/dev_codec_httpsig.html)

##### WAO API

- [HyperBEAM Class API](/api/hyperbeam)
- [HB Class API](/api/hb)
