# Flat Codec

`flat@1.0` is a simple codec device to flatten/unflatten object paths since HTTP headers and body cannot handle nested object structures. It's internally used by `httpsig@1.0` to resolve object paths.

Codec devices have `to` and `from` methods to encode and decode, but they are not exposed to external URLs. We can create custom methods for our custom device to expose them.

```erlang [/HyperBEAM/src/dev_mydev.erl]
-export([ flat_to/3, flat_from/3 ]).

flat_to(Msg1, Msg2, Opts) ->
    Body = maps:get(<<"body">>, Msg1),
    OBJ = dev_codec_json:from(Body),
    FLAT = dev_codec_flat:to(OBJ),
    JSON = dev_codec_json:to(FLAT),
    {ok, JSON}.

flat_from(Msg1, Msg2, Opts) ->
    Body = maps:get(<<"body">>, Msg1),
    OBJ = dev_codec_json:from(Body),
    FLAT = dev_codec_flat:from(OBJ),
    JSON = dev_codec_json:to(FLAT),
    {ok, JSON}.
```

One thing to note is that HTTP header keys cannot contain `/`, so if you ever need to send keys with `/` you need to push them into multipart body. We will handle this in the next chapter with `Httpsig Codec`. Flat codec only handles map structures. List structures are handled by `Structured Codec`. Also `Flat Codec` is an intermediary step used by `Httpsig Codec`, so values also have to be strings with `dev_codec_flat:to`.

```js [/test/codec-flat.test.js]
const cases = [
  { a: { b: "v" } },
  { a: "v", b: { c: "v2", d: "v3" } },
  { a: { b: { c: { d: "v" } } } },
]
for (const v of cases) {
  const { body } = await hb.post({
    path: "/~mydev@1.0/flat_to",
    body: JSON.stringify(v),
  })
  console.log(JSON.parse(body))
}
```

- `{ a: { b: "v" } }` -> `{ "a/b": "v" }`
- `{ a: "v", b: { c: "v2", d: "v3" } }` -> `{ a: "v", "b/c": "v2", "b/d": "v3" }`
- `{ a: { b: { c: { d: "v" } } } }` -> `{ "a/b/c/d": "v" }`

```js [/test/codec-flat.test.js]
const cases = [
  { "a/b": "v" },
  { a: "v", "b/c": "v2", "b/d": "v3" },
  { "a/b/c/d": "v" },
]
for (const v of cases) {
  const { body } = await hb.post({
    path: "/~mydev@1.0/flat_from",
    body: JSON.stringify(v),
  })
  console.log(JSON.parse(body))
}
```

## Running Tests

You can find the working test file for this chapter here:

- [codec-flat.test.js](https://github.com/weavedb/wao/blob/master/dhfs-tutorial-app/test/codec-flat.test.js)

Run tests:

```bash
yarn test test/codec-flat.test.js
```

Now we're ready to decode HyperBEAM.

## References

- [HyperBEAM Class API](/api/hyperbeam)
- [HB Class API](/api/hb)
