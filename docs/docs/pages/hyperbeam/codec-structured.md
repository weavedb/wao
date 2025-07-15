# Structured Codec

`structured@1.0` turns complex objects into strings with extended `ao-types` according to the HTTP Structured Field Values ([RFC-9651](https://datatracker.ietf.org/doc/rfc9651/)) specification.

`ao-types` has the following types:

- `integer` : `123`
- `float` : `3.14`
- `binary` : `"abc"` | `Buffer.from([1,2,3])`
- `atom` : `true` | `false` | `null` | `Symbol("abc")`
- `list` : `[1, 2, 3]`
- `empty-binary` : `""` | `Buffer.from([])`
- `empty-list` : `[]`
- `empty-message` : `{}`


```erlang
structured_to(Msg1, Msg2, Opts) ->
    Body = maps:get(<<"body">>, Msg1),
    TABM = dev_codec_json:from(Body),
    FLAT = dev_codec_structured:to(TABM),
    JSON = dev_codec_json:to(FLAT),
    {ok, JSON}.

structured_from(Msg1, Msg2, Opts) ->
    Body = maps:get(<<"body">>, Msg1),
    TABM = dev_codec_json:from(Body),
    FLAT = dev_codec_structured:from(TABM),
    JSON = dev_codec_json:to(FLAT),
    {ok, JSON}.
```

Encode JSON with `dev_codec_structured:from`.

```js
const cases = [
  { list: [1, true, "abc"] },
  { nested_list: [1, [2, 3]] },
  { a: { b: [1, 2, 3] } },
  { a: [1, 2], b: [3, 4] },
  { empty_list: [], empty_binary: "", empty_message: {} },
]
for (const v of cases) {
  const { out } = await hb.post({
    path: "/~wao@1.0/structured_from",
    body: JSON.stringify(v),
  })
  console.log(JSON.parse(out))
}
```

- `{ list: [1, true, "abc"] }`

```js
{
  'ao-types': 'list="list"',
  list: '"(ao-type-integer) 1", "(ao-type-atom) \\"true\\"", "abc"'
}
```

- `{ nested_list: [1, [2, 3]] }`

```js
{
  'ao-types': 'nested_list="list"',
  nested_list: '"(ao-type-integer) 1", "(ao-type-list) \\"(ao-type-integer) 2\\", \\"(ao-type-integer) 3\\""'
}
```

- `{ a: { b: [1, 2, 3] } }`

```js
{
  a: {
    'ao-types': 'b="list"',
    b: '"(ao-type-integer) 1", "(ao-type-integer) 2", "(ao-type-integer) 3"'
  }
}
```

- `{ a: [1, 2], b: [3, 4] }`

```js
{
  a: '"(ao-type-integer) 1", "(ao-type-integer) 2"',
  'ao-types': 'a="list", b="list"',
  b: '"(ao-type-integer) 3", "(ao-type-integer) 4"'
}
```

- `{ empty_list: [], empty_binary: "", empty_message: {} }`

```js
{
  'ao-types': 'empty_binary="empty-binary", empty_list="empty-list", empty_message="empty-message"'
}
```

You can specify `ao-types` of the values at the same level, annotate keys with `(ao-type-[type])`, and join multiple entries with `, `.

Let's decode the encoded values.

```js
const cases = [
  {
    'ao-types': 'list="list"',
    list: '"(ao-type-integer) 1", "(ao-type-atom) \\"true\\"", "abc"'
  },
  {
    'ao-types': 'nested_list="list"',
    nested_list: '"(ao-type-integer) 1", "(ao-type-list) \\"(ao-type-integer) 2\\", \\"(ao-type-integer) 3\\""'
  },
  {
    a: {
      'ao-types': 'b="list"',
      b: '"(ao-type-integer) 1", "(ao-type-integer) 2", "(ao-type-integer) 3"'
    }
  },
  {
    a: '"(ao-type-integer) 1", "(ao-type-integer) 2"',
    'ao-types': 'a="list", b="list"',
    b: '"(ao-type-integer) 3", "(ao-type-integer) 4"'
  },
  {
    'ao-types': 'empty_binary="empty-binary", empty_list="empty-list", empty_message="empty-message"'
  }
]
for (const v of cases) {
  const { out } = await hb.post({
    path: "/~wao@1.0/structured_to",
    body: JSON.stringify(v),
  })
  console.log(JSON.parse(out))
}
```
