# HTTP Message Signatures

So far, we've been virtually testing the encoding process by exposing internal codec methods and sending JSON stringified messages. But in practice, these methods are not available and we need to sign the httpsig encoded message before sending it to a remote node. AO Core / HyperBEAM uses the web standard protocol of HTTP Message Signatures ([RFC-9421](https://datatracker.ietf.org/doc/html/draft-ietf-httpbis-message-signatures)).

Let's go back to the message tested in an earlier chapter and encode it with the pipeline from the previous chapter.

```js [/test/http-message-signatures.test.js]
const msg = {
  path: "/~mydev@1.0/forward",
  key: "abc",
  list: [1, 2, 3],
  map: { abc: "123" },
  bool: true,
  body: "test_body",
}
const res = await hb.post({
  path: "/~mydev@1.0/structured_from",
  body: JSON.stringify(msg),
})
const structured = JSON.parse(res.body)
console.log(structured)
const res2 = await hb.post({
  path: "/~mydev@1.0/httpsig_to",
  body: JSON.stringify(structured),
})
const encoded = JSON.parse(res2.body)
console.log(encoded)
```

This is the encoded message:

```js
{
  'ao-types': 'bool="atom", list="list"',
  body: '--x8jUsRrtoRCInzE6Nwgl_uoK-D2Oe-9i_RKeFskZk8c\r\n' +
    'content-disposition: inline\r\n' +
    '\r\n' +
    'test_body\r\n' +
    '--x8jUsRrtoRCInzE6Nwgl_uoK-D2Oe-9i_RKeFskZk8c\r\n' +
    'abc: 123\r\n' +
    'content-disposition: form-data;name="map"\r\n' +
    '--x8jUsRrtoRCInzE6Nwgl_uoK-D2Oe-9i_RKeFskZk8c--',
  'body-keys': '"body", "map"',
  bool: '"true"',
  'content-digest': 'sha-256=:yrY11i+3uYmjCLzOaOeIijrNL/dyPWHNHJTJwvsKvsc=:',
  'content-type': 'multipart/form-data; boundary="x8jUsRrtoRCInzE6Nwgl_uoK-D2Oe-9i_RKeFskZk8c"',
  key: 'abc',
  list: '"(ao-type-integer) 1", "(ao-type-integer) 2", "(ao-type-integer) 3"',
  path: '/~mydev@1.0/forward'
}
```
You can sign it with `hb.signEncoded`.

```js [/test/http-message-signatures.test.js]
const signed = await hb.signEncoded(encoded)
```

This is the signed message:

```js
{
  url: 'http://localhost:10001/~mydev@1.0/forward',
  method: 'POST',
  headers: {
    'ao-types': 'bool="atom", list="list"',
    'body-keys': '"body", "map"',
    bool: '"true"',
    'content-digest': 'sha-256=:yrY11i+3uYmjCLzOaOeIijrNL/dyPWHNHJTJwvsKvsc=:',
    'content-type': 'multipart/form-data; boundary="x8jUsRrtoRCInzE6Nwgl_uoK-D2Oe-9i_RKeFskZk8c"',
    key: 'abc',
    list: '"(ao-type-integer) 1", "(ao-type-integer) 2", "(ao-type-integer) 3"',
    path: '/~mydev@1.0/forward',
    'content-length': '236',
    signature: 'http-sig-bba7e22451416f77=:D8jgRQXCC0WIdTaKs4v9cjvmzZy13VdoFNzWeXxT8ErP8inUeLyXQy0V4aUaodbAueMSG0sk8Ut5EmMaeV6AX4mHO6YtZHWj7TL7x8h2Sa8dlvcYNHjauNlygs0URoeKIaE0eZoWM1LWD6F+qqLPL4mm2C4Ex5UttPkNb8kT4UI5AuxlGWmIgOBZZngWT4xoRsFIlanr2bz4Px4IeiTJgLAS2QwRsJTAJ60EumZ5xCBTU6Ir98W45PrHUf2MUjVxmcaVFZb3nrB4mC/3IjXIBlHmMkjD4lRr7/FuIr8JwuBDzlpA+/VUmfx+0L2qVp+F0rL0VhBiFB7KiRCpPqNDXO5bw2bei1cxoQHmcwhAxO+BIisJrBrbylHo+7yw4LLzAGebunMgsfVzl5DIxZRcjxNCF/4vSYllB+oybEgqTw9MP0Iwip59yCnzsnCrvo1m8PvQ9izJBVL7OrasjFTj5i+iOwOJt7YAZ8Yea93m7c2lMFJNocEme3Otj3oWU9MwPv+qyId5Q59A9uSzKD5wKMrUcICRYduw3NmGzcxKzPMHoC4lZQEDtzZvIvsjm/EVPQpIt/oPLMp3y5ZHEuPUoZx1xT9ahMWyg8eNw01+TQPsDK6QDE7eeRvWleBuaIMkBVH6mXSiR5J+vteSXox1xK0cV9evc10inWZd9LtPfxM=:',
    'signature-input': 'http-sig-bba7e22451416f77=("ao-types" "bool" "content-digest" "content-type" "key" "list" "content-length");alg="rsa-pss-sha512";keyid="o1kvTqZQ0wbS_WkdwX70TFCk7UF76ldnJ85l8iRV7t6mSlzkXBYCecb-8RXsNEQQmO0KergtHOvhuBJmB6YXaYe_UftI_gendojfIa6jlTgw-qmH6g4_oErI8djDRbQSm-5nCfGVRuYxsNZLYDeqw4gFb9K3b1h7tuMoLd6-d5pkaLfTMUNcvs2OqpkLo0i_av746FieaURdWozwFqO0APtdA7pLHDqQZDMNdTmsUBJFszL6SOa1bKe5cUWnrq4uaW4NAN3JAQniILKGsKZENeKtfXwiKVaFJtriWWsbhOaNT0JLcuBAwXQAP59RXzcr8bRY6XFn8zBmEmZBGszOD9c9ssDENRFDa5uyVhk8XgIgQjErAWYd9T6edrYcIp3R78jhNK_nLiIBBz8_Oz3bLjL5i_aiV2gpfIbd44DCHihuuxSWRAPJxhEy9TS0_QbVOIWhcDTIeEJE3aRPTwSTMt1_Fec7i9HJWN0mvMbAAJw8k6HxjA3pFZiCowZJw7FBwMAeYgEwIeB82f-S2-PtFLwR9i0tExo36hEBHqaS4Y-O3NGgQ8mKnhT7Z1EfxEbA2BpR9oL8rJFEnPIrHHu7B88OHDDfnfRD3D79fKktnisC7XOuwbHG3TQo0_j4_mElH7xj_7IyAbmCUHDd-eRa482wOYXBB01DGnad901qaHU"'
  },
  body: Blob { size: 236, type: '' }
}
```

The signing added `signature` and `signature-input` to `headers`. Let's break down `signature-input`.

You can also verify the signature with `hbsig`, which gives you the decomposition of the message if you ever need it.

```js [/test/http-message-signatures.test.js]
import { verify } from "hbsig"
const { 
  valid, // should be true
  verified,
  signatureName, 
  keyId, 
  algorithm, 
  decodedSignatureInput : { components, params: { alg, keyid, tag }, raw }
} = await verify(signed)
```

- signatureName : `http-sig-bba7e22451416f77`
- components : `("ao-types" "bool" "content-digest" "content-type" "key" "list" "content-length")`
- `alg` : `rsa-pss-sha512`
- `keyid` : `o1kvTqZQ0wbS_WkdwX70TFCk7UF76ldnJ85l8iRV7t6mSlzkXBYCecb...`  
  The signer public key = `jwk.n`
- `tag` : hashpath (missing unless messages are chained)

So the message is signed by the private key paired with `keyid` using the `rsa-pss-sha512` method, and the signed fields must exist in the HTTP `headers`.

Keys prefixed with `@` get special treatment. For example, HyperBEAM uses the `path` field from `headers` for `@path`. We're not including `@path` on purpose for now since there's a discrepancy between the `RFC-9421` spec and how HyperBEAM handles `path`, which produces signature verification errors. To be specific, the spec requires a full path with a leading `/`, but HyperBEAM strips off the slash and the device name from the `path`. We're currently investigating this issue. 

HyperBEAM also resolves the method and device to route the message to using the `path` field in the HTTP `headers`. So you should still send `path` even if you're not signing it. That's how the WAO signer handles it for now.

`@` prefixed keys with special behaviors are the following:

- `@path`
- `@query`
- `@query-param`
- `@scheme`
- `@request-target`
- `@authority`
- `@target-uri`
- `@method`

You can now send the signed message with `hb.send`. We're sending it to `/~mydev@1.0/forward`. 

```js [/test/http-message-signatures.test.js]
const { body } = await hb.send(signed)
const { msg1, msg2, opts } = JSON.parse(body)
console.log(msg2)
```

Let's see what we got back in `msg2`:

```js
{
body: 'test_body',
  bool: true,
  commitments: {
    Fg0kC92eBhUH3t894aH0IHMBiGIlLU80gt5Zjyip_bU: {
      alg: 'rsa-pss-sha512',
      'commitment-device': 'httpsig@1.0',
      committer: 'Tbun4iRRQW93gUiSAmTmZJ2PGI-_yYaXsX69ETgzSRE',
      signature: 'http-sig-bba7e22451416f77=:INJ6uxI4aLR0YB6raM1SWkeQvPKld+0sFVSBdj7P32+FgJ8QJyDHumYs4HK18JNs/CnG7zfn4pV9gMI2Ce9AklqbIflYCrjfIL00FCtqJ5Q4icKLe9/3XawNCtw3LNr9gFaXyBHzGdV0TaKYPemp88IFuYJ75Ins9IblOBIDXKSB4al+WySbnWBzy7uMyP2mt7L8jBv8J/q8N5YoWTIlebcfasjECXeDs+bRE9idcjn5zi74JdDgwQGNV7nujwwtJm4eh+WiUQsbOVsPWtAH/DDiiXOW5GTPimQBRcMvDir06YVasioIu0zarcdyPq5p3+pTJB4Q8AvrUSrqGqTu9RGlzzs6Hsbydy+9FsWo4vyZqQWxcMx1JPlpyl32+GH9SttEHG89OV1PKzo7sCmGUKhSIoIp05NplA/mOJxGnJfu3lYXkEf26U4qLmACk6fYABwtMypCnjckKTH/xFcZ81V2KQ1qBm1M3OnRysBWKNyWFiqwxQ4x0EfqQEl+xz3+Bb1JBR5f+DTSHHlbX2NyTimjR0LHryCmNG4jKrYeNYz5qt4BwBr/cE7DSVpnrFfk3f7SOai6/KkIdg+QrcHfI6U5I0K72Wg4FKqa0WUKMaO08OfV03QllFu1OlggrmiRYVw5n8CgDmq2SBiqyua/zBJeHzItaQ1nXM1ZGDBal2I=:',
      'signature-input': 'http-sig-bba7e22451416f77=("ao-types" "bool" "content-digest" "content-type" "key" "list" "content-length");alg="rsa-pss-sha512";keyid="o1kvTqZQ0wbS_WkdwX70TFCk7UF76ldnJ85l8iRV7t6mSlzkXBYCecb-8RXsNEQQmO0KergtHOvhuBJmB6YXaYe_UftI_gendojfIa6jlTgw-qmH6g4_oErI8djDRbQSm-5nCfGVRuYxsNZLYDeqw4gFb9K3b1h7tuMoLd6-d5pkaLfTMUNcvs2OqpkLo0i_av746FieaURdWozwFqO0APtdA7pLHDqQZDMNdTmsUBJFszL6SOa1bKe5cUWnrq4uaW4NAN3JAQniILKGsKZENeKtfXwiKVaFJtriWWsbhOaNT0JLcuBAwXQAP59RXzcr8bRY6XFn8zBmEmZBGszOD9c9ssDENRFDa5uyVhk8XgIgQjErAWYd9T6edrYcIp3R78jhNK_nLiIBBz8_Oz3bLjL5i_aiV2gpfIbd44DCHihuuxSWRAPJxhEy9TS0_QbVOIWhcDTIeEJE3aRPTwSTMt1_Fec7i9HJWN0mvMbAAJw8k6HxjA3pFZiCowZJw7FBwMAeYgEwIeB82f-S2-PtFLwR9i0tExo36hEBHqaS4Y-O3NGgQ8mKnhT7Z1EfxEbA2BpR9oL8rJFEnPIrHHu7B88OHDDfnfRD3D79fKktnisC7XOuwbHG3TQo0_j4_mElH7xj_7IyAbmCUHDd-eRa482wOYXBB01DGnad901qaHU"'
    },
    RivrHRmpfYVEIH45TxQdW99NR34IgEcStLm467eea38: {
      alg: 'hmac-sha256',
      'commitment-device': 'httpsig@1.0',
      signature: 'http-sig-bba7e22451416f77=:INJ6uxI4aLR0YB6raM1SWkeQvPKld+0sFVSBdj7P32+FgJ8QJyDHumYs4HK18JNs/CnG7zfn4pV9gMI2Ce9AklqbIflYCrjfIL00FCtqJ5Q4icKLe9/3XawNCtw3LNr9gFaXyBHzGdV0TaKYPemp88IFuYJ75Ins9IblOBIDXKSB4al+WySbnWBzy7uMyP2mt7L8jBv8J/q8N5YoWTIlebcfasjECXeDs+bRE9idcjn5zi74JdDgwQGNV7nujwwtJm4eh+WiUQsbOVsPWtAH/DDiiXOW5GTPimQBRcMvDir06YVasioIu0zarcdyPq5p3+pTJB4Q8AvrUSrqGqTu9RGlzzs6Hsbydy+9FsWo4vyZqQWxcMx1JPlpyl32+GH9SttEHG89OV1PKzo7sCmGUKhSIoIp05NplA/mOJxGnJfu3lYXkEf26U4qLmACk6fYABwtMypCnjckKTH/xFcZ81V2KQ1qBm1M3OnRysBWKNyWFiqwxQ4x0EfqQEl+xz3+Bb1JBR5f+DTSHHlbX2NyTimjR0LHryCmNG4jKrYeNYz5qt4BwBr/cE7DSVpnrFfk3f7SOai6/KkIdg+QrcHfI6U5I0K72Wg4FKqa0WUKMaO08OfV03QllFu1OlggrmiRYVw5n8CgDmq2SBiqyua/zBJeHzItaQ1nXM1ZGDBal2I=:',
      'signature-input': 'http-sig-bba7e22451416f77=("ao-types" "bool" "content-digest" "content-type" "key" "list" "content-length");alg="rsa-pss-sha512";keyid="o1kvTqZQ0wbS_WkdwX70TFCk7UF76ldnJ85l8iRV7t6mSlzkXBYCecb-8RXsNEQQmO0KergtHOvhuBJmB6YXaYe_UftI_gendojfIa6jlTgw-qmH6g4_oErI8djDRbQSm-5nCfGVRuYxsNZLYDeqw4gFb9K3b1h7tuMoLd6-d5pkaLfTMUNcvs2OqpkLo0i_av746FieaURdWozwFqO0APtdA7pLHDqQZDMNdTmsUBJFszL6SOa1bKe5cUWnrq4uaW4NAN3JAQniILKGsKZENeKtfXwiKVaFJtriWWsbhOaNT0JLcuBAwXQAP59RXzcr8bRY6XFn8zBmEmZBGszOD9c9ssDENRFDa5uyVhk8XgIgQjErAWYd9T6edrYcIp3R78jhNK_nLiIBBz8_Oz3bLjL5i_aiV2gpfIbd44DCHihuuxSWRAPJxhEy9TS0_QbVOIWhcDTIeEJE3aRPTwSTMt1_Fec7i9HJWN0mvMbAAJw8k6HxjA3pFZiCowZJw7FBwMAeYgEwIeB82f-S2-PtFLwR9i0tExo36hEBHqaS4Y-O3NGgQ8mKnhT7Z1EfxEbA2BpR9oL8rJFEnPIrHHu7B88OHDDfnfRD3D79fKktnisC7XOuwbHG3TQo0_j4_mElH7xj_7IyAbmCUHDd-eRa482wOYXBB01DGnad901qaHU"'
    }
  },
  'content-length': '236',
  'content-type': 'multipart/form-data; boundary="x8jUsRrtoRCInzE6Nwgl_uoK-D2Oe-9i_RKeFskZk8c"',
  key: 'abc',
  list: [ 1, 2, 3 ],
  map: { abc: '123' },
  method: 'POST',
  path: 'forward'
}
```

Your `signature` is in the `commitments`. And there are 2 entries with different `alg`.

- `Fg0kC92eBhUH3t894aH0IHMBiGIlLU80gt5Zjyip_bU` : `rsa-pss-sha512`
- `RivrHRmpfYVEIH45TxQdW99NR34IgEcStLm467eea38` : `hmac-sha256`

These commitment IDs are important for HyperBEAM to verify the message. The former is the sha-256 hash of the signature bytes, and the latter is the hmac-sha256 hash of the signed content with `ao` as the key. You can generate each ID with `rsaid` and `hmacid` methods from `wao/utils`. You can execute node internal scripts by manually creating commitments. We'll discuss this in a later chapter.

```js [/test/http-message-signatures.test.js]
import { rsaid, hmacid } from "wao/utils"
const rsa_id = rsaid(signed.headers)
const hmac_id = hmacid(signed.headers)
assert.deepEqual(
  Object.keys(msg2.commitments).sort(),
  [rsa_id, hmac_id].sort(),
)
```

FYI, you can use `hb_message:commit` to sign a message on HyperBEAM.

```erlang
% get operator wallet
Wallet = hb_opts:get(priv_wallet, not_found, Opts),
Signed = hb_message:commit( Msg, Wallet ),
```

## WAO HB SDK

In practice, you don't have to go through all these steps to construct signed messages. `hb.post` handles everything for you. You can get the decoded message in `out` instead of `headers` and `body`.

```js [/test/http-message-signatures.test.js]
const { out } = await hb.post({
  path: "/~mydev@1.0/forward",
  key: "abc",
  list: [1, 2, 3],
  map: { abc: "123" },
  bool: true,
  body: "test_body",
})
```

`get` and `g` work the same.

This is all you need to encode, sign, and send a message.

`p` is a shortcut method for `post` to get only the decoded message.

```js [/test/http-message-signatures.test.js]
const out = await hb.p("/~mydev@1.0/forward", {
  key: "abc",
  list: [1, 2, 3],
  map: { abc: "123" },
  bool: true,
  body: "test_body",
})
```

## Running Tests

You can find the working test file for this chapter here:

- [http-message-signatures.test.js](https://github.com/weavedb/wao/blob/master/dhfs-tutorial-app/test/http-message-signatures.test.js)

Run tests:

```bash [Terminal]
yarn test test/http-message-signatures.test.js
```

## References

##### Specs

- [HTTP Message Signatures [RFC-9421]](https://datatracker.ietf.org/doc/html/draft-ietf-httpbis-message-signatures)

##### WAO API

- [HyperBEAM Class API](/api/hyperbeam)
- [HB Class API](/api/hb)
- [HBSig API](/api/hbsig)
