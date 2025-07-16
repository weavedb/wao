import assert from "assert"
import { describe, it, before, after, beforeEach } from "node:test"
import { HyperBEAM } from "wao/test"

import { verify } from "wao/signer"
import { rsaid, hmacid } from "wao/utils"

const cwd = "../HyperBEAM"
const mydev = { name: "mydev@1.0", module: "dev_mydev" }
const devices = ["json", "structured", "httpsig", "flat", "meta", mydev]

describe("Custom Devices and Codecs", function () {
  let hbeam, hb
  before(async () => {
    hbeam = await new HyperBEAM({ cwd, devices, reset: true }).ready()
  })
  beforeEach(async () => (hb = hbeam.hb))
  after(async () => hbeam.kill())

  it("should sign a message", async () => {
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

    const signed = await hb.signEncoded(encoded)
    console.log(signed)

    const {
      valid, // should be true
      verified,
      signatureName,
      keyId,
      algorithm,
      decodedSignatureInput: {
        components,
        params: { alg, keyid, tag },
        raw,
      },
    } = await verify(signed)
    console.log(valid)

    const { body } = await hb.send(signed)
    const { msg1, msg2, opts } = JSON.parse(body)
    console.log(msg2)

    const rsa_id = rsaid(signed.headers)
    const hmac_id = hmacid(signed.headers)
    assert.deepEqual(
      Object.keys(msg2.commitments).sort(),
      [rsa_id, hmac_id].sort()
    )
  })

  it("should sign a message with hb.post", async () => {
    const { out } = await hb.post({
      path: "/~mydev@1.0/forward",
      key: "abc",
      list: [1, 2, 3],
      map: { abc: "123" },
      bool: true,
      body: "test_body",
    })
    console.log(JSON.parse(out))
  })

  it("should sign a message with hb.p", async () => {
    const out = await hb.p("/~mydev@1.0/forward", {
      key: "abc",
      list: [1, 2, 3],
      map: { abc: "123" },
      bool: true,
      body: "test_body",
    })
    console.log(JSON.parse(out))
  })
})
