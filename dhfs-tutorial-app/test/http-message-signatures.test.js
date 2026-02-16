import assert from "assert"
import { describe, it, before, after } from "node:test"
import { HyperBEAM } from "wao/test"
import { verify, rsaid, hmacid } from "hbsig"

describe("Custom Devices and Codecs", function () {
  let hbeam, hb
  before(async () => {
    hbeam = await new HyperBEAM({ reset: true }).ready()
    hb = hbeam.hb
  })
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

    // signEncoded expects { path, headers: {...}, body } structure
    const { body: encBody, path: _p, ...encHeaders } = encoded
    const signed = await hb.signEncoded({
      path: "/~mydev@1.0/forward",
      headers: encHeaders,
      body: encBody,
    })
    console.log(signed)

    const { valid } = await verify(signed)
    console.log("verified:", valid)
    assert.ok(valid, "Signature should be valid")

    const { body } = await hb.send(signed)
    const { msg2 } = JSON.parse(body)
    console.log(msg2)

    // The message should have at least one commitment
    const commitmentKeys = Object.keys(msg2.commitments || {})
    assert.ok(commitmentKeys.length > 0, "Should have at least one commitment")

    // RSA id should be among the commitments
    const rsa_id = rsaid(signed.headers)
    assert.ok(
      commitmentKeys.includes(rsa_id),
      "RSA commitment should be present"
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
