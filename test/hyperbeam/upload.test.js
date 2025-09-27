import { bundleAndSignData, createData } from "@dha-team/arbundles"
import { httpsig_from, structured_to } from "hbsig"
import { ArweaveSigner } from "@ar.io/sdk"
import assert from "assert"
import { after, describe, it, before, beforeEach } from "node:test"
import HyperBEAM from "../../src/hyperbeam.js"
import { wait } from "../../src/utils.js"
import { acc } from "../../src/test.js"
import gateway from "../../src/bundler.js"
import HB from "../../src/hb.js"

const toMsg = async req => {
  let msg = {}
  req?.headers?.forEach((v, k) => {
    msg[k] = v
  })
  if (req.body) msg.body = await req.text?.()
  return structured_to(httpsig_from(msg))
}
const toTags = fields => {
  let tags = []
  for (const k in fields) {
    tags.push({ name: k, value: fields[k] })
  }
  return tags
}

describe("Hyperbeam Device", function () {
  let hb, hbeam, gw
  before(async () => {
    const _hbeam = new HyperBEAM({
      reset: true,
      bundler_ans104: false,
      //bundler_httpsig: "http://localhost:4001",
    })
    gw = gateway({ jwk: _hbeam.jwk })
    hbeam = await _hbeam.ready()
  })
  beforeEach(async () => (hb = hbeam.hb))
  after(async () => {
    gw.close()
    hbeam.kill()
  })

  it("should test process #0", async () => {
    const signer = new ArweaveSigner(hbeam.jwk)
    const fields = {
      Type: "Process",
      device: "process@1.0",
      "execution-device": "wao@1.0",
      "Data-Protocol": "ao",
      Variant: "ao.N.1",
      "codec-device": "ans104@1.0",
      signingFormat: "ANS-104",
      "Scheduler-Location": hb.addr,
      Scheduler: hb.addr,
    }
    const di = createData("1984", signer, { tags: toTags(fields) })
    await di.sign(signer)
    const res = await fetch("http://localhost:10001/~process@1.0/schedule", {
      method: "POST",
      headers: {
        "Content-Type": "application/ans104",
        "codec-device": "ans104@1.0",
      },
      body: di.binary,
    })
    const msg = await toMsg(res)
    console.log()
    console.log("process", msg.process)
    console.log()
    assert.equal(res.status, 200)

    const fields2 = {
      Type: "Message",
      "Data-Protocol": "ao",
      Variant: "ao.N.1",
      signingFormat: "ANS-104",
    }
    const di2 = createData("1984", signer, { tags: toTags(fields2) })
    await di2.sign(signer)
    const res2 = await fetch(
      `http://localhost:10001/${msg.process}~process@1.0/push`,
      {
        method: "POST",
        path: `/${msg.process}~process@1.0/push`,
        headers: {
          "Content-Type": "application/ans104",
          "codec-device": "ans104@1.0",
        },
        body: di2.binary,
      }
    )
    const msg2 = await toMsg(res2)
    console.log()
    console.log(msg2)
    console.log()
    assert.equal(res2.status, 200)

    await wait(5000)
  })

  it.only("should test process #2", async () => {
    const hb2 = new HB({ jwk: hb.jwk, format: "ans104" })
    const { pid } = await hb2.spawn({
      Name: "turbo-test",
      "execution-device": "wao@1.0",
      "Data-Protocol": "ao",
      Variant: "ao.WDB.1",
    })
    await hb2.schedule({
      pid,
      tags: { "Data-Protocol": "ao", Variant: "ao.WDB.1" },
    })
    await hb2.schedule({
      pid,
      tags: { "Data-Protocol": "ao", Variant: "ao.WDB.1" },
    })
    await hb2.schedule({
      pid,
      tags: { "Data-Protocol": "ao", Variant: "ao.WDB.1" },
    })
    const { count } = await hb2.compute({ pid, slot: 2 })
    assert.equal(count, 3)
    await hb2.schedule({ pid })
    assert.equal((await hb2.compute({ pid, slot: 4 })).count, 5)
    const edges = (await hb2.messages({ pid })).edges
    for (const v of edges) {
      console.log(v.node.assignment)
      console.log(v.node.message)
    }
  })
})
