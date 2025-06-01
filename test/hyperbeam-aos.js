import assert from "assert"
import { connect, createSigner } from "@permaweb/aoconnect"
import { after, describe, it, before, beforeEach } from "node:test"
import { wait } from "../src/utils.js"
import { prepare, getJWK } from "./test-utils.js"
import { acc } from "../src/test.js"
import AR from "../src/ar.js"
import HB from "../src/hb.js"
import HyperBEAM from "../src/HyperBEAM"

import { resolve } from "path"
import { readFileSync } from "fs"
import { mergeLeft } from "ramda"
const randomBytes = num => {
  const array = new Uint8Array(num)
  return crypto.getRandomValues(array)
}

const src_data = `
local count = 0
Handlers.add("Add", "Add", function (msg)
  count = count + tonumber(msg.Plus)
end)

Handlers.add("Get", "Get", function (msg)
  msg.reply({ Data = tostring(count) })
end)
`

class HB2 {
  constructor({ port = 10001, pid } = {}) {
    this.port = port
    this.pid = pid
  }

  async init(jwk) {
    const res = await prepare(this.port, 4000, jwk)
    this.hbeam = res.hbeam
    this.server = res.server
    this.send = res.send
    this.addr = res.addr
    return this
  }

  async getAddr() {
    return (await this.send({ path: "/~meta@1.0/info/address", method: "GET" }))
      .body
  }

  async getImage() {
    const target = JSON.parse(
      (await this.send({ path: "/~wao@1.0/spawn", method: "POST" })).body
    ).process
    return (
      await this.send({ path: `/~cache@1.0/read`, method: "GET", target })
    ).headers.get("image")
  }

  async spawn() {
    const addr = await this.getAddr()
    this.scheduler ??= addr
    const res = await this.send({
      device: "process@1.0",
      path: "/schedule",
      scheduler: this.scheduler,
      "Data-Protocol": "ao",
      Variant: "ao.N.1",
      "scheduler-location": this.scheduler,
      Authority: this.scheduler,
      "random-seed": randomBytes(16).toString("hex"),
      Type: "Process",
      image: await this.getImage(),
      "scheduler-device": "scheduler@1.0",
      "execution-device": "stack@1.0",
      "device-stack": [
        "wasi@1.0",
        "json-iface@1.0",
        "wasm-64@1.0",
        "multipass@1.0",
      ],
      "output-prefix": "wasm",
      "patch-from": "/results/outbox",
      "stack-keys": ["init", "compute", "snapshot", "normalize"],
      passes: 2,
    })
    const pid = res.headers.get("process")
    this.pid ??= pid
    return pid
  }

  async message(action = "Eval", tags = {}, data) {
    let _tags = mergeLeft(tags, {
      device: "process@1.0",
      method: "POST",
      path: `/${this.pid}/schedule`,
      scheduler: this.scheduler,
      Type: "Message",
      Action: action,
      Target: this.pid,
    })
    if (data) _tags.data = data
    let res = await this.send(_tags)
    const slot = res.headers.get("slot")
    return { slot, outbox: await this.compute(this.pid, slot) }
  }
  async compute(pid, slot) {
    return await fetch(
      `http://localhost:${this.port}/${pid}/compute/results/outbox/serialize~json@1.0?slot=${slot}`
    ).then(r => r.json())
  }
}

describe("HyperBEAM Template", function () {
  let hb, hbeam
  before(async () => {
    hb = await new HB2().init(getJWK("../HyperBEAM/hyperbeam-key.json"))
  })
  after(async () => {
    hb.hbeam.kill("SIGKILL")
    hb.server.close()
  })
  it.only("should handle counter with Add and Get handlers", async () => {
    const pid = await hb.spawn()
    console.log(await hb.message("Eval", {}, src_data))
    console.log(await hb.message("Add", { Plus: "3" }))
    console.log(await hb.message("Get"))
  })
})
