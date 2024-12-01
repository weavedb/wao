import { expect } from "chai"
import { AO, connect, acc } from "../src/test.js"
import { setup } from "../src/helpers.js"
import { tags } from "../src/utils.js"

const {
  scheduler,
  mu,
  accounts: [{ signer }],
  modules,
  getProcesses,
  spawn,
  message,
  dryrun,
} = connect()

const src_data = `
Handlers.add("Hello", "Hello", function (msg)
  msg.reply({ Data = "Hello, World!" })
end)
`

const src_data2 = `
Handlers.add("Hello2", "Hello2", function (msg)
  local name = Send({ Target = ao.id, Action = "Reply" }).receive().Data
  msg.reply({ Data = "Hello, " .. name .. "!" })
end)

Handlers.add("Reply", "Reply", function (msg)
  msg.reply({ Data = "Japan" })
end)
`

const src_data3 = `
Handlers.add("Hello3", "Hello3", function (msg)
   Spawn(msg.module, { Data = msg.Data, ["On-Boot"] = "Data" })
end)
`

describe("WAO", function () {
  this.timeout(0)
  describe("Aoconnect", function () {
    it("should spawn a process send messages", async () => {
      const pid = await spawn({ signer, scheduler, module: modules.aos2_0_1 })
      await message({
        process: pid,
        tags: [{ name: "Action", value: "Eval" }],
        data: src_data,
        signer,
      })
      const res = await dryrun({
        process: pid,
        tags: [{ name: "Action", value: "Hello" }],
        signer,
      })
      expect(res.Messages[0].Data).to.eql("Hello, World!")
    })
  })

  describe("SDK", function () {
    let ao
    beforeEach(async () => (ao = await new AO().init(acc[0])))

    it("should spawn a process send messages", async () => {
      const { p } = await ao.deploy({ src_data })
      expect(await p.d("Hello")).to.eql("Hello, World!")
    })

    it("should spawn a process with On-Boot tag", async () => {
      const { p, pid } = await ao.deploy({ boot: true, src_data })
      expect(await p.d("Hello")).to.eql("Hello, World!")
      const { p: p2 } = await ao.deploy({ boot: pid })
      expect(await p2.d("Hello")).to.eql("Hello, World!")
    })

    it("should spawn a message from a handler with receive", async () => {
      const { p, pid } = await ao.deploy({ boot: true, src_data: src_data2 })
      expect(await p.m("Hello2")).to.eql("Hello, Japan!")
    })

    it("should spawn a process from a handler", async () => {
      const { p, pid } = await ao.deploy({ boot: true, src_data: src_data3 })
      await p.m(
        "Hello3",
        { module: modules.aos2_0_1, auth: mu.addr },
        { data: src_data },
      )
      const prs = getProcesses()
      let p2 = null
      for (let k in prs) {
        if (tags(prs[k].opt.tags)["From-Process"] === pid) {
          p2 = ao.p(k)
        }
      }
      expect(p2).to.not.eql(null)
    })
  })
})
