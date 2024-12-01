import { expect } from "chai"
import { AO, connect, acc } from "../src/test.js"

const src_data = `
Handlers.add( "Hello", "Hello", function (msg)
    msg.reply({ Data = "Hello, World!" })
  end
)
`

describe("WAO", function () {
  this.timeout(0)
  describe("Aoconnect", function () {
    let message, dryrun, spawn, signer, modules, scheduler

    before(async () => {
      ;({
        scheduler,
        accounts: [{ signer }],
        modules,
        spawn,
        message,
        dryrun,
      } = connect())
    })

    it("should spawn a process send messages", async () => {
      const pid = await spawn({ signer, scheduler, module: modules.aos_2_0_1 })
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
    before(async () => (ao = await new AO().init(acc[0])))
    it("should spawn a process send messages", async () => {
      const { p } = await ao.deploy({ src_data })
      expect(await p.d("Hello")).to.eql("Hello, World!")
    })
  })
})
