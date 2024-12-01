import { expect } from "chai"
import { AO, connect, acc } from "../src/test.js"
const {
  scheduler,
  accounts: [{ signer }],
  modules,
  spawn,
  message,
  dryrun,
} = connect()

const src_data = `
Handlers.add( "Hello", "Hello", function (msg)
    msg.reply({ Data = "Hello, World!" })
  end
)
`

describe("WAO", function () {
  this.timeout(0)
  describe("Aoconnect", function () {
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
    beforeEach(async () => (ao = await new AO().init(acc[0])))

    it("should spawn a process send messages", async () => {
      const { p } = await ao.deploy({ src_data })
      expect(await p.d("Hello")).to.eql("Hello, World!")
    })

    it("should spawn a process with On-Boot tag", async () => {
      const { p, pid } = await ao.deploy({ boot: true, src_data })
      expect(await p.d("Hello")).to.eql("Hello, World!")
      const { p: p2 } = await ao.deploy({ boot: pid, src_data })
      expect(await p2.d("Hello")).to.eql("Hello, World!")
    })
  })
})
