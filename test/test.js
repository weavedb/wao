import { expect } from "chai"
import { connect } from "../src/aoconnect.js"
import { AO } from "../src/index.js"

const src_data = `
Handlers.add( "Hello", "Hello", function (msg)
    msg.reply({ Data = "Hello, World!" })
  end
)
`
describe("WAO", function () {
  this.timeout(0)
  describe("Aoconnect", function () {
    let message, dryrun, spawn, signer

    before(async () => {
      ;({
        accounts: [{ signer }],
        spawn,
        message,
        dryrun,
      } = connect())
    })

    it("should spawn a process send messages", async () => {
      const pid = await spawn({ signer })
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
    before(async () => (ao = await new AO({ in_memory: true }).init()))
    it("should spawn a process send messages", async () => {
      const { p } = await ao.deploy({ src_data })
      expect(await p.d("Hello")).to.eql("Hello, World!")
    })
  })
})
