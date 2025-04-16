const src_data_js = `describe("WAO", ()=>{
  it("should run", async ({ ao, p, src })=> {
    // write your test here
  })
})`

const src_data_lua = `
Handlers.add("Hello", "Hello", function (msg)
  msg.reply({ Data = "Hello, World!" })
end)`

export { src_data_js, src_data_lua }
