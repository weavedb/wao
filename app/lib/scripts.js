const src_data_js = `const src_data = \`local count = 0

Handlers.add("Inc", "Inc", function (msg)
  count = count + 1
  msg.reply({ Data = "Incremented!" })
end)

Handlers.add("Get", "Get", function (msg)
  msg.reply({ Data = tostring(count) })
end)

Handlers.add("Hello", "Hello", function (msg)
  msg.reply({ Data = "Hello, World!" })
end)\`

describe("Counter", ()=>{
  it("should increment", async ({ ao })=> {
    const { p } = await ao.deploy({src_data})
    assert.equal(await p.d("Hello", false), "Hello, World!")
    assert.equal(await p.d("Get"), "0")
    assert.equal(await p.m("Inc", false), "Incremented!")
    assert.equal(await p.d("Get"), "1")
  })
})

describe("WAO", ()=>{
  it("should run", async ({ ao, p, src })=> {
    // write your test
  })
})`

const src_data_lua = `local count = 0

Handlers.add("Inc", "Inc", function (msg)
  count = count + 1
  msg.reply({ Data = "Incremented!" })
end)

Handlers.add("Get", "Get", function (msg)
  msg.reply({ Data = tostring(count) })
end)

Handlers.add("Hello", "Hello", function (msg)
  msg.reply({ Data = "Hello, World!" })
end)`

export { src_data_js, src_data_lua }
