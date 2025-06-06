const node = new HyperBEAM({ port: 10001 })
await wait(5000)
const hb = await new HB({ url: "http://localhost:10001" }).init(jwk)

const { pid } = await hb.spawn()
const { slot } = await hb.schedule({ pid })
const res = await hb.compute({ pid, slot })

node.kill()
