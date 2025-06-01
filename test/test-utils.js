import { connect, createSigner } from "@permaweb/aoconnect"
import { send as _send, verify, createRequest } from "../src/signer.js"
import { wait, toAddr } from "../src/utils.js"
import { acc } from "../src/test.js"
import { run } from "../src/hyperbeam-server.js"
import { resolve } from "path"
import { readFileSync } from "fs"

const prepare = async (port = 10001, port2 = 4000, jwk) => {
  await wait(5000)
  const server = run(port2)
  jwk ??= acc[0].jwk
  const signer = createSigner(jwk, `http://localhost:${port}`)
  const request = createRequest({ signer })
  const send = async args => {
    const msg = await request(args)
    return await _send(msg)
  }
  return { server, request, send, addr: toAddr(jwk.n) }
}

const getJWK = file => {
  return JSON.parse(readFileSync(resolve(import.meta.dirname, file), "utf8"))
}

export { prepare, getJWK }
