import { connect, createSigner } from "@permaweb/aoconnect"
import { signer } from "../../src/signer.js"
import { send as _send } from "../../src/send.js"
import { wait, toAddr } from "../../src/utils.js"
import { acc } from "../../src/test.js"
import { run } from "../../src/hyperbeam-server.js"
import { resolve } from "path"
import { readFileSync } from "fs"

const prepare = async (port = 10001, port2 = 4000, jwk) => {
  await wait(5000)
  const server = run(port2)
  jwk ??= acc[0].jwk
  const _signer = createSigner(jwk, `http://localhost:${port}`)
  const request = signer({ signer: _signer })
  const send = async args => {
    const msg = await request(args)
    return await _send(msg)
  }
  return { server, request, send, addr: toAddr(jwk.n) }
}

const getJWK = file => {
  return JSON.parse(readFileSync(resolve(import.meta.dirname, file), "utf8"))
}
const seed = num => {
  const array = new Uint8Array(num)
  return crypto.getRandomValues(array)
}

// Recursively transform values to match expected format
function mod(obj) {
  // Handle undefined - convert to string "undefined"
  if (obj === undefined) return "undefined"

  // Handle symbols - convert to their description or "symbol"
  if (typeof obj === "symbol") {
    return obj.description || "symbol"
  }

  // Handle arrays
  if (Array.isArray(obj)) {
    return obj.map(item => mod(item))
  }

  // Handle binary data (Buffer, Uint8Array, etc.)
  if (
    obj instanceof Uint8Array ||
    obj instanceof ArrayBuffer ||
    Buffer.isBuffer(obj)
  ) {
    // Convert to base64
    const buffer = Buffer.isBuffer(obj) ? obj : Buffer.from(obj)
    return buffer.toString("base64")
  }

  // Handle objects
  if (typeof obj === "object" && obj !== null) {
    const result = {}
    for (const [key, value] of Object.entries(obj)) {
      result[key] = mod(value)
    }
    return result
  }

  // Handle strings - check if it's already an atom-like string
  if (typeof obj === "string" && obj.match(/^[a-z_][a-zA-Z0-9_]*$/)) {
    // This looks like an atom value, keep as is
    return obj
  }

  // Return primitive values as-is (strings, numbers, booleans)
  return obj
}

export { prepare, getJWK, seed, mod }
