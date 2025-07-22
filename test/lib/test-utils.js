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
    const desc = obj.description || "symbol"
    // Special handling for symbols with descriptions that match special values
    if (desc === "null") return null
    if (desc === "undefined") return undefined
    if (desc === "true") return true
    if (desc === "false") return false
    return desc
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
    // Convert to empty string for empty buffers
    const buffer = Buffer.isBuffer(obj) ? obj : Buffer.from(obj)
    return buffer.length === 0 ? "" : buffer.toString("base64")
  }

  // Handle objects
  if (typeof obj === "object" && obj !== null) {
    const result = {}
    for (const [key, value] of Object.entries(obj)) {
      // Lowercase the key when creating the result object
      result[key.toLowerCase()] = mod(value)
    }
    return result
  }

  // Handle strings - check if it's already an atom-like string
  if (typeof obj === "string" && obj.match(/^[a-z_][a-zA-Z0-9_]*$/)) {
    // This looks like an atom value, keep as is
    return obj
  }

  // Return primitive values as-is (strings, numbers, booleans, null)
  return obj
}
// Recursively transform values to match expected format, removing undefined values
function mod2(obj) {
  // Handle undefined - return undefined to signal removal
  if (obj === undefined) return undefined

  // Handle symbols - convert to their description or "symbol"
  if (typeof obj === "symbol") {
    const desc = obj.description || "symbol"
    // Special handling for symbols with descriptions that match special values
    if (desc === "null") return null
    if (desc === "undefined") return undefined // This will be removed
    if (desc === "true") return true
    if (desc === "false") return false
    return desc
  }

  // Handle arrays - filter out undefined values
  if (Array.isArray(obj)) {
    return obj.map(item => mod2(item)).filter(item => item !== undefined)
  }

  // Handle binary data (Buffer, Uint8Array, etc.)
  if (
    obj instanceof Uint8Array ||
    obj instanceof ArrayBuffer ||
    Buffer.isBuffer(obj)
  ) {
    // Convert to empty string for empty buffers
    const buffer = Buffer.isBuffer(obj) ? obj : Buffer.from(obj)
    return buffer.length === 0 ? "" : buffer.toString("base64")
  }

  // Handle objects - remove undefined properties
  if (typeof obj === "object" && obj !== null) {
    const result = {}
    for (const [key, value] of Object.entries(obj)) {
      const modifiedValue = mod2(value)
      // Only add the property if the value is not undefined
      if (modifiedValue !== undefined) {
        // Lowercase the key when creating the result object
        result[key.toLowerCase()] = modifiedValue
      }
    }
    return result
  }

  // Handle strings - check if it's already an atom-like string
  if (typeof obj === "string" && obj.match(/^[a-z_][a-zA-Z0-9_]*$/)) {
    // This looks like an atom value, keep as is
    return obj
  }

  // Return primitive values as-is (strings, numbers, booleans, null)
  return obj
}

export { prepare, getJWK, seed, mod, mod2 }
