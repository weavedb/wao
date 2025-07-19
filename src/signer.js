import { toHttpSigner } from "./send.js"
import { enc } from "./encode.js"
import { createSigner } from "@permaweb/aoconnect"
export { verify } from "./signer-utils.js"

const joinUrl = ({ url, path }) => {
  if (path.startsWith("http://") || path.startsWith("https://")) return path
  const normalizedPath = path.startsWith("/") ? path : "/" + path
  return url.endsWith("/")
    ? url.slice(0, -1) + normalizedPath
    : url + normalizedPath
}

export async function sign({ url, path, msg: encoded, jwk, signPath = true }) {
  const signer = createSigner(jwk, url)
  const { body = null, ...headers } = encoded
  let _enc = { headers }
  if (body) _enc.body = new Blob([body])
  return await _sign({ signer, encoded: _enc, url, path, _path: signPath })
}

async function _sign({
  encoded,
  signer,
  path,
  url,
  method = "POST",
  _path = true,
}) {
  const headersObj = encoded ? encoded.headers : {}
  const body = encoded ? encoded.body : undefined
  let url_path = typeof _path === "string" ? _path : path
  const _url = joinUrl({ url, path: url_path })
  headersObj["path"] = path
  if (body && !headersObj["content-length"]) {
    const bodySize = body.size || body.byteLength || 0
    if (bodySize > 0) headersObj["content-length"] = String(bodySize)
  }
  const lowercaseHeaders = {}
  for (const [key, value] of Object.entries(headersObj)) {
    lowercaseHeaders[key.toLowerCase()] = value
  }
  const bodyKeys = headersObj["body-keys"]
    ? headersObj["body-keys"]
        .replace(/"/g, "")
        .split(",")
        .map(k => k.trim())
    : []
  let isPath = false
  const signingFields = Object.keys(lowercaseHeaders).filter(key => {
    if (key === "path") isPath = true
    return key !== "body-keys" && key !== "path" && !bodyKeys.includes(key)
  })
  if (_path !== false && isPath) signingFields.push("@path")
  const signedRequest = await toHttpSigner(signer)({
    request: { url: _url, method, headers: lowercaseHeaders },
    fields: signingFields,
  })

  const finalHeaders = {}

  for (const [key, value] of Object.entries(headersObj)) {
    finalHeaders[key] = value
  }

  finalHeaders["signature"] = signedRequest.headers["signature"]
  finalHeaders["signature-input"] = signedRequest.headers["signature-input"]

  if (headersObj["body-keys"]) {
    finalHeaders["body-keys"] = headersObj["body-keys"]
  }
  const result = { url: _url, method, headers: finalHeaders }

  if (body) result.body = body

  return result
}
export function signer(config) {
  const { signer, url = "http://localhost:10001" } = config
  if (!signer) throw new Error("Signer is required for mainnet mode")
  return async (
    fields,
    { encoded: _encoded = false, path: _path = true } = {}
  ) => {
    const { path = "/relay/process", method = "POST", ...aoFields } = fields
    const encoded = _encoded ? aoFields : await enc(aoFields)
    return await _sign({ url, method, path, _path, encoded, signer })
  }
}
