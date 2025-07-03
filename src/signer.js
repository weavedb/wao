import { toHttpSigner } from "./send.js"
import { enc } from "./encode.js"

const joinUrl = ({ url, path }) => {
  if (path.startsWith("http://") || path.startsWith("https://")) {
    return path
  }
  return url.endsWith("/") ? url.slice(0, -1) + path : url + path
}

export function signer(config) {
  const { signer, url = "http://localhost:10001" } = config

  if (!signer) {
    throw new Error("Signer is required for mainnet mode")
  }

  return async function sign(fields) {
    const { path = "/relay/process", method = "POST", ...restFields } = fields
    const aoFields = { ...restFields }
    const encoded = await enc(aoFields)
    const headersObj = encoded ? encoded.headers : {}
    const body = encoded ? encoded.body : undefined

    const _url = joinUrl({ url, path })

    if (body && !headersObj["content-length"]) {
      const bodySize = body.size || body.byteLength || 0
      if (bodySize > 0) {
        headersObj["content-length"] = String(bodySize)
      }
    }

    const lowercaseHeaders = {}
    for (const [key, value] of Object.entries(headersObj)) {
      lowercaseHeaders[key.toLowerCase()] = value
    }

    const signingFields = Object.keys(lowercaseHeaders).filter(
      key => key !== "body-keys"
    )

    if (signingFields.length === 0 && !body) {
      lowercaseHeaders["content-length"] = "0"
      signingFields.push("content-length")
    }

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
}
