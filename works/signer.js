import { toHttpSigner } from "./send.js"
import { enc } from "./encode.js"

const joinUrl = ({ url, path }) => {
  if (path.startsWith("http://") || path.startsWith("https://")) {
    return path
  }
  // Ensure path starts with /
  const normalizedPath = path.startsWith("/") ? path : "/" + path
  return url.endsWith("/")
    ? url.slice(0, -1) + normalizedPath
    : url + normalizedPath
}

export function signer(config) {
  const { signer, url = "http://localhost:10001" } = config

  if (!signer) {
    throw new Error("Signer is required for mainnet mode")
  }

  return async function sign(fields, { path: _path = false } = {}) {
    console.log("fields", fields)
    const { path = "/relay/process", method = "POST", ...restFields } = fields
    const aoFields = { ...restFields }
    const encoded = await enc(aoFields)
    const headersObj = encoded ? encoded.headers : {}
    const body = encoded ? encoded.body : undefined

    const _url = joinUrl({ url, path })

    headersObj["path"] = path

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
    if (lowercaseHeaders.path && /^\//.test(lowercaseHeaders.path)) {
      //const sp = lowercaseHeaders.path.split("/")
      //lowercaseHeaders.path = sp.slice(sp.length - 1).join("/")
    }
    // Parse body-keys if present
    const bodyKeys = headersObj["body-keys"]
      ? headersObj["body-keys"]
          .replace(/"/g, "")
          .split(",")
          .map(k => k.trim())
      : []

    // Collect fields to sign from headers
    const signingFields = Object.keys(lowercaseHeaders).filter(
      key => key !== "body-keys" && key !== "path" && !bodyKeys.includes(key) // Also exclude fields that are in body-keys
    )
    // Always include @path in the signature
    //if (!signingFields.includes("path")) signingFields.push("path")
    if (_path) signingFields.push("path")
    // Ensure we have at least one field to sign
    if (signingFields.length === 0 && !body) {
      lowercaseHeaders["content-length"] = "0"
      signingFields.push("content-length")
    }

    const signedRequest = await toHttpSigner(signer)({
      request: {
        url: _url,
        method,
        headers: lowercaseHeaders,
      },
      fields: signingFields,
    })
    console.log(lowercaseHeaders)
    console.log(signingFields)
    const finalHeaders = {}

    for (const [key, value] of Object.entries(headersObj)) {
      finalHeaders[key] = value
    }

    finalHeaders["signature"] = signedRequest.headers["signature"]
    finalHeaders["signature-input"] = signedRequest.headers["signature-input"]

    if (headersObj["body-keys"]) {
      finalHeaders["body-keys"] = headersObj["body-keys"]
    }
    console.log("url:", _url)
    console.log(finalHeaders)
    const result = { url: _url, method, headers: finalHeaders }

    if (body) result.body = body

    return result
  }
}

// stack / simple-pay / patch / hyperbeam / hyperbeam-aos / scheduler / cron
