import base64url from "base64url"
import crypto from "crypto"
export { send } from "./signer-utils.js"
import { toHttpSigner } from "./signer-utils.js"

/**
 * HyperBEAM Encoding Logic
 */
const MAX_HEADER_LENGTH = 4096

async function hasNewline(value) {
  if (typeof value === "string") return value.includes("\n")
  if (value instanceof Blob) {
    value = await value.text()
    return value.includes("\n")
  }
  if (isBytes(value)) return Buffer.from(value).includes("\n")
  return false
}

async function sha256(data) {
  return crypto.subtle.digest("SHA-256", data)
}

function isBytes(value) {
  return value instanceof ArrayBuffer || ArrayBuffer.isView(value)
}

function isPojo(value) {
  return (
    !isBytes(value) &&
    !Array.isArray(value) &&
    !(value instanceof Blob) &&
    typeof value === "object" &&
    value !== null
  )
}

function hasNestedObjects(obj) {
  return Object.values(obj).some(v => isPojo(v))
}

function hbEncodeValue(value) {
  if (isBytes(value)) {
    if (value.byteLength === 0) return hbEncodeValue("")
    return [undefined, value]
  }

  if (typeof value === "string") {
    if (value.length === 0) return ["empty-binary", undefined]
    return [undefined, value]
  }

  if (Array.isArray(value)) {
    if (value.length === 0) return ["empty-list", undefined]
    // For structured fields, encode based on the type of each element
    const encoded = value
      .map(v => {
        if (typeof v === "string") {
          // Escape quotes and backslashes
          const escaped = v.replace(/\\/g, "\\\\").replace(/"/g, '\\"')
          return `"${escaped}"`
        } else if (typeof v === "number") {
          // Numbers should be encoded as bare items, not strings
          return String(v)
        } else if (typeof v === "boolean") {
          // Booleans as structured field tokens
          return v ? "?1" : "?0"
        }
        // Fallback for other types
        return `"${String(v)}"`
      })
      .join(", ")
    return ["list", encoded]
  }

  if (typeof value === "number") {
    if (!Number.isInteger(value)) return ["float", `${value}`]
    return ["integer", String(value)]
  }

  if (typeof value === "boolean") {
    return [undefined, String(value)]
  }

  if (typeof value === "symbol") {
    return ["atom", value.description]
  }

  // Objects should be handled by hbEncodeLift, not here
  if (isPojo(value)) {
    throw new Error(
      `Object should have been flattened by hbEncodeLift: ${JSON.stringify(value)}`
    )
  }

  throw new Error(`Cannot encode value: ${value.toString()}`)
}

function hbEncodeLift(obj, parent = "", top = {}) {
  const [flattened, types] = Object.entries({ ...obj }).reduce(
    (acc, [key, value]) => {
      // skip nullish values
      if (value == null) return acc

      // list of objects
      if (Array.isArray(value) && value.some(isPojo)) {
        value = value.reduce(
          (indexedObj, v, idx) => Object.assign(indexedObj, { [idx + 1]: v }),
          {}
        )
      }

      // Handle maps/objects
      if (isPojo(value)) {
        // Check if this object has any nested objects or arrays with objects
        const hasComplexValues = Object.values(value).some(
          v => isPojo(v) || (Array.isArray(v) && v.some(item => isPojo(item)))
        )

        if (!hasComplexValues) {
          // Simple flat object - can still have simple arrays
          const items = []

          Object.entries(value).forEach(([k, v]) => {
            const subKey = k.toLowerCase()

            if (typeof v === "string") {
              const escaped = v.replace(/\\/g, "\\\\").replace(/"/g, '\\"')
              items.push(`${subKey}="${escaped}"`)
            } else if (typeof v === "number") {
              items.push(`${subKey}=${v}`)
              if (Number.isInteger(v)) {
                acc[1][`${key.toLowerCase()}%2f${subKey}`] = "integer"
              } else {
                acc[1][`${key.toLowerCase()}%2f${subKey}`] = "float"
              }
            } else if (typeof v === "boolean") {
              items.push(`${subKey}=${v ? "?1" : "?0"}`)
            } else if (Array.isArray(v) && !v.some(item => isPojo(item))) {
              // Simple array (no objects) - encode as structured field inner list
              const listItems = v.map(item => {
                if (typeof item === "string") {
                  return `"${item.replace(/\\/g, "\\\\").replace(/"/g, '\\"')}"`
                } else if (typeof item === "number") {
                  return String(item)
                } else if (typeof item === "boolean") {
                  return item ? "?1" : "?0"
                } else {
                  return `"${String(item)}"`
                }
              })
              items.push(`${subKey}=(${listItems.join(" ")})`)
              // Don't add type for items inside structured field dictionaries
              // The parser will handle them based on the structured field format
            }
          })

          acc[0][key] = items.join(", ")
          acc[1][key.toLowerCase()] = "map"
        } else {
          // Has nested objects or arrays with objects - must use multipart body
          // DO NOT add to types - values in body should not be in ao-types
          acc[0][key] = value
        }

        return acc
      }

      // leaf encode value
      const [type, encoded] = hbEncodeValue(value)
      if (encoded !== undefined) {
        // Preserve original key casing for headers
        acc[0][key] = encoded
      }
      if (type) {
        // Only add type for lowercase key
        acc[1][key.toLowerCase()] = type
      }
      return acc
    },
    [{}, {}]
  )

  // Filter out any types for keys that are going to the body
  const bodyKeys = Object.keys(flattened).filter(key => {
    const value = flattened[key]
    return isPojo(value) && hasNestedObjects(value)
  })

  // Remove body field types from the types object
  bodyKeys.forEach(key => {
    delete types[key.toLowerCase()]
    // Also remove any nested types for this key
    Object.keys(types).forEach(typeKey => {
      if (typeKey.startsWith(key.toLowerCase() + "%2f")) {
        delete types[typeKey]
      }
    })
  })

  if (Object.keys(types).length > 0) {
    // Format as structured fields dictionary
    const aoTypeItems = Object.entries(types).map(([key, value]) => {
      // Types should be tokens, not quoted strings
      return `${key}=${value}`
    })
    aoTypeItems.sort()
    flattened["ao-types"] = aoTypeItems.join(", ")
  }
  Object.assign(top, flattened)
  return top
}

async function encode(obj = {}) {
  if (Object.keys(obj).length === 0) return undefined

  // Keep reference to original object for data field
  const originalObj = obj
  const flattened = hbEncodeLift(obj)

  const bodyKeys = []
  const headerKeys = []

  // Process all flattened keys
  await Promise.all(
    Object.keys(flattened).map(async key => {
      const value = flattened[key]

      // Skip ao-types - it's always a header
      if (key === "ao-types") {
        headerKeys.push(key)
        return
      }

      // Check if this should be a body field
      if (isPojo(value)) {
        // Objects with nested objects must go to body
        bodyKeys.push(key)
        return
      }

      const valueStr = String(value)
      if (
        (await hasNewline(valueStr)) ||
        key.includes("/") ||
        Buffer.from(valueStr).byteLength > MAX_HEADER_LENGTH
      ) {
        bodyKeys.push(key)
        return
      }

      // It's a header
      headerKeys.push(key)
    })
  )

  // Build headers object
  const headers = {}
  for (const key of headerKeys) {
    const value = flattened[key]
    headers[key] = String(value)
  }

  // Process body keys
  let body = undefined

  // Special handling for data and body fields
  if ("data" in originalObj && !bodyKeys.includes("data")) {
    if (
      typeof originalObj.data === "string" ||
      originalObj.data instanceof Blob ||
      isBytes(originalObj.data)
    ) {
      bodyKeys.push("data")
      delete headers["data"]
    }
  }

  // Body field is special - it should ALWAYS go to HTTP body
  if ("body" in originalObj && !bodyKeys.includes("body")) {
    bodyKeys.push("body")
    delete headers["body"]
  }

  // Clean up ao-types to remove any keys that are going to the body
  if (headers["ao-types"] && bodyKeys.length > 0) {
    const types = headers["ao-types"].split(", ").filter(t => {
      const [key] = t.split("=")
      return !bodyKeys.some(
        bodyKey =>
          key === bodyKey.toLowerCase() ||
          key.startsWith(bodyKey.toLowerCase() + "%2f")
      )
    })
    headers["ao-types"] = types.length > 0 ? types.join(", ") : undefined
    if (!headers["ao-types"]) delete headers["ao-types"]
  }

  if (bodyKeys.length > 0) {
    // Always use multipart for body content
    const boundary = crypto.randomUUID().replace(/-/g, "")
    const parts = []

    for (const key of bodyKeys) {
      const value = originalObj[key]

      if (isPojo(value)) {
        // For objects, recursively encode them
        const subEncoded = await encode(value)

        // Create the part with content-disposition header
        const disposition = `form-data;name="${key}"`
        const partHeaders = [`content-disposition: ${disposition}`]

        if (subEncoded && subEncoded.body) {
          // Has nested multipart - this object contains other objects
          if (subEncoded.headers["content-type"]) {
            partHeaders.push(
              `content-type: ${subEncoded.headers["content-type"]}`
            )
          }
          // Add all non-system headers
          Object.entries(subEncoded.headers).forEach(([k, v]) => {
            if (
              !["content-type", "content-digest", "content-length"].includes(k)
            ) {
              partHeaders.push(`${k}: ${v}`)
            }
          })

          const partBody = await subEncoded.body.text()
          parts.push(`--${boundary}`, partHeaders.join("\r\n"), "", partBody)
        } else if (subEncoded && subEncoded.headers) {
          // Simple object - just headers, no nested body
          // Add all headers from the sub-encoding
          Object.entries(subEncoded.headers).forEach(([k, v]) => {
            partHeaders.push(`${k}: ${v}`)
          })

          parts.push(
            `--${boundary}`,
            partHeaders.join("\r\n"),
            "",
            "" // Empty body
          )
        }
      } else {
        // Simple values
        const content = String(value)
        const disposition =
          key === "body" ? "inline" : `form-data;name="${key}"`
        parts.push(
          `--${boundary}`,
          `content-disposition: ${disposition}`,
          "",
          content
        )
      }
    }

    parts.push(`--${boundary}--`)
    body = new Blob([parts.join("\r\n")])
    headers["content-type"] = `multipart/form-data; boundary="${boundary}"`

    const finalContent = await body.arrayBuffer()
    const contentDigest = await sha256(finalContent)
    const base64 = base64url.toBase64(base64url.encode(contentDigest))

    headers["content-digest"] = `sha-256=:${base64}:`
    headers["content-length"] = String(finalContent.byteLength)
  }

  return { headers, body }
}

/**
 * Join URL parts
 */
const joinUrl = ({ url, path }) => {
  // If path is already a full URL, return it as-is
  if (path.startsWith("http://") || path.startsWith("https://")) {
    return path
  }

  // Otherwise, join the base URL with the path
  return url.endsWith("/") ? url.slice(0, -1) + path : url + path
}

export function createRequest(config) {
  const { signer, url = "http://localhost:10001" } = config

  if (!signer) {
    throw new Error("Signer is required for mainnet mode")
  }

  return async function request(fields) {
    const { path = "/relay/process", method = "POST", ...restFields } = fields

    // Add default AO fields
    const aoFields = { ...restFields }

    // Use the HyperBEAM encode function
    const encoded = await encode(aoFields)

    // If no encoding needed, create minimal structure
    const headersObj = encoded ? encoded.headers || {} : {}
    const body = encoded ? encoded.body : undefined

    const _url = joinUrl({ url, path })

    // Add Content-Length if body exists
    if (body && !headersObj["content-length"]) {
      const bodySize = body.size || body.byteLength || 0
      if (bodySize > 0) {
        headersObj["content-length"] = String(bodySize)
      }
    }

    // Create lowercase headers for signing
    const lowercaseHeaders = {}
    for (const [key, value] of Object.entries(headersObj)) {
      // Ensure all values are strings for the signature library
      lowercaseHeaders[key.toLowerCase()] = String(value)
    }

    // Get all header keys for signing (lowercase)
    const signingFields = Object.keys(lowercaseHeaders)

    // Sign the request with lowercase headers
    const signedRequest = await toHttpSigner(signer)({
      request: { url: _url, method, headers: lowercaseHeaders },
      fields: signingFields,
    })

    // Build final headers: send all headers in lowercase to match what was signed
    const finalHeaders = {}

    // Use the lowercase headers that were signed
    Object.assign(finalHeaders, lowercaseHeaders)

    // Override with the signature headers from the signed request
    finalHeaders["signature"] =
      signedRequest.headers["signature"] || signedRequest.headers["Signature"]
    finalHeaders["signature-input"] =
      signedRequest.headers["signature-input"] ||
      signedRequest.headers["Signature-Input"]

    // Return the signed message
    const result = {
      url: _url,
      method,
      headers: finalHeaders,
    }

    // Only add body if it exists
    if (body) {
      result.body = body
    }

    return result
  }
}
