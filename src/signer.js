import base64url from "base64url"
import crypto from "crypto"
import { httpbis } from "http-message-signatures"
import { parseItem, serializeList } from "structured-headers"
const { verifyMessage } = httpbis
const {
  augmentHeaders,
  createSignatureBase,
  createSigningParameters,
  formatSignatureBase,
} = httpbis

/**
 * Convert value to Buffer
 */
const toView = value => {
  if (ArrayBuffer.isView(value)) {
    return Buffer.from(value.buffer, value.byteOffset, value.byteLength)
  } else if (typeof value === "string") {
    return base64url.toBuffer(value)
  }
  throw new Error(
    "Value must be Uint8Array, ArrayBuffer, or base64url-encoded string"
  )
}

/**
 * Generate HTTP signature name from address
 */
const httpSigName = address => {
  const decoded = base64url.toBuffer(address)
  const hexString = [...decoded.subarray(1, 9)]
    .map(byte => byte.toString(16).padStart(2, "0"))
    .join("")
  return `http-sig-${hexString}`
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

function hbEncodeValue(value) {
  if (isBytes(value)) {
    if (value.byteLength === 0) return hbEncodeValue("")
    return [undefined, value]
  }

  if (typeof value === "string") {
    if (value.length === 0) return [undefined, "empty-binary"]
    return [undefined, value]
  }

  if (Array.isArray(value)) {
    if (value.length === 0) return ["empty-list", undefined]
    // For structured fields, just join the string values
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
        return `"${String(v)}"`
      })
      .join(", ")
    return ["list", encoded]
  }

  if (typeof value === "number") {
    if (!Number.isInteger(value)) return ["float", `${value}`]
    return ["integer", String(value)]
  }

  if (typeof value === "symbol") {
    return ["atom", value.description]
  }

  throw new Error(`Cannot encode value: ${value.toString()}`)
}

function hbEncodeLift(obj, parent = "", top = {}) {
  const [flattened, types] = Object.entries({ ...obj }).reduce(
    (acc, [key, value]) => {
      // For nested paths, preserve casing. For top-level, also preserve casing
      const storageKey = parent ? `${parent}/${key}` : key

      // skip nullish values
      if (value == null) return acc

      // list of objects
      if (Array.isArray(value) && value.some(isPojo)) {
        value = value.reduce(
          (indexedObj, v, idx) => Object.assign(indexedObj, { [idx]: v }),
          {}
        )
      }

      // first/second lift object - handle nested objects
      if (isPojo(value)) {
        // Check if this object has any nested objects or arrays with objects
        const hasComplexValues = Object.values(value).some(
          v => isPojo(v) || (Array.isArray(v) && v.some(item => isPojo(item)))
        )

        if (!hasComplexValues) {
          // Simple flat object - can be encoded as structured field dictionary
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
            }
          })

          acc[0][key] = items.join(", ")
          acc[1][key.toLowerCase()] = "map"
        } else {
          // Has nested objects - needs multipart encoding
          hbEncodeLift(value, storageKey, top)
        }

        return acc
      }

      // leaf encode value
      const [type, encoded] = hbEncodeValue(value)
      if (encoded !== undefined) {
        if (Buffer.from(String(encoded)).byteLength > MAX_HEADER_LENGTH) {
          top[storageKey] = String(encoded)
        } else {
          // Preserve the original key casing
          const httpKey = key
          if (type === "integer" && typeof value === "number") {
            acc[0][httpKey] = String(value)
          } else {
            acc[0][httpKey] = encoded
          }
        }
      }
      if (type) {
        // Store type with lowercase key for ao-types dictionary
        acc[1][key.toLowerCase()] = type
      }
      return acc
    },
    [{}, {}]
  )

  if (Object.keys(flattened).length === 0) return top

  if (Object.keys(types).length > 0) {
    // Format as structured fields dictionary
    const aoTypeItems = Object.entries(types).map(([key, value]) => {
      const safeKey = key
        .toLowerCase()
        .replace(
          /[^a-z0-9_-]/g,
          c => "%" + c.charCodeAt(0).toString(16).padStart(2, "0")
        )
      return `${safeKey}="${value}"`
    })
    aoTypeItems.sort()
    const aoTypes = aoTypeItems.join(", ")

    if (Buffer.from(aoTypes).byteLength > MAX_HEADER_LENGTH) {
      const flatK = parent ? `${parent}/ao-types` : "ao-types"
      top[flatK] = aoTypes
    } else {
      flattened["ao-types"] = aoTypes
    }
  }

  if (parent) {
    top[parent] = flattened
  } else {
    Object.assign(top, flattened)
  }

  return top
}

function encodePart(name, { headers = {}, body }) {
  // Convert headers to a plain object if it's a Headers instance
  const headerEntries =
    headers instanceof Headers
      ? Array.from(headers.entries())
      : Object.entries(headers || {})

  const parts = headerEntries.reduce(
    (acc, [name, value]) => {
      acc.push(`${name}: `, value, "\r\n")
      return acc
    },
    [`content-disposition: form-data;name="${name}"\r\n`]
  )

  if (body) parts.push("\r\n", body)

  return new Blob(parts)
}

async function encode(obj = {}) {
  if (Object.keys(obj).length === 0) return { headers: {}, body: undefined }

  // Keep reference to original object for data field
  const originalObj = obj
  const flattened = hbEncodeLift(obj)

  const bodyKeys = []
  const headerKeys = []

  // Process all flattened keys
  await Promise.all(
    Object.keys(flattened).map(async key => {
      const value = flattened[key]

      if (isPojo(value)) {
        const subPart = await encode(value)
        if (!subPart) return

        bodyKeys.push(key)
        flattened[key] = encodePart(key, subPart)
        return
      }

      // Check if this should be a body field
      const valueStr = String(value)
      if (
        (await hasNewline(valueStr)) ||
        key.includes("/") ||
        Buffer.from(valueStr).byteLength > MAX_HEADER_LENGTH
      ) {
        bodyKeys.push(key)
        flattened[key] = new Blob([
          `content-disposition: form-data;name="${key}"\r\n\r\n`,
          value,
        ])
        return
      }

      // It's a header
      headerKeys.push(key)
    })
  )

  // Build headers object with all header keys
  const headers = {}
  headerKeys.forEach(key => {
    headers[key] = flattened[key]
  })

  // Special handling for data and body fields
  if ("data" in originalObj && !bodyKeys.includes("data")) {
    bodyKeys.push("data")
    delete headers["data"] // Remove from headers if it was there
  }

  if ("body" in originalObj && !bodyKeys.includes("body")) {
    bodyKeys.push("body")
    delete headers["body"] // Remove from headers if it was there
  }

  let body = undefined
  if (bodyKeys.length > 0) {
    if (bodyKeys.length === 1) {
      // If there is only one element, promote it to be the full body
      const bodyKey = bodyKeys[0]
      const originalValue = originalObj[bodyKey]
      const flattenedValue = flattened[bodyKey]

      // Special handling only for body field with plain objects
      if (bodyKey === "body" && isPojo(originalValue)) {
        // Check if it was encoded as a structured field
        if (
          headers["ao-types"]?.includes('body="map"') &&
          typeof flattenedValue === "string"
        ) {
          body = new Blob([flattenedValue])
        } else {
          // Object but not encoded as structured field - use original
          body = new Blob([originalValue])
        }
      } else {
        // For data field and all non-object values, always use original
        body = new Blob([originalValue || flattenedValue])
      }
      headers["inline-body-key"] = bodyKey
    } else {
      // Multiple body fields - create multipart
      const bodyParts = await Promise.all(
        bodyKeys.map(async name => {
          if (flattened[name] instanceof Blob) {
            // The blob already has the content-disposition header
            return flattened[name]
          }
          // For raw values, we need to create a proper multipart part
          const value = originalObj[name] || flattened[name] || ""
          const partBlob = new Blob([
            `content-disposition: form-data;name="${name}"\r\n\r\n`,
            value,
          ])
          return partBlob
        })
      )

      // Calculate boundary from the content
      const allPartsBuffer = await new Blob(bodyParts).arrayBuffer()
      const hash = await sha256(allPartsBuffer)
      const boundary = base64url.encode(Buffer.from(hash))

      // Build the multipart body with proper boundaries
      const finalParts = []
      for (const part of bodyParts) {
        finalParts.push(`--${boundary}\r\n`)
        finalParts.push(part)
        finalParts.push("\r\n")
      }
      finalParts.push(`--${boundary}--`)

      headers["content-type"] = `multipart/form-data; boundary="${boundary}"`
      body = new Blob(finalParts)
    }

    if (body) {
      const finalContent = await body.arrayBuffer()
      const contentDigest = await sha256(finalContent)
      const base64 = base64url.toBase64(base64url.encode(contentDigest))

      // Use lowercase to match what's in the other headers
      headers["content-digest"] = `sha-256=:${base64}:`
      headers["content-length"] = String(finalContent.byteLength)
    }
  }

  return { headers, body }
}

/**
 * Create HTTP signer wrapper
 */
const toHttpSigner = signer => {
  const params = ["alg", "keyid"].sort()

  return async ({ request, fields }) => {
    let signatureBase
    let signatureInput
    let createCalled = false

    const create = injected => {
      createCalled = true

      const { publicKey, alg = "rsa-pss-sha512" } = injected

      const publicKeyBuffer = toView(publicKey)

      const signingParameters = createSigningParameters({
        params,
        paramValues: {
          keyid: base64url.encode(publicKeyBuffer),
          alg,
        },
      })

      const signatureBaseArray = createSignatureBase({ fields }, request)
      signatureInput = serializeList([
        [
          signatureBaseArray.map(([item]) => parseItem(item)),
          signingParameters,
        ],
      ])

      signatureBaseArray.push(['"@signature-params"', [signatureInput]])
      signatureBase = formatSignatureBase(signatureBaseArray)

      return new TextEncoder().encode(signatureBase)
    }

    const result = await signer(create, "httpsig")

    if (!createCalled) {
      throw new Error(
        "create() must be invoked in order to construct the data to sign"
      )
    }

    if (!result.signature || !result.address) {
      throw new Error("Signer must return signature and address")
    }

    const signatureBuffer = toView(result.signature)
    const signedHeaders = augmentHeaders(
      request.headers,
      signatureBuffer,
      signatureInput,
      httpSigName(result.address)
    )

    // Only lowercase the signature headers
    const finalHeaders = {}
    for (const [key, value] of Object.entries(signedHeaders)) {
      if (key === "Signature" || key === "Signature-Input") {
        finalHeaders[key.toLowerCase()] = value
      } else {
        finalHeaders[key] = value
      }
    }

    return {
      ...request,
      headers: finalHeaders,
    }
  }
}

/**
 * Create the main request function that creates signed messages locally
 *
 * @param {Object} config - Configuration object
 * @param {Function} config.signer - Signer function
 * @param {string} [config.HB_URL='http://relay.ao-hb.xyz'] - Base URL
 * @returns {Function} Request function that takes tags and returns signed message
 */
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
    const headersObj = encoded ? encoded.headers : {}
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
      lowercaseHeaders[key.toLowerCase()] = value
    }

    // Get all header keys for signing (lowercase)
    const signingFields = Object.keys(lowercaseHeaders)

    // Sign the request with lowercase headers
    const signedRequest = await toHttpSigner(signer)({
      request: { url: _url, method, headers: lowercaseHeaders },
      fields: signingFields,
    })

    // Build final headers: use original casing for all headers except signature headers
    const finalHeaders = {}

    // First, add all original headers with their original casing
    for (const [key, value] of Object.entries(headersObj)) {
      finalHeaders[key] = value
    }

    // Then add the signature headers (which should be lowercase)
    finalHeaders["signature"] = signedRequest.headers["signature"]
    finalHeaders["signature-input"] = signedRequest.headers["signature-input"]

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
/**
 * Utility function to extract the message ID from a signed message
 * Based on the original code's hash calculation
 */
async function getMessageId(signedMessage) {
  // Extract signature from the Signature header
  const signatureHeader =
    signedMessage.headers.Signature || signedMessage.headers.signature
  const match = signatureHeader.match(/Signature:\s*'http-sig-[^:]+:([^']+)'/)
  const signature = match ? match[1] : null

  if (!signature) {
    throw new Error("Could not extract signature from headers")
  }

  // Hash the signature to get message ID
  const encoder = new TextEncoder()
  const data = encoder.encode(signature)
  const hashBuffer = await crypto.subtle.digest("SHA-256", data)
  const hashArray = Array.from(new Uint8Array(hashBuffer))
  const hashBase64 = btoa(String.fromCharCode(...hashArray))

  return hashBase64
}

export async function send(signedMsg, fetchImpl = fetch) {
  const fetchOptions = {
    method: signedMsg.method,
    headers: signedMsg.headers,
    redirect: "follow",
  }

  // Only add body if it exists and method supports it
  if (
    signedMsg.body !== undefined &&
    signedMsg.method !== "GET" &&
    signedMsg.method !== "HEAD"
  ) {
    fetchOptions.body = signedMsg.body
  }
  const response = await fetchImpl(signedMsg.url, fetchOptions)

  if (response.status >= 400) {
    throw new Error(`${response.status}: ${await response.text()}`)
  }

  return {
    headers: response.headers,
    body: await response.text(),
    status: response.status,
  }
}

/**
 * Convert JWK modulus (n) to PEM format public key
 * @param {Buffer} nBuffer - The modulus buffer
 * @returns {string} PEM formatted public key
 */
function jwkModulusToPem(nBuffer) {
  // RSA public key with standard exponent
  const rsaPublicKey = crypto.createPublicKey({
    key: {
      kty: "RSA",
      n: base64url.encode(nBuffer),
      e: "AQAB", // Standard exponent 65537
    },
    format: "jwk",
  })

  return rsaPublicKey.export({ type: "spki", format: "pem" })
}

/**
 * Extract signature name from headers
 * @param {Object} headers - Request headers
 * @returns {string|null} Signature name or null
 */
function extractSignatureName(headers) {
  const signatureHeader = headers["signature"] || headers["Signature"]
  if (!signatureHeader) return null

  // Extract signature name (e.g., "http-sig-xxxxxxxx")
  // Handle both "name:" and "name=" formats
  const match = signatureHeader.match(/^([^:=]+)[:=]/)
  return match ? match[1] : null
}

/**
 * Extract public key from signature-input header
 * @param {Object} headers - Request headers
 * @param {string} [signatureName] - Optional signature name to look for
 * @returns {Buffer|null} Public key buffer or null
 */
function extractPublicKeyFromHeaders(headers, signatureName) {
  const signatureInput =
    headers["signature-input"] || headers["Signature-Input"]
  if (!signatureInput) return null

  // If we have a signature name, look for its specific keyid
  let keyidMatch
  if (signatureName) {
    // The signature-input format is: signatureName=(...);alg="...";keyid="..."
    // We need to match after the signature name
    const signatureSection = signatureInput.substring(
      signatureInput.indexOf(signatureName)
    )
    keyidMatch = signatureSection.match(/keyid="([^"]+)"/)
  } else {
    // General keyid match
    keyidMatch = signatureInput.match(/keyid="([^"]+)"/)
  }

  if (!keyidMatch) return null

  try {
    return base64url.toBuffer(keyidMatch[1])
  } catch (error) {
    return null
  }
}

/**
 * Verify an HTTP signed message using http-message-signatures
 *
 * @param {Object} signedMessage - The signed message to verify
 * @param {string} signedMessage.url - Request URL
 * @param {string} signedMessage.method - HTTP method
 * @param {Object} signedMessage.headers - Headers including signature
 * @param {string} [signedMessage.body] - Request body
 * @param {string|Buffer} [publicKey] - Optional public key (if not provided, extracts from keyid)
 * @returns {Object} Verification result
 */
export async function verify(signedMessage, publicKey) {
  try {
    const { url, method, headers, body } = signedMessage

    // Determine which public key to use
    let keyLookup

    if (publicKey) {
      // Use provided public key
      const pem =
        typeof publicKey === "string" ? publicKey : jwkModulusToPem(publicKey)

      keyLookup = async keyId => {
        return {
          id: keyId,
          algs: ["rsa-pss-sha512", "rsa-pss-sha256", "rsa-v1_5-sha256"],
          verify: async (data, signature, parameters) => {
            const verifier = crypto.createVerify(
              `RSA-SHA${parameters.alg.includes("512") ? "512" : "256"}`
            )
            verifier.update(data)

            if (parameters.alg.startsWith("rsa-pss")) {
              return verifier.verify(
                {
                  key: pem,
                  padding: crypto.constants.RSA_PKCS1_PSS_PADDING,
                  saltLength: crypto.constants.RSA_PSS_SALTLEN_DIGEST,
                },
                signature
              )
            } else {
              return verifier.verify(pem, signature)
            }
          },
        }
      }
    } else {
      // Extract public key from keyid
      const signatureName = extractSignatureName(headers)
      const extractedKey = extractPublicKeyFromHeaders(headers, signatureName)
      if (!extractedKey) {
        return {
          valid: false,
          error: "No public key provided and none found in signature",
        }
      }

      const pem = jwkModulusToPem(extractedKey)

      keyLookup = async keyId => {
        // The library might pass the keyId in different formats, so be flexible
        return {
          id: keyId,
          algs: ["rsa-pss-sha512", "rsa-pss-sha256", "rsa-v1_5-sha256"],
          verify: async (data, signature, parameters) => {
            try {
              const verifier = crypto.createVerify(
                `RSA-SHA${parameters.alg.includes("512") ? "512" : "256"}`
              )
              verifier.update(data)

              let verified
              if (parameters.alg.startsWith("rsa-pss")) {
                verified = verifier.verify(
                  {
                    key: pem,
                    padding: crypto.constants.RSA_PKCS1_PSS_PADDING,
                    saltLength: crypto.constants.RSA_PSS_SALTLEN_DIGEST,
                  },
                  signature
                )
              } else {
                verified = verifier.verify(pem, signature)
              }

              return verified
            } catch (error) {
              console.error("Verification error:", error)
              return false
            }
          },
        }
      }
    }

    // Create request object for verification
    const request = {
      method,
      url,
      headers: { ...headers },
    }

    // Extract additional info from headers
    const signatureName = extractSignatureName(headers)
    const extractedPublicKey = extractPublicKeyFromHeaders(
      headers,
      signatureName
    )

    // Extract algorithm from signature-input
    const signatureInputHeader =
      headers["signature-input"] || headers["Signature-Input"]
    const algMatch = signatureInputHeader?.match(/alg="([^"]+)"/)
    const algorithm = algMatch ? algMatch[1] : undefined

    // Verify using the library
    let verified = false
    let verificationError = null

    try {
      const verificationResult = await verifyMessage(
        {
          keyLookup,
          requiredFields: [], // Don't require specific fields
        },
        request
      )
      // If we get here without throwing, verification succeeded
      verified = true
    } catch (verifyError) {
      // Verification failed
      verificationError = verifyError.message
      verified = false
    }

    return {
      valid: true, // The signature format is valid
      verified, // Whether the cryptographic verification passed
      signatureName,
      keyId: extractedPublicKey
        ? base64url.encode(extractedPublicKey)
        : undefined,
      algorithm,
      publicKeyFromHeader: extractedPublicKey,
      ...(verificationError && { error: verificationError }),
    }
  } catch (error) {
    return {
      valid: false,
      error: error.message,
    }
  }
}

/**
 * Extract public key from a signed message
 *
 * @param {Object} signedMessage - The signed message
 * @returns {Buffer|null} The public key buffer or null
 */
function extractPublicKeyFromMessage(signedMessage) {
  const signatureName = extractSignatureName(signedMessage.headers)
  return extractPublicKeyFromHeaders(signedMessage.headers, signatureName)
}
