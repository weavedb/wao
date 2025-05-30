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
 * Encode fields to headers and body
 */
function encode(fields) {
  const headers = {}
  const body = fields.data || ""

  // Convert all fields to headers except data and body
  Object.entries(fields).forEach(([key, value]) => {
    if (key !== "data" && key !== "body") {
      headers[key] = String(value)
    }
  })

  // Set content headers
  if (body) {
    headers["Content-Type"] = "application/octet-stream"
    headers["Content-Length"] = String(body.length)

    // Add content-digest for body integrity
    const bodyBuffer =
      typeof body === "string" ? new TextEncoder().encode(body) : body
    const digest = crypto.createHash("sha256").update(bodyBuffer).digest()
    headers["Content-Digest"] = `sha-256=:${digest.toString("base64")}:`
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
  const { signer, HB_URL = "http://localhost:10000" } = config

  if (!signer) {
    throw new Error("Signer is required for mainnet mode")
  }

  /**
   * Create a signed message with tags (no network request)
   *
   * @param {Object} fields - Request fields (tags)
   * @param {string} [fields.path='/relay/process'] - Path to append to base URL
   * @param {string} [fields.method='POST'] - HTTP method
   * @returns {Promise<Object>} Signed message object
   */
  return async function request(fields) {
    const { path = "/relay/process", method = "POST", ...restFields } = fields

    // Add default AO fields
    const aoFields = {
      "Data-Protocol": "ao",
      Type: "Message",
      Variant: "ao.N.1",
      ...restFields,
    }

    // Encode fields to headers and body
    const { headers, body } = encode(aoFields)

    // Create request with proper headers
    const encoded = encode(aoFields)
    const url = joinUrl({ url: HB_URL, path })

    // Ensure all headers are lowercase for HyperBEAM compatibility
    const headersObj = {}
    if (encoded.headers instanceof Headers) {
      for (const [key, value] of encoded.headers.entries()) {
        headersObj[key.toLowerCase()] = value
      }
    } else {
      for (const [key, value] of Object.entries(encoded.headers)) {
        headersObj[key.toLowerCase()] = value
      }
    }

    // Get all header keys for signing (excluding any you might want to skip)
    const signingFields = Object.keys(headersObj).filter(key => {
      // Optionally exclude headers that shouldn't be signed
      // For now, include everything
      return true
    })

    // Sign the request
    const signedRequest = await toHttpSigner(signer)({
      request: { url, method, headers: headersObj },
      fields: signingFields,
    })

    // Return the signed message (no fetch!)
    return {
      url,
      method,
      headers: signedRequest.headers,
      body: encoded.body,
    }
  }
}

/**
 * Utility function to extract the message ID from a signed message
 * Based on the original code's hash calculation
 */
export async function getMessageId(signedMessage) {
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
  const response = await fetchImpl(signedMsg.url, {
    method: signedMsg.method,
    headers: signedMsg.headers,
    body: signedMsg.body,
    redirect: "follow",
  })

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
export function extractPublicKeyFromMessage(signedMessage) {
  const signatureName = extractSignatureName(signedMessage.headers)
  return extractPublicKeyFromHeaders(signedMessage.headers, signatureName)
}

/**
 * Simple verification helper that returns just true/false
 *
 * @param {Object} signedMessage - The signed message
 * @param {string|Buffer} [publicKey] - Optional public key
 * @returns {Promise<boolean>} True if valid, false otherwise
 */
export async function isValidSignature(signedMessage, publicKey) {
  const result = await verify(signedMessage, publicKey)
  return result.valid
}
