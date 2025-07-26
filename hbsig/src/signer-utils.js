import base64url from "base64url"
import crypto from "crypto"
import { httpbis } from "./http-message-signatures/index.js"
import { parseItem, serializeList } from "structured-headers"
const {
  augmentHeaders,
  createSignatureBase,
  createSigningParameters,
  formatSignatureBase,
} = httpbis

const { verifyMessage } = httpbis

/**
 * Decode signature-input header to extract all components
 * Handles format: signature-name=(components);params
 *
 * @param {string} signatureInput - The signature-input header value
 * @param {string} [signatureName] - Optional specific signature name to decode
 * @returns {Object} Decoded signature input with components and parameters
 */
export function decodeSigInput(signatureInput, signatureName = null) {
  if (!signatureInput) {
    return null
  }

  try {
    // If signature name is provided, extract just that section
    let inputToDecode = signatureInput
    if (signatureName) {
      // Find the section for this specific signature
      const startIndex = signatureInput.indexOf(signatureName)
      if (startIndex === -1) {
        return null
      }

      // Extract from signature name to the next signature (if any) or end
      const nextSigMatch = signatureInput
        .substring(startIndex + signatureName.length)
        .match(/,\s*[a-zA-Z0-9-]+=/)
      const endIndex = nextSigMatch
        ? startIndex + signatureName.length + nextSigMatch.index
        : signatureInput.length

      inputToDecode = signatureInput.substring(startIndex, endIndex).trim()
    }

    // Parse each signature entry
    const signatures = {}

    // Split by signature entries (handle multiple signatures)
    const entries = inputToDecode.split(/,(?=\s*[a-zA-Z0-9-]+=)/)

    for (const entry of entries) {
      const trimmedEntry = entry.trim()

      // Match signature-name=(components);params format
      const match = trimmedEntry.match(/^([a-zA-Z0-9-]+)=\(([^)]*)\)(.*)$/)
      if (!match) {
        continue
      }

      const sigName = match[1]
      const componentsStr = match[2]
      const paramsStr = match[3]

      // Parse components (space-separated, may be quoted)
      const components = []
      const componentRegex = /"[^"]+"|[^\s]+/g
      let componentMatch
      while ((componentMatch = componentRegex.exec(componentsStr)) !== null) {
        components.push(componentMatch[0].replace(/"/g, ""))
      }

      // Parse parameters (semicolon-separated key="value" pairs)
      const params = {}
      if (paramsStr) {
        const paramPairs = paramsStr.split(";").filter(p => p.trim())
        for (const pair of paramPairs) {
          const [key, ...valueParts] = pair.split("=")
          if (key && valueParts.length > 0) {
            const value = valueParts.join("=").replace(/^"|"$/g, "")
            params[key.trim()] = value
          }
        }
      }

      signatures[sigName] = {
        components,
        params,
        raw: trimmedEntry,
      }
    }

    // If specific signature was requested, return just that one
    if (signatureName && signatures[signatureName]) {
      return signatures[signatureName]
    }

    // Return all signatures or the first one if no specific name was given
    return signatureName ? null : signatures
  } catch (error) {
    console.error("Error decoding signature-input:", error)
    return null
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
      const extractedKey = extractPubKey(headers, signatureName)
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
    const extractedPublicKey = extractPubKey(headers, signatureName)

    // Extract algorithm from signature-input
    const signatureInputHeader =
      headers["signature-input"] || headers["Signature-Input"]
    const decodedSigInput = decodeSigInput(signatureInputHeader, signatureName)
    const algorithm = decodedSigInput?.params?.alg

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
      decodedSignatureInput: decodedSigInput,
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
 * Extract public key from signature-input header
 * @param {Object} headers - Request headers
 * @param {string} [signatureName] - Optional signature name to look for
 * @returns {Buffer|null} Public key buffer or null
 */
export function extractPubKey(headers, signatureName) {
  const signatureInput =
    headers["signature-input"] || headers["Signature-Input"]
  if (!signatureInput) return null

  // Use the decoder to properly parse the signature-input
  const decoded = decodeSigInput(signatureInput, signatureName)

  if (!decoded) return null

  // If we decoded a specific signature, use its keyid
  const keyid =
    signatureName && decoded.params
      ? decoded.params.keyid
      : Object.values(decoded)[0]?.params?.keyid

  if (!keyid) return null

  try {
    return base64url.toBuffer(keyid)
  } catch (error) {
    return null
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
  return extractPubKey(signedMessage.headers, signatureName)
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
 * Create HTTP signer wrapper
 */
export const toHttpSigner = signer => {
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
