import base64url from "base64url"

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
 * This is a convenience wrapper that extracts the signature name first
 *
 * @param {Object} signedMessage - The signed message with headers
 * @returns {Buffer|null} The public key buffer or null
 */
export function extractPublicKeyFromMessage(signedMessage) {
  const signatureName = extractSignatureName(signedMessage.headers)
  return extractPubKey(signedMessage.headers, signatureName)
}
