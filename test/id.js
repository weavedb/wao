import crypto from "crypto"

/**
 * Parse structured field dictionary format
 * Handles both complex format: name=(components);params
 * and simple format: name=:value:
 */
function parseStructuredFieldDictionary(input) {
  // Try complex format first
  const match = input.match(/([^=]+)=\((.*?)\);(.*)$/)
  if (match) {
    const name = match[1]
    const components = match[2].split(" ")
    const params = {}

    const paramPairs = match[3].split(";").filter(p => p)
    paramPairs.forEach(pair => {
      const [key, value] = pair.split("=")
      if (key && value) {
        params[key] = value.replace(/"/g, "")
      }
    })

    return { name, components, params }
  }

  // Try simple format
  const simpleMatch = input.match(/([^=]+)=:([^:]+):/)
  if (simpleMatch) {
    return { name: simpleMatch[1], value: simpleMatch[2] }
  }

  return null
}

/**
 * Generate commitment ID for RSA-PSS and ECDSA signatures
 * The ID is the SHA256 hash of the raw signature bytes
 *
 * @param {Object} commitment - The commitment object containing signature
 * @returns {string} The commitment ID in base64url format
 */
function generateRsaCommitmentId(commitment) {
  // Extract the base64 signature from structured field format
  // Format: "signature-name=:BASE64_SIGNATURE:"
  const match = commitment.signature.match(/^[^=]+=:([^:]+):/)
  if (!match) {
    throw new Error("Invalid signature format")
  }

  const signatureBase64 = match[1]
  const signatureBinary = Buffer.from(signatureBase64, "base64")

  // SHA256 hash of the raw signature
  const hash = crypto.createHash("sha256")
  hash.update(signatureBinary)
  const id = hash.digest("base64url")

  return id
}

/**
 * Generate HMAC commitment ID for HyperBEAM messages
 * The ID is deterministic based on message content only
 *
 * The Erlang implementation sorts components WITH @ prefix included,
 * then removes @ from derived components in the signature base.
 *
 * @param {Object} message - The message with signature and signature-input
 * @returns {string} The commitment ID in base64url format
 */
function generateHmacCommitmentId(message) {
  // Parse signature-input to get components
  const parsed = parseStructuredFieldDictionary(message["signature-input"])
  if (!parsed || !parsed.components) {
    throw new Error("Failed to parse signature-input")
  }

  // Sort components AS-IS (with quotes and @ prefix)
  const sortedComponents = [...parsed.components].sort()

  // Build signature base in sorted order
  const lines = []

  sortedComponents.forEach(component => {
    const cleanComponent = component.replace(/"/g, "")
    let fieldName = cleanComponent
    let value

    // For derived components (starting with @), remove @ in the signature base
    if (cleanComponent.startsWith("@")) {
      fieldName = cleanComponent.substring(1)
      value = message[fieldName]
    } else {
      value = message[cleanComponent]
    }

    if (value === undefined || value === null) {
      value = ""
    } else if (typeof value === "number") {
      value = value.toString()
    }

    lines.push(`"${fieldName}": ${value}`)
  })

  // Add signature-params line with sorted components (keeping @ prefix)
  const paramsComponents = sortedComponents.join(" ")
  lines.push(
    `"@signature-params": (${paramsComponents});alg="hmac-sha256";keyid="ao"`
  )

  const signatureBase = lines.join("\n")

  // Generate HMAC with key "ao"
  const hmac = crypto.createHmac("sha256", "ao")
  hmac.update(signatureBase, "utf8")
  return hmac.digest().toString("base64url")
}

/**
 * Generate commitment ID based on the algorithm type
 *
 * @param {Object} commitment - The commitment object containing alg, signature, etc.
 * @param {Object} fullMessage - The full message (required for HMAC)
 * @returns {string} The commitment ID in base64url format
 */
function generateCommitmentId(commitment, fullMessage = null) {
  switch (commitment.alg) {
    case "rsa-pss-sha512":
    case "ecdsa-p256-sha256":
      return generateRsaCommitmentId(commitment)

    case "hmac-sha256":
      if (!fullMessage) {
        throw new Error("HMAC commitment IDs require full message context")
      }
      return generateHmacCommitmentId(fullMessage)

    default:
      throw new Error(`Unsupported algorithm: ${commitment.alg}`)
  }
}

/**
 * Extract all commitment IDs from a HyperBEAM message
 *
 * @param {Object} message - The message with commitments
 * @returns {Object} Map of commitment IDs to their types
 */
function extractCommitmentIds(message) {
  const ids = {}

  if (!message.commitments) {
    return ids
  }

  for (const [id, commitment] of Object.entries(message.commitments)) {
    ids[id] = {
      alg: commitment.alg,
      committer: commitment.committer,
      device: commitment["commitment-device"],
    }
  }

  return ids
}

/**
 * Verify a commitment ID matches the expected value
 *
 * @param {Object} commitment - The commitment object
 * @param {string} expectedId - The expected commitment ID
 * @param {Object} fullMessage - The full message (required for HMAC)
 * @returns {boolean} True if the ID matches
 */
function verifyCommitmentId(commitment, expectedId, fullMessage = null) {
  try {
    const calculatedId = generateCommitmentId(commitment, fullMessage)
    return calculatedId === expectedId
  } catch (error) {
    console.error("Error verifying commitment ID:", error)
    return false
  }
}

// Export all functions
export {
  generateCommitmentId,
  generateRsaCommitmentId,
  generateHmacCommitmentId,
  extractCommitmentIds,
  verifyCommitmentId,
  parseStructuredFieldDictionary,
}
