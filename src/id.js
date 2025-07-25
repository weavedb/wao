import { hash, hmac } from "fast-sha256"

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
 * Convert base64url string to base64
 */
function base64urlToBase64(str) {
  return str.replace(/-/g, "+").replace(/_/g, "/")
}

/**
 * Generate commitment ID for RSA-PSS and ECDSA signatures
 * The ID is the SHA256 hash of the raw signature bytes
 *
 * @param {Object} commitment - The commitment object containing signature
 * @returns {string} The commitment ID in base64url format
 */
function rsaid(commitment) {
  // Extract the base64 signature from structured field format
  // Format: "signature-name=:BASE64_SIGNATURE:"
  const match = commitment.signature.match(/^[^=]+=:([^:]+):/)
  if (!match) {
    throw new Error("Invalid signature format")
  }

  const signatureBase64 = match[1]
  // Convert base64 to Uint8Array
  const signatureBinary = Uint8Array.from(atob(signatureBase64), c =>
    c.charCodeAt(0)
  )

  // SHA256 hash of the raw signature
  const hashResult = hash(signatureBinary)
  const id = uint8ArrayToBase64url(hashResult)

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
function hmacid(message) {
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
  // Convert string to Uint8Array
  const messageBytes = new TextEncoder().encode(signatureBase)
  const keyBytes = new TextEncoder().encode("ao")

  const hmacResult = hmac(keyBytes, messageBytes)
  return uint8ArrayToBase64url(hmacResult)
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
      return rsaid(commitment)

    case "hmac-sha256":
      if (!fullMessage) {
        throw new Error("HMAC commitment IDs require full message context")
      }
      return hmacid(fullMessage)

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

/**
 * Convert Uint8Array to base64url string
 */
function uint8ArrayToBase64url(bytes) {
  let binary = ""
  for (let i = 0; i < bytes.length; i++) {
    binary += String.fromCharCode(bytes[i])
  }
  const base64 = btoa(binary)
  return base64.replace(/\+/g, "-").replace(/\//g, "_").replace(/=/g, "")
}

/**
 * Parse structured field dictionary to extract components
 * Handles format: name=(components);params
 */
function parseSignatureInput(sigInput) {
  // Extract components from format: name=(components);params
  const match = sigInput.match(/[^=]+=\(([^)]+)\)/)
  if (!match) return []

  // Split components and clean quotes
  return match[1].split(" ").map(c => c.replace(/"/g, ""))
}

/**
 * Calculate HMAC commitment ID for HyperBEAM messages
 */
function calculateHmacId(message) {
  if (!message["signature-input"]) {
    throw new Error("HMAC calculation requires signature-input")
  }

  // Parse components from signature-input
  const components = parseSignatureInput(message["signature-input"])

  // Sort components AS-IS (with @ prefix)
  const sortedComponents = [...components].sort()

  // Build signature base in sorted order
  const lines = []

  for (const component of sortedComponents) {
    let fieldName = component
    let value

    // For derived components (starting with @), remove @ in the signature base
    if (component.startsWith("@")) {
      fieldName = component.substring(1)
      value = message[fieldName]
    } else {
      value = message[component]
    }

    if (value === undefined || value === null) {
      value = ""
    } else if (typeof value === "number") {
      value = value.toString()
    }

    lines.push(`"${fieldName}": ${value}`)
  }

  // Add signature-params line with sorted components (keeping @ prefix)
  const paramsComponents = sortedComponents.join(" ")
  lines.push(
    `"@signature-params": (${paramsComponents});alg="hmac-sha256";keyid="ao"`
  )

  const signatureBase = lines.join("\n")

  // Generate HMAC with key "ao"
  const messageBytes = new TextEncoder().encode(signatureBase)
  const keyBytes = new TextEncoder().encode("ao")

  const hmacResult = hmac(keyBytes, messageBytes)
  return uint8ArrayToBase64url(hmacResult)
}

/**
 * Calculate unsigned message ID following the exact Erlang flow
 */
function calculateUnsignedId(message) {
  // Derived components from Erlang ?DERIVED_COMPONENTS
  const DERIVED_COMPONENTS = [
    "method",
    "target-uri",
    "authority",
    "scheme",
    "request-target",
    "path",
    "query",
    "query-param",
    "status",
  ]

  // Convert message for httpsig format
  const httpsigMsg = {}
  for (const [key, value] of Object.entries(message)) {
    httpsigMsg[key.toLowerCase()] = value
  }

  // Get keys and add @ to derived components
  const keys = Object.keys(httpsigMsg)
  const componentsWithPrefix = keys
    .map(key => {
      // Check if this is a derived component
      if (DERIVED_COMPONENTS.includes(key.replace(/_/g, "-"))) {
        return "@" + key
      }
      return key
    })
    .sort() // Sort AFTER adding @ prefix

  // Build signature base - use the components in order
  const lines = []
  for (const component of componentsWithPrefix) {
    const key = component.replace("@", "")
    const value = httpsigMsg[key]
    const valueStr = typeof value === "string" ? value : String(value)
    lines.push(`"${key}": ${valueStr}`)
  }

  // Add signature-params line with the @ prefixes
  const componentsList = componentsWithPrefix.map(k => `"${k}"`).join(" ")
  lines.push(
    `"@signature-params": (${componentsList});alg="hmac-sha256";keyid="ao"`
  )

  const signatureBase = lines.join("\n")

  // HMAC with key "ao"
  const messageBytes = new TextEncoder().encode(signatureBase)
  const keyBytes = new TextEncoder().encode("ao")

  const hmacResult = hmac(keyBytes, messageBytes)
  return uint8ArrayToBase64url(hmacResult)
}

function id(message) {
  // Get commitment IDs
  const commitmentIds = Object.keys(message.commitments || {})

  if (commitmentIds.length === 0) {
    // No commitments - calculate unsigned ID using HMAC
    return calculateUnsignedId(message)
  } else if (commitmentIds.length === 1) {
    // Single commitment - the ID is just the commitment ID
    return commitmentIds[0]
  } else {
    // Multiple commitments - sort, join with ", ", and hash
    const sortedIds = commitmentIds.sort()
    const idsLine = sortedIds.join(", ")

    // Calculate SHA-256 hash using fast-sha256
    const encoder = new TextEncoder()
    const data = encoder.encode(idsLine)
    const hashArray = hash(data)

    // Convert to base64url
    return uint8ArrayToBase64url(hashArray)
  }
}
// Export all functions
export {
  id,
  generateCommitmentId,
  rsaid,
  hmacid,
  extractCommitmentIds,
  verifyCommitmentId,
  parseStructuredFieldDictionary,
}

/**
 * Calculate the next base from a hashpath
 * A hashpath has the format: base/request
 * The next base is calculated as: sha256(base + request)
 *
 * @param {string} hashpath - The current hashpath in format "base/request"
 * @returns {string} The next base in base64url format
 */
function base(hashpath) {
  // Split the hashpath into base and request
  const parts = hashpath.split("/")
  if (parts.length !== 2) {
    throw new Error("Invalid hashpath format. Expected 'base/request'")
  }

  const [base, request] = parts

  // Convert base64url to native binary (Uint8Array)
  const baseBinary = base64urlToUint8Array(base)
  const requestBinary = base64urlToUint8Array(request)

  // Concatenate base and request
  const combined = new Uint8Array(baseBinary.length + requestBinary.length)
  combined.set(baseBinary, 0)
  combined.set(requestBinary, baseBinary.length)

  // Calculate SHA256 of the combined data
  const nextBaseHash = hash(combined)

  // Convert to base64url
  return uint8ArrayToBase64url(nextBaseHash)
}

/**
 * Calculate the next hashpath given the current hashpath and a new message
 *
 * @param {string} currentHashpath - The current hashpath (or null for first operation)
 * @param {Object} newMessage - The new message/request
 * @returns {string} The next hashpath in format "nextBase/newMessageId"
 */
function hashpath(currentHashpath, newMessage) {
  // Calculate the ID of the new message
  const newMessageId = id(newMessage)

  if (!currentHashpath) {
    // First operation: the hashpath is just the message ID
    // In the Erlang code, the first hashpath is "baseId/requestId"
    // where baseId is the ID of the initial message
    throw new Error(
      "For first operation, provide the base message ID as currentHashpath"
    )
  }

  // Check if this is the first operation (currentHashpath is just an ID, not a path)
  if (!currentHashpath.includes("/")) {
    // First operation: currentHashpath is the base message ID
    return `${currentHashpath}/${newMessageId}`
  }

  // Subsequent operations: calculate the next base from current hashpath
  const nextBase = base(currentHashpath)

  // Return the new hashpath
  return `${nextBase}/${newMessageId}`
}

/**
 * Helper function to convert base64url string to Uint8Array
 */
function base64urlToUint8Array(base64url) {
  // Convert base64url to base64
  const base64 = base64urlToBase64(base64url)

  // Decode base64 to binary string
  const binaryString = atob(base64)

  // Convert binary string to Uint8Array
  const bytes = new Uint8Array(binaryString.length)
  for (let i = 0; i < binaryString.length; i++) {
    bytes[i] = binaryString.charCodeAt(i)
  }

  return bytes
}

// Export the new functions
export { base, hashpath }
