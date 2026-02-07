import { id, base, hashpath, rsaid } from "./id.js"
import { toAddr } from "./utils.js"
import { extractPubKey } from "./signer-utils.js"
import { verify } from "./signer-utils.js"
import crypto from "crypto"

// Helper to compute SHA-256 content-digest in RFC 9530 format
const computeContentDigest = (body) => {
  let bodyBuffer
  if (Buffer.isBuffer(body)) {
    bodyBuffer = body
  } else if (body instanceof Blob) {
    return null // Can't compute synchronously for Blob
  } else if (typeof body === "string") {
    bodyBuffer = Buffer.from(body, "binary")
  } else {
    bodyBuffer = Buffer.from(String(body), "binary")
  }
  const hash = crypto.createHash("sha256").update(bodyBuffer).digest("base64")
  return `sha-256=:${hash}:`
}

// Helper to build ao-types string from an object
const buildAoTypes = (obj) => {
  const types = []
  for (const [key, value] of Object.entries(obj)) {
    if (typeof value === "number") {
      types.push(`${key}="${Number.isInteger(value) ? "integer" : "float"}"`)
    } else if (typeof value === "boolean") {
      types.push(`${key}="atom"`)
    } else if (value === null) {
      types.push(`${key}="atom"`)
    } else if (typeof value === "symbol") {
      // Symbols are Erlang atoms
      types.push(`${key}="atom"`)
    }
  }
  return types.length > 0 ? types.join(", ") : null
}

// todo: handle @
export const commit = async (obj, opts) => {
  const msg = await opts.signer(obj, opts)
  const {
    decodedSignatureInput: { components },
  } = await verify(msg)

  let body = {}

  // Check for inline-body-key (indicates a field was moved to HTTP body during encoding)
  const inlineBodyKey = msg.headers["inline-body-key"] || msg.headers["ao-body-key"]

  // Body field names that HyperBEAM's inline_key() recognizes natively.
  // For these, normalize_for_encoding() re-derives ao-body-key automatically.
  // For custom names (e.g., "json"), we must keep ao-body-key in committed list
  // so HyperBEAM knows which field to inline during verification.
  const NATIVE_BODY_KEYS = new Set(["body", "data"])
  const isNativeBodyKey = !inlineBodyKey || NATIVE_BODY_KEYS.has(inlineBodyKey)

  // Build body from committed components.
  // IMPORTANT: Use original typed values from obj (preserves integers, booleans, etc.)
  // instead of string values from headers. This is critical because:
  // 1. HTTP headers are always strings (e.g., quantity: 100 → "100")
  // 2. We include ao-types to tell HyperBEAM the original types
  // 3. HyperBEAM applies ao-types BEFORE signature verification
  // 4. If we send strings, HyperBEAM converts to integers, breaking signature
  // 5. If we send original integers, HyperBEAM re-encodes them as strings for verification
  //
  // Always skip content-digest and inline-body-key (transport artifacts re-derived by HyperBEAM).
  // Skip ao-body-key only for native body keys (HyperBEAM re-derives it).
  // Keep ao-body-key for custom body keys (HyperBEAM needs it to find the body field).

  // Create case-insensitive lookup maps
  const headerLookup = new Map()
  for (const [k, v] of Object.entries(msg.headers)) {
    headerLookup.set(k.toLowerCase(), v)
  }
  // Case-insensitive lookup for original object values (preserves types)
  const objLookup = new Map()
  for (const [k, v] of Object.entries(obj)) {
    objLookup.set(k.toLowerCase(), v)
  }

  for (const v of components) {
    const key = v === "@path" ? "path" : v
    if (key === "content-length") continue
    if (key === "content-digest") continue
    if (key === "inline-body-key") continue
    if (isNativeBodyKey && key === "ao-body-key") continue

    // Prefer original value from input object (preserves integer/boolean/etc. types)
    // Fall back to header string value if not found in original object
    const originalValue = objLookup.get(key.toLowerCase())
    if (originalValue !== undefined) {
      body[key] = originalValue
    } else {
      const headerValue = headerLookup.get(key)
      if (headerValue !== undefined) {
        body[key] = headerValue
      }
    }
  }

  // Handle body resolution - restore the inlined field to its AO-Core name
  let bodyContent = null
  if (msg.body) {
    if (msg.body instanceof Blob) {
      const arrayBuffer = await msg.body.arrayBuffer()
      bodyContent = Buffer.from(arrayBuffer)
    } else {
      bodyContent = msg.body
    }

    // Put body content under the original field name (e.g., "data", "json")
    if (inlineBodyKey) {
      body[inlineBodyKey] = bodyContent
    } else {
      body.body = bodyContent
    }
  }

  // Include non-committed fields from the original object as JSON values.
  // This covers: (1) body-key fields (arrays/objects encoded as multipart,
  // which can't be included as raw multipart in JSON), and (2) any other
  // fields excluded from signing. These fields are unsigned but present
  // so HyperBEAM can parse them directly as JSON types.
  // Use case-insensitive matching: signing normalizes keys to lowercase,
  // but the original object may use mixed case (e.g., "To" vs "to").
  const committedSetLower = new Set(components.map(v => (v === "@path" ? "path" : v).toLowerCase()))
  // Also track lowercase keys already in body to prevent duplicates
  const bodyKeysLower = new Set(Object.keys(body).map(k => k.toLowerCase()))
  for (const [key, value] of Object.entries(obj)) {
    // Skip HTTP method (not a data field)
    if (key === "method") continue
    // Skip if already in committed set (was signed)
    if (committedSetLower.has(key.toLowerCase())) continue
    // Skip if already in body (duplicate prevention)
    if (bodyKeysLower.has(key.toLowerCase())) continue
    if (value === undefined) continue
    body[key] = value
    bodyKeysLower.add(key.toLowerCase())
  }

  // Include ao-types so HyperBEAM knows how to convert string values to proper types.
  // First check if it was set by the signer, otherwise compute it from the original object
  let aoTypes = msg.headers["ao-types"]
  if (!aoTypes) {
    // Compute ao-types from the original typed values in obj
    aoTypes = buildAoTypes(obj)
  }
  if (aoTypes) {
    body["ao-types"] = aoTypes
  }

  const rsaId = rsaid(msg.headers)
  const pub = extractPubKey(msg.headers)
  const pubKeyBase64 = pub.toString("base64")
  const committer = toAddr(pubKeyBase64)

  // Extract keyid from signature-input to ensure it matches what was signed
  // Format: sig-xxx=("field1"...);alg="...";keyid="..."
  // The keyid may already have a scheme prefix (e.g., "publickey:base64data")
  const extractKeyidFromSigInput = (sigInput) => {
    if (!sigInput) return null
    const match = sigInput.match(/keyid="([^"]+)"/)
    if (!match) return null
    // Return as-is - the keyid already includes the prefix from signing
    return match[1]
  }
  const keyid = extractKeyidFromSigInput(msg.headers["signature-input"]) || `publickey:${pubKeyBase64}`

  // Build the list of committed fields.
  // Always transform HTTPSig transport keys to AO-Core keys:
  // - Replace content-digest with the body field name (HyperBEAM re-derives content-digest)
  // - For native body keys ("body", "data"): also remove ao-body-key (HyperBEAM re-derives it)
  // - For custom body keys (e.g., "json"): keep ao-body-key (HyperBEAM needs it to find the field)
  let committedFields = components.map(v => v === "@path" ? "path" : v)
  if (committedFields.includes("content-digest") && (inlineBodyKey || msg.body)) {
    const bodyFieldName = inlineBodyKey || "body"
    committedFields = committedFields.filter(k => k !== "content-digest")
    if (!committedFields.includes(bodyFieldName)) {
      committedFields.push(bodyFieldName)
    }
  }
  if (isNativeBodyKey) {
    // For native body keys, ao-body-key is a transport artifact - remove it
    committedFields = committedFields.filter(k => k !== "ao-body-key")
  }

  // Extract just the base64 signature data from the header format "sig-xxx=:base64data:"
  // HyperBEAM expects raw base64 without colons (uses b64fast:encode/decode)
  const extractSignature = (sigHeader) => {
    if (!sigHeader) return sigHeader
    // Match the base64 data between colons after the label
    const match = sigHeader.match(/=:([^:]+):/)
    return match ? match[1] : sigHeader
  }
  const rawSignature = extractSignature(msg.headers.signature)
  const meta = {
    type: "rsa-pss-sha512",
    alg: "rsa-pss-sha512",
    "commitment-device": "httpsig@1.0",
    keyid: keyid,
    committed: committedFields
  }
  const sigs = {
    signature: rawSignature,
    "signature-input": msg.headers["signature-input"],
  }
  // Only include the RSA commitment - HMAC commitments are created by HyperBEAM
  // when needed and require the server's HMAC key which we don't have
  const committed = {
    commitments: {
      [rsaId]: { ...meta, committer, ...sigs },
    },
    ...body,
  }
  return committed
}
