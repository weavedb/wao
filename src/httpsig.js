// httpsig.js - JavaScript implementation of HTTP Signature codec

import crypto from "crypto"

const CRLF = "\r\n"
const DOUBLE_CRLF = CRLF + CRLF
const MAX_HEADER_LENGTH = 4096

// Helper to normalize keys (lowercase only, no underscore conversion)
function normalizeKey(key) {
  if (typeof key === "string") {
    return key.toLowerCase()
  }
  return String(key).toLowerCase()
}

// Helper to check if a value is an ID (43 character base64url string)
function isId(value) {
  return (
    typeof value === "string" &&
    value.length === 43 &&
    /^[A-Za-z0-9_-]+$/.test(value)
  )
}

// Helper to encode structured field dictionary
function encodeSfDict(dict) {
  const items = []
  for (const [key, value] of Object.entries(dict)) {
    if (typeof value === "string") {
      items.push(`${key}="${value}"`)
    } else {
      items.push(`${key}=${value}`)
    }
  }
  return items.join(", ")
}

// Helper to parse structured field dictionary
function parseSfDict(str) {
  const dict = {}
  if (!str) return dict

  const parts = str.split(/,\s*/)
  for (const part of parts) {
    const match = part.match(/^([^=]+)=(.+)$/)
    if (match) {
      const key = match[1].trim()
      let value = match[2].trim()
      if (value.startsWith('"') && value.endsWith('"')) {
        value = value.slice(1, -1)
      }
      dict[key] = value
    }
  }
  return dict
}

// Helper to generate boundary from parts
function boundaryFromParts(parts) {
  const bodyBin = parts.map(p => p.body).join(CRLF)
  const hash = crypto.createHash("sha256")
  hash.update(bodyBin)
  return hash.digest("base64url")
}

// Helper to determine inline key
function inlineKey(msg) {
  const inlineBodyKey = msg["inline-body-key"]
  if (inlineBodyKey) {
    return [{}, inlineBodyKey]
  }
  if ("body" in msg) {
    return [{}, "body"]
  }
  if ("data" in msg) {
    return [{ "inline-body-key": "data" }, "data"]
  }
  return [{}, "body"]
}

// Group IDs into ao-ids field
function groupIds(map) {
  const idDict = {}
  const stripped = {}

  for (const [key, value] of Object.entries(map)) {
    if (isId(key) && typeof value === "string") {
      // Store with lowercase key as in Erlang
      idDict[key.toLowerCase()] = value
    } else {
      stripped[key] = value
    }
  }

  if (Object.keys(idDict).length > 0) {
    const items = []
    for (const [k, v] of Object.entries(idDict)) {
      items.push(`${k}="${v}"`)
    }
    stripped["ao-ids"] = items.join(", ")
  }

  // Return IDs as lowercase in the result since they will be normalized
  // when processed as headers
  for (const [k, v] of Object.entries(idDict)) {
    stripped[k] = v
  }

  return stripped
}

// Ungroup IDs from ao-ids field
function ungroupIds(msg) {
  if (!msg["ao-ids"]) return msg

  const result = { ...msg }
  delete result["ao-ids"]

  const dict = parseSfDict(msg["ao-ids"])
  for (const [key, value] of Object.entries(dict)) {
    // ao-ids keys are lowercase, use them as-is
    result[key] = value
  }

  return result
}

// Group maps for body encoding - following Erlang logic exactly
function groupMaps(map, parent = "", top = {}) {
  if (typeof map !== "object" || map === null) return top

  const flattened = {}
  let newTop = { ...top }

  // Process entries in sorted order for consistency
  const entries = Object.entries(map).sort(([a], [b]) => a.localeCompare(b))

  for (const [key, value] of entries) {
    // Normalize keys to lowercase
    const normKey = normalizeKey(key)
    const flatK = parent ? `${parent}/${normKey}` : normKey

    if (typeof value === "object" && value !== null && !Array.isArray(value)) {
      // Recursively process nested objects
      newTop = groupMaps(value, flatK, newTop)
    } else if (typeof value === "string" && value.length > MAX_HEADER_LENGTH) {
      // Value too large for header, lift to top level
      newTop[flatK] = value
    } else {
      // Keep in current flattened map
      flattened[normKey] = value
    }
  }

  if (Object.keys(flattened).length === 0) {
    return newTop
  } else if (parent) {
    // Add flattened map under parent key
    return { ...newTop, [parent]: flattened }
  } else {
    // Merge flattened with top level
    return { ...newTop, ...flattened }
  }
}

// Encode multipart body part
function encodeBodyPart(partName, bodyPart, inlineKey) {
  const disposition =
    partName === inlineKey ? "inline" : `form-data;name="${partName}"`

  if (typeof bodyPart === "object" && bodyPart !== null) {
    // For objects, we need to match Erlang's behavior exactly
    // Erlang sorts ALL entries including content-disposition
    const allEntries = Object.entries(bodyPart)
      .filter(([key]) => key !== "body")
      .map(([key, value]) => [key, `${key}: ${value}`])

    // Add content-disposition to the entries
    allEntries.push([
      "content-disposition",
      `content-disposition: ${disposition}`,
    ])

    // Sort ALL entries alphabetically by key
    allEntries.sort(([a], [b]) => a.localeCompare(b))

    const headers = allEntries.map(([_, header]) => header)
    const headerStr = headers.join(CRLF)
    const body = bodyPart.body || ""

    return body ? `${headerStr}${DOUBLE_CRLF}${body}` : headerStr
  } else if (typeof bodyPart === "string") {
    return `content-disposition: ${disposition}${DOUBLE_CRLF}${bodyPart}`
  }
  return ""
}

// Encode HTTP message to binary
function encodeHttpMsg(httpsig) {
  const headers = []
  const body = httpsig.body || ""

  // Ensure content-disposition comes first if present
  if (httpsig["content-disposition"]) {
    headers.push(`content-disposition: ${httpsig["content-disposition"]}`)
  }

  // Then add all other headers in sorted order
  const otherEntries = Object.entries(httpsig)
    .filter(([name]) => name !== "body" && name !== "content-disposition")
    .sort(([a], [b]) => a.localeCompare(b))

  for (const [name, value] of otherEntries) {
    headers.push(`${name}: ${value}`)
  }

  const encodedHeaders = headers.join(CRLF)
  if (!body) {
    return encodedHeaders
  }
  return `${encodedHeaders}${DOUBLE_CRLF}${body}`
}

// Parse multipart body
function parseMultipart(contentType, body) {
  const boundaryMatch = contentType.match(/boundary="?([^";\s]+)"?/)
  if (!boundaryMatch) return {}

  const boundary = boundaryMatch[1]
  const boundaryDelim = `--${boundary}`
  const endBoundary = `--${boundary}--`

  // Remove the final boundary terminator if present
  let bodyContent = body
  if (bodyContent.endsWith(endBoundary)) {
    bodyContent = bodyContent.substring(0, bodyContent.lastIndexOf(endBoundary))
  }

  const parts = bodyContent
    .split(new RegExp(`\\r?\\n?${boundaryDelim}\\r?\\n`))
    .filter(p => p && p.trim() && !p.startsWith("--"))

  const result = {}
  const bodyKeysList = []

  for (const part of parts) {
    const [headerBlock, ...bodyParts] = part.split(DOUBLE_CRLF)
    let partBody = bodyParts.join(DOUBLE_CRLF)

    // Remove trailing CRLF
    partBody = partBody.replace(/\r?\n?$/, "")

    const headers = {}
    const headerLines = headerBlock.split(/\r?\n/)
    for (const line of headerLines) {
      const colonIndex = line.indexOf(": ")
      if (colonIndex > 0) {
        const name = line.substring(0, colonIndex).toLowerCase()
        const value = line.substring(colonIndex + 2)
        headers[name] = value
      }
    }

    const disposition = headers["content-disposition"]
    if (!disposition) continue

    let partName
    if (disposition === "inline") {
      partName = "body"
      bodyKeysList.push("body")
    } else {
      const nameMatch = disposition.match(/name="([^"]+)"/)
      partName = nameMatch ? nameMatch[1] : null
      if (partName) {
        // Add the top-level key for this part
        const topLevelKey = partName.split("/")[0]
        bodyKeysList.push(topLevelKey)
      }
    }

    if (!partName) continue

    const restHeaders = { ...headers }
    delete restHeaders["content-disposition"]

    if (Object.keys(restHeaders).length === 0) {
      result[partName] = partBody
    } else if (!partBody) {
      result[partName] = restHeaders
    } else {
      result[partName] = { ...restHeaders, body: partBody }
    }
  }

  if (bodyKeysList.length > 0) {
    // Format as structured field list, preserving order and duplicates
    result["body-keys"] = bodyKeysList.map(k => `"${k}"`).join(", ")
  }

  return result
}

// Convert from flat to nested structure
function flatToNested(flat) {
  const result = {}

  for (const [path, value] of Object.entries(flat)) {
    const parts = path.split("/")
    let current = result

    for (let i = 0; i < parts.length - 1; i++) {
      const part = parts[i]
      if (!current[part]) {
        current[part] = {}
      }
      current = current[part]
    }

    current[parts[parts.length - 1]] = value
  }

  return result
}

// Add content-digest header
function addContentDigest(msg) {
  if (!msg.body) return msg

  const hash = crypto.createHash("sha256")
  hash.update(msg.body, "utf8")
  const digest = hash.digest("base64")

  return {
    ...msg,
    "content-digest": `sha-256=:${digest}:`,
  }
}

/**
 * Convert HTTP message to TABM
 */
export function httpsig_from(http) {
  if (typeof http === "string") return http

  const body = http.body || ""
  const [inlinedFieldHdrs, inlinedKey] = inlineKey(http)
  const headers = { ...http }
  delete headers.body
  delete headers["body-keys"]

  const contentType = headers["content-type"]
  let withBodyKeys = headers

  // Parse multipart body if present
  if (body && contentType && contentType.includes("multipart")) {
    const parsed = parseMultipart(contentType, body)

    // Handle the body-keys from the original HTTP message
    if (http["body-keys"]) {
      // Parse the existing body-keys and ensure they're in quoted format
      const bodyKeys = http["body-keys"]
      if (typeof bodyKeys === "string") {
        if (!bodyKeys.includes('"')) {
          // Convert unquoted format to quoted format
          parsed["body-keys"] = bodyKeys
            .split(/,\s*/)
            .map(k => `"${k.trim()}"`)
            .join(", ")
        } else {
          // Already quoted, use as-is
          parsed["body-keys"] = bodyKeys
        }
      }
    }

    withBodyKeys = { ...headers, ...parsed }

    // Convert flat structure to nested
    const flat = {}
    for (const [key, value] of Object.entries(withBodyKeys)) {
      if (key.includes("/")) {
        flat[key] = value
      }
    }

    if (Object.keys(flat).length > 0) {
      const nested = flatToNested(flat)
      for (const [key, value] of Object.entries(nested)) {
        withBodyKeys[key] = value
      }
      for (const key of Object.keys(flat)) {
        delete withBodyKeys[key]
      }
    }
  } else if (body) {
    withBodyKeys[inlinedKey] = body
  }

  // Ungroup IDs
  const withIds = ungroupIds(withBodyKeys)

  // Remove signature-related headers and content-digest
  const result = { ...withIds }
  delete result.signature
  delete result["signature-input"]
  delete result.commitments
  delete result["content-digest"]

  // Extract hashpaths if any
  for (const key of Object.keys(result)) {
    if (key.startsWith("hashpath")) {
      delete result[key]
    }
  }

  return result
}

/**
 * Convert TABM to HTTP message
 */
export function httpsig_to(tabm) {
  if (typeof tabm === "string") return tabm

  // Group IDs
  const withGroupedIds = groupIds(tabm)

  // Remove private and signature-related keys
  const stripped = { ...withGroupedIds }
  delete stripped.commitments
  delete stripped.signature
  delete stripped["signature-input"]
  delete stripped.priv

  const [inlineFieldHdrs, inlineKeyVal] = inlineKey(tabm)

  // Group maps and prepare body
  const bodyMap = {}
  const headers = { ...inlineFieldHdrs }

  for (const [key, value] of Object.entries(stripped)) {
    if (key === "body" || key === inlineKeyVal) {
      bodyMap[key === inlineKeyVal ? inlineKeyVal : "body"] = value
    } else if (
      typeof value === "object" &&
      value !== null &&
      !Array.isArray(value)
    ) {
      bodyMap[key] = value
    } else if (typeof value === "string" && value.length <= MAX_HEADER_LENGTH) {
      // Normalize headers: lowercase but preserve underscores
      headers[key.toLowerCase()] = value
    } else {
      bodyMap[key] = value
    }
  }

  // Handle body encoding
  const groupedBodyMap = groupMaps(bodyMap)

  if (Object.keys(groupedBodyMap).length === 0) {
    // Empty body
    return headers
  } else if (
    Object.keys(groupedBodyMap).length === 1 &&
    groupedBodyMap[inlineKeyVal] &&
    typeof groupedBodyMap[inlineKeyVal] === "string"
  ) {
    // Single body value
    const result = { ...headers, body: groupedBodyMap[inlineKeyVal] }
    return addContentDigest(result)
  } else {
    // Multipart body
    const parts = []
    const bodyKeysList = []

    // Sort entries for consistent ordering to match Erlang
    const sortedEntries = Object.entries(groupedBodyMap).sort(([a], [b]) =>
      a.localeCompare(b)
    )

    for (const [key, value] of sortedEntries) {
      // Special handling for single-key objects with only "body"
      if (
        typeof value === "object" &&
        value !== null &&
        Object.keys(value).length === 1 &&
        "body" in value
      ) {
        // Encode with /body suffix to preserve hierarchy
        const encoded = encodeBodyPart(`${key}/body`, value, "body")
        parts.push({
          name: `${key}/body`,
          body: encoded,
        })
        bodyKeysList.push(key)
      } else {
        const encoded = encodeBodyPart(key, value, inlineKeyVal)
        parts.push({
          name: key,
          body: encoded,
        })
        bodyKeysList.push(key)
      }
    }

    const boundary = boundaryFromParts(parts)

    const bodyParts = parts.map(p => `--${boundary}${CRLF}${p.body}`)
    const finalBody = bodyParts.join(CRLF) + `${CRLF}--${boundary}--`

    const result = {
      ...headers,
      "body-keys": bodyKeysList.map(k => `"${k}"`).join(", "),
      "content-type": `multipart/form-data; boundary="${boundary}"`,
      body: finalBody,
    }

    return addContentDigest(result)
  }
}
