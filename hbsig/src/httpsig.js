// httpsig.js - JavaScript implementation of HTTP Signature codec

import { hash } from "fast-sha256"
import { flat_from, flat_to } from "./flat.js"
import { structured_from as structuredFrom, structured_to as structuredTo } from "./structured.js"

const CRLF = "\r\n"
const DOUBLE_CRLF = CRLF + CRLF
const MAX_HEADER_LENGTH = 4096

// Helper to convert string to Uint8Array
function stringToBytes(str, encoding = "utf8") {
  if (encoding === "binary") {
    const bytes = new Uint8Array(str.length)
    for (let i = 0; i < str.length; i++) {
      bytes[i] = str.charCodeAt(i) & 0xff
    }
    return bytes
  }
  return new TextEncoder().encode(str)
}

// Helper to convert bytes to base64url
function bytesToBase64url(bytes) {
  const chars =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"
  let result = ""

  for (let i = 0; i < bytes.length; i += 3) {
    const a = bytes[i]
    const b = i + 1 < bytes.length ? bytes[i + 1] : 0
    const c = i + 2 < bytes.length ? bytes[i + 2] : 0

    const combined = (a << 16) | (b << 8) | c

    result += chars[(combined >> 18) & 0x3f]
    result += chars[(combined >> 12) & 0x3f]
    if (i + 1 < bytes.length) result += chars[(combined >> 6) & 0x3f]
    if (i + 2 < bytes.length) result += chars[combined & 0x3f]
  }

  return result
}

// Helper to convert bytes to base64
function bytesToBase64(bytes) {
  const chars =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
  let result = ""

  for (let i = 0; i < bytes.length; i += 3) {
    const a = bytes[i]
    const b = i + 1 < bytes.length ? bytes[i + 1] : 0
    const c = i + 2 < bytes.length ? bytes[i + 2] : 0

    const combined = (a << 16) | (b << 8) | c

    result += chars[(combined >> 18) & 0x3f]
    result += chars[(combined >> 12) & 0x3f]
    result += i + 1 < bytes.length ? chars[(combined >> 6) & 0x3f] : "="
    result += i + 2 < bytes.length ? chars[combined & 0x3f] : "="
  }

  return result
}

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

// Helper to generate boundary from parts using fast-sha256
function boundaryFromParts(parts) {
  const bodyBin = parts.map(p => p.body).join(CRLF)
  const hashBytes = hash(stringToBytes(bodyBin, "binary"))
  return bytesToBase64url(hashBytes)
}

// Helper to determine inline key - matches Erlang's inline_key/2
function inlineKey(msg) {
  // Check for ao-body-key (Erlang uses ao-body-key, not inline-body-key)
  const aoBodyKey = msg["ao-body-key"]
  if (aoBodyKey) {
    return [{}, aoBodyKey]
  }
  if ("body" in msg) {
    return [{}, "body"]
  }
  if ("data" in msg) {
    return [{ "ao-body-key": "data" }, "data"]
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

// Get the size of a map (matches Erlang's maps:size behavior)
// This counts ALL keys including ao-types - empty means literally {}
function mapSize(obj) {
  if (typeof obj !== "object" || obj === null) return 0
  return Object.keys(obj).length
}

// Group maps for body encoding - following Erlang logic exactly
function groupMaps(map, parent = "", top = {}) {
  if (
    typeof map !== "object" ||
    map === null ||
    Array.isArray(map) ||
    Buffer.isBuffer(map)
  ) {
    return top
  }

  const flattened = {}
  let newTop = { ...top }

  // Process entries in sorted order for consistency
  const entries = Object.entries(map).sort(([a], [b]) => a.localeCompare(b))

  for (const [key, value] of entries) {
    // Normalize keys to lowercase
    const normKey = normalizeKey(key)
    const flatK = parent ? `${parent}/${normKey}` : normKey

    if (
      typeof value === "object" &&
      value !== null &&
      !Array.isArray(value) &&
      !Buffer.isBuffer(value)
    ) {
      // Check size of the nested object (including metadata keys like ao-types)
      // Empty means literally {} - a map with only ao-types is NOT empty
      const size = mapSize(value)

      if (size === 0) {
        // Empty map (no data keys) - add empty-message marker
        // This matches Erlang's group_maps behavior for empty maps
        newTop[flatK] = { "ao-types": "empty-message" }
      } else {
        // Recursively process nested objects
        newTop = groupMaps(value, flatK, newTop)
      }
    } else if (typeof value === "string" && value.length > MAX_HEADER_LENGTH) {
      // Value too large for header, lift to top level
      newTop[flatK] = value
    } else if (Buffer.isBuffer(value) && value.length > MAX_HEADER_LENGTH) {
      // Large buffers also get lifted to top level
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

// Helper to compute content-digest for a body value
function computePartDigest(bodyValue) {
  let bodyBytes
  if (Buffer.isBuffer(bodyValue)) {
    bodyBytes = new Uint8Array(bodyValue)
  } else if (typeof bodyValue === "string") {
    bodyBytes = stringToBytes(bodyValue, "binary")
  } else {
    bodyBytes = stringToBytes(String(bodyValue), "binary")
  }
  const hashBytes = hash(bodyBytes)
  return `sha-256=:${bytesToBase64(hashBytes)}:`
}

// Encode multipart body part
// NOTE: This matches Erlang's encode_body_part/4 which does NOT apply inline_key
// logic to nested parts. For nested maps, ALL fields become headers (except 'body'
// which becomes the part body). The inline_key logic is only for top-level messages.
function encodeBodyPart(partName, bodyPart, inlineKey) {
  const disposition =
    partName === inlineKey ? "inline" : `form-data;name="${partName}"`

  if (
    typeof bodyPart === "object" &&
    bodyPart !== null &&
    !Array.isArray(bodyPart) &&
    !Buffer.isBuffer(bodyPart)
  ) {
    // Collect all headers (everything except 'body' and 'priv')
    const allEntries = []

    for (const [key, value] of Object.entries(bodyPart)) {
      if (key === "body" || key === "priv") continue

      // Handle Buffer values properly
      let valueStr = value
      if (Buffer.isBuffer(value)) {
        // Use binary/latin1 encoding to preserve all byte values 0-255
        valueStr = value.toString("binary")
      }
      allEntries.push({ key: key, line: `${key}: ${valueStr}` })
    }

    // Add content-disposition
    allEntries.push({
      key: "content-disposition",
      line: `content-disposition: ${disposition}`,
    })

    // Sort all entries by key alphabetically - matches Erlang behavior
    allEntries.sort((a, b) => a.key.localeCompare(b.key))

    // Build the lines
    const lines = allEntries.map(entry => entry.line)

    // Only the 'body' field (if present) becomes the part body
    const body = bodyPart.body
    if (body !== "" && body !== undefined && body !== null) {
      lines.push("") // Always add empty line before body
      lines.push(Buffer.isBuffer(body) ? body.toString("binary") : String(body))
    }

    return lines.join(CRLF)
  } else if (typeof bodyPart === "string" || Buffer.isBuffer(bodyPart)) {
    // Use binary/latin1 encoding to preserve byte values 0-255
    const bodyStr = Buffer.isBuffer(bodyPart) ? bodyPart.toString("binary") : bodyPart
    return `content-disposition: ${disposition}${DOUBLE_CRLF}${bodyStr}`
  }
  return ""
}

// Helper to detect if a Buffer contains binary data
function isBinaryData(buf) {
  // Check first 512 bytes for binary indicators
  const checkLength = Math.min(buf.length, 512)

  // Any null byte means binary
  for (let i = 0; i < checkLength; i++) {
    if (buf[i] === 0) return true
  }

  // Count non-text bytes
  let controlCount = 0
  let highByteCount = 0

  for (let i = 0; i < checkLength; i++) {
    const byte = buf[i]
    // Non-printable chars (except CR/LF/TAB)
    if (byte < 32 && byte !== 9 && byte !== 10 && byte !== 13) {
      controlCount++
    }
    // High byte range
    if (byte > 127) {
      highByteCount++
    }
  }

  // If more than 5% are control chars, likely binary
  if (controlCount / checkLength > 0.05) return true

  // If more than 20% are high bytes, likely binary
  if (highByteCount / checkLength > 0.2) return true

  return false
}

// Parse multipart body - handles binary data in headers
function parseMultipart(contentType, body) {
  const boundaryMatch = contentType.match(/boundary="?([^";\s]+)"?/)
  if (!boundaryMatch) return {}

  const boundary = boundaryMatch[1]
  const boundaryDelim = `--${boundary}`
  const endBoundary = `--${boundary}--`

  // Use binary encoding to preserve all bytes as character codes 0-255
  let bodyContent = typeof body === "string" ? body : body.toString("binary")
  if (bodyContent.endsWith(endBoundary)) {
    bodyContent = bodyContent.substring(0, bodyContent.lastIndexOf(endBoundary))
  }

  const parts = bodyContent
    .split(new RegExp(`\\r?\\n?${boundaryDelim}\\r?\\n`))
    .filter(p => p && p.trim() && !p.startsWith("--"))

  const result = {}
  const bodyKeysList = []

  for (const part of parts) {
    // First split to find the headers/body boundary (double CRLF)
    const headerBodySplit = part.indexOf(DOUBLE_CRLF)
    let headerBlock, partBody

    if (headerBodySplit !== -1) {
      headerBlock = part.substring(0, headerBodySplit)
      partBody = part.substring(headerBodySplit + DOUBLE_CRLF.length)
      // Remove trailing CRLF from body
      partBody = partBody.replace(/\r?\n?$/, "")
    } else {
      headerBlock = part
      partBody = ""
    }

    const headers = {}

    // Parse headers more carefully to handle binary data
    let currentPos = 0
    while (currentPos < headerBlock.length) {
      // Find the next colon to identify a potential header
      let colonPos = headerBlock.indexOf(": ", currentPos)

      if (colonPos === -1) {
        // No more headers
        break
      }

      // Look backwards from colon to find the start of this line
      let lineStart = currentPos
      let searchBack = colonPos - 1
      while (searchBack >= currentPos) {
        if (headerBlock[searchBack] === "\n") {
          lineStart = searchBack + 1
          break
        }
        searchBack--
      }

      // Extract the header name
      const name = headerBlock
        .substring(lineStart, colonPos)
        .trim()
        .toLowerCase()

      // Check if this looks like a valid header name
      if (!/^[a-zA-Z0-9-]+$/.test(name) || name.length > 50) {
        // Not a valid header, skip past this colon
        currentPos = colonPos + 2
        continue
      }

      // Start of value is after ": "
      let valueStart = colonPos + 2

      // Find the end of this header's value by looking for the next valid header
      let valueEnd = headerBlock.length
      let searchPos = valueStart

      while (searchPos < headerBlock.length) {
        let nextNewline = headerBlock.indexOf("\n", searchPos)
        if (nextNewline === -1) break

        let nextLineStart = nextNewline + 1
        if (nextLineStart >= headerBlock.length) break

        // Check if next line starts with a header pattern
        let nextColon = headerBlock.indexOf(": ", nextLineStart)

        if (nextColon > nextLineStart && nextColon < nextLineStart + 50) {
          let possibleHeaderName = headerBlock
            .substring(nextLineStart, nextColon)
            .trim()

          // Must be valid header name format
          if (/^[a-zA-Z0-9-]+$/.test(possibleHeaderName)) {
            valueEnd = nextNewline
            break
          }
        }

        searchPos = nextNewline + 1
      }

      // Extract the value - valueEnd points to \n which is NOT part of the value
      let value = headerBlock.substring(valueStart, valueEnd)

      // Determine if this is binary data FIRST (before trimming)
      const isBinary =
        value.length > 0 && isBinaryData(Buffer.from(value, "binary"))

      if (isBinary) {
        // Binary data: only remove trailing \r if it's part of CRLF separator
        // (when valueEnd points to \n and value ends with \r)
        if (
          valueEnd < headerBlock.length &&
          headerBlock[valueEnd] === "\n" &&
          value.endsWith("\r")
        ) {
          value = value.substring(0, value.length - 1)
        }
        headers[name] = Buffer.from(value, "binary")
      } else {
        // Text data: remove all trailing CRLF/LF (these are line separators, not data)
        value = value.replace(/[\r\n]+$/, "")
        headers[name] = value
      }

      // Move to the end of this header's value
      currentPos = valueEnd
      if (currentPos < headerBlock.length && headerBlock[currentPos] === "\n") {
        currentPos++
      }
      if (currentPos < headerBlock.length && headerBlock[currentPos] === "\r") {
        currentPos++
      }
    }

    const disposition = headers["content-disposition"]
    if (!disposition) continue

    let partName
    if (disposition === "inline") {
      // This is the inline part
      partName = "body"
      bodyKeysList.push("body")

      // Extract all headers from inline part as top-level fields
      const restHeaders = { ...headers }
      delete restHeaders["content-disposition"]

      // Add each header from the inline part to the top level of result
      for (const [key, value] of Object.entries(restHeaders)) {
        result[key] = value
      }

      // If there's body content in the inline part, add it as 'body'
      if (partBody) {
        // Keep as string unless it's binary data
        result[partName] = isBinaryData(Buffer.from(partBody, "binary"))
          ? Buffer.from(partBody, "binary")
          : partBody
      }
    } else {
      // Handle named form-data parts
      const nameMatch = disposition.match(/name="([^"]+)"/)
      partName = nameMatch ? nameMatch[1] : null
      if (partName) {
        const topLevelKey = partName.split("/")[0]
        bodyKeysList.push(topLevelKey)
      }

      if (!partName) continue

      const restHeaders = { ...headers }
      delete restHeaders["content-disposition"]

      if (Object.keys(restHeaders).length === 0) {
        // Keep as string unless it's binary data
        result[partName] = isBinaryData(Buffer.from(partBody, "binary"))
          ? Buffer.from(partBody, "binary")
          : partBody
      } else if (!partBody) {
        // ao-types should stay with this part, not be extracted
        result[partName] = restHeaders
      } else {
        // Keep as string unless it's binary data
        result[partName] = {
          ...restHeaders,
          body: isBinaryData(Buffer.from(partBody, "binary"))
            ? Buffer.from(partBody, "binary")
            : partBody,
        }
      }
    }
  }

  if (bodyKeysList.length > 0) {
    result["body-keys"] = bodyKeysList.map(k => `"${k}"`).join(", ")
  }

  return result
}

// Add content-digest header using fast-sha256
function addContentDigest(msg) {
  if (!msg.body) return msg

  let bodyBytes
  // Handle both string and Buffer bodies
  if (Buffer.isBuffer(msg.body)) {
    bodyBytes = new Uint8Array(msg.body)
  } else {
    // For strings, use binary encoding to match how the multipart body is encoded
    bodyBytes = stringToBytes(msg.body, "binary")
  }

  const hashBytes = hash(bodyBytes)
  const digest = bytesToBase64(hashBytes)

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

    // Convert flat structure to nested using flat.js
    const flat = {}
    const nonFlat = {}
    for (const [key, value] of Object.entries(withBodyKeys)) {
      if (key.includes("/")) {
        flat[key] = value
      } else {
        nonFlat[key] = value
      }
    }

    if (Object.keys(flat).length > 0) {
      // Merge non-flat keys into flat for processing
      // This ensures flat_from can see existing objects like results: { "ao-types": "..." }
      const combined = { ...nonFlat, ...flat }

      // Use flat_from to convert flat structure to nested
      const nested = flat_from(combined)

      // The nested result already has everything merged
      withBodyKeys = nested
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
 * Implements bundle mode like Erlang's dev_codec_httpsig_conv:to/3 with bundle=true
 */
export function httpsig_to(tabm) {
  if (typeof tabm === "string") return tabm

  // Bundle logic: TABM → structured → TABM
  // This matches Erlang's behavior when bundle=true:
  // 1. Convert TABM to structured@1.0 (interprets ao-types, decodes to native types)
  // 2. Convert back to TABM (re-encodes with ao-types)
  const structured = structuredTo(tabm)
  const bundledTabm = structuredFrom(structured)

  // Group IDs
  const withGroupedIds = groupIds(bundledTabm)

  // Remove private and signature-related keys
  const stripped = { ...withGroupedIds }
  delete stripped.commitments
  delete stripped.signature
  delete stripped["signature-input"]
  delete stripped.priv

  const [inlineFieldHdrs, inlineKeyVal] = inlineKey(tabm)

  // Check if this is a flat structure that should stay as headers
  // A flat structure has no nested objects (maps), excluding:
  // - Arrays (JS arrays, not numbered maps)
  // - Buffers
  // Note: List-encoded maps (numbered maps with .="list") ARE nested maps
  // and should trigger multipart encoding, matching Erlang's behavior
  const hasNestedMaps = Object.values(stripped).some(value => {
    // Not an object
    if (typeof value !== "object" || value === null) return false
    // Arrays and Buffers are not nested maps
    if (Array.isArray(value) || Buffer.isBuffer(value)) return false
    // Any other object (including list-encoded maps) is a nested map
    return true
  })

  // If it's just a flat map with strings/primitives, keep as headers
  // This matches Erlang's behavior where flat maps don't become multipart
  if (!hasNestedMaps) {
    // For flat structures, just return with normalized keys
    const result = { ...inlineFieldHdrs, ...stripped }

    // Handle inline body key - move data from inline key to body
    if (inlineKeyVal && inlineKeyVal !== "body" && result[inlineKeyVal]) {
      result.body = result[inlineKeyVal]
      delete result[inlineKeyVal]
    }

    // If the only field is ao-types (no actual data), return empty object
    // This matches Erlang's behavior where ao-types-only messages become empty
    const dataKeys = Object.keys(result).filter(k =>
      k !== "ao-types" &&
      k !== "ao-ids" &&
      k !== "inline-body-key" &&
      k !== "ao-body-key"
    )
    if (dataKeys.length === 0) {
      return {}
    }

    // If there's a non-empty body, add content-digest
    // Erlang doesn't add content-digest for empty bodies (<<>>)
    if (result.body) {
      const bodyIsEmpty = Buffer.isBuffer(result.body)
        ? result.body.length === 0
        : (typeof result.body === "string" && result.body.length === 0)

      if (!bodyIsEmpty) {
        return addContentDigest(result)
      }
    }

    return result
  }

  // Original multipart logic for nested structures
  const bodyMap = {}
  const headers = { ...inlineFieldHdrs }

  // Process each field - ao-types at top level should go to headers
  for (const [key, value] of Object.entries(stripped)) {
    if (key === "ao-types") {
      // Top-level ao-types goes to headers only
      // Keep as Buffer if it's a Buffer, otherwise use as-is
      headers[key] = value
    } else if (key === "body" || key === inlineKeyVal) {
      bodyMap[key === inlineKeyVal ? inlineKeyVal : "body"] = value
    } else if (
      typeof value === "object" &&
      value !== null &&
      !Array.isArray(value) &&
      !Buffer.isBuffer(value)
    ) {
      bodyMap[key] = value
    } else if (
      typeof value === "string" &&
      value.length <= MAX_HEADER_LENGTH &&
      key !== "ao-types"
    ) {
      headers[normalizeKey(key)] = value
    } else if (
      Buffer.isBuffer(value) &&
      value.length <= MAX_HEADER_LENGTH &&
      key !== "ao-types"
    ) {
      // Keep buffers as buffers for headers
      headers[normalizeKey(key)] = value
    } else if (key !== "ao-types") {
      // Only add to bodyMap if it's not ao-types
      bodyMap[key] = value
    }
  }

  // Handle body encoding
  const groupedBodyMap = groupMaps(bodyMap)

  if (Object.keys(groupedBodyMap).length === 0) {
    return headers
  } else if (
    Object.keys(groupedBodyMap).length === 1 &&
    groupedBodyMap[inlineKeyVal] &&
    typeof groupedBodyMap[inlineKeyVal] === "string"
  ) {
    const result = { ...headers, body: groupedBodyMap[inlineKeyVal] }
    return addContentDigest(result)
  } else {
    // Multipart body
    const parts = []
    const bodyKeysList = []

    const sortedEntries = Object.entries(groupedBodyMap).sort(([a], [b]) =>
      a.localeCompare(b)
    )

    for (const [key, value] of sortedEntries) {
      if (
        typeof value === "object" &&
        value !== null &&
        Object.keys(value).length === 1 &&
        "body" in value
      ) {
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
      // Note: body-keys is NOT included in httpsig output - it's only used for parsing
      "content-type": `multipart/form-data; boundary="${boundary}"`,
      body: finalBody,
    }

    return addContentDigest(result)
  }
}

/**
 * Convert structured message to flat format
 */
export function structured_to(msg) {
  return msg
}

/**
 * Convert flat format to structured message
 */
export function structured_from(msg) {
  return msg
}
