// httpsig.js - JavaScript implementation of HTTP Signature codec

import crypto from "crypto"
import { flat_from, flat_to } from "./flat.js"

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
      // Recursively process nested objects
      newTop = groupMaps(value, flatK, newTop)
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

// Encode multipart body part
function encodeBodyPart(partName, bodyPart, inlineKey) {
  const disposition =
    partName === inlineKey ? "inline" : `form-data;name="${partName}"`
  const isInline = partName === inlineKey

  if (
    typeof bodyPart === "object" &&
    bodyPart !== null &&
    !Array.isArray(bodyPart) &&
    !Buffer.isBuffer(bodyPart)
  ) {
    // Check if this part has ao-types
    const hasAoTypes = "ao-types" in bodyPart

    if (hasAoTypes) {
      // For parts WITH ao-types: sort all entries alphabetically
      const allEntries = []

      // Collect all entries except body
      for (const [key, value] of Object.entries(bodyPart)) {
        if (key === "body") continue

        if (key === "ao-types") {
          allEntries.push({ key: "ao-types", line: `ao-types: ${value}` })
        } else {
          // Handle Buffer values properly
          let valueStr = value
          if (Buffer.isBuffer(value)) {
            // Use binary/latin1 encoding to preserve all byte values 0-255
            valueStr = value.toString("binary")
          }
          allEntries.push({ key: key, line: `${key}: ${valueStr}` })
        }
      }

      // Add content-disposition
      allEntries.push({
        key: "content-disposition",
        line: `content-disposition: ${disposition}`,
      })

      // Sort alphabetically by key
      allEntries.sort((a, b) => a.key.localeCompare(b.key))

      // Build the lines
      const lines = allEntries.map(entry => entry.line)

      // Body handling
      const body = bodyPart.body || ""
      if (body) {
        lines.push("") // Always add empty line before body
        lines.push(body)
      }

      return lines.join(CRLF)
    } else {
      // For parts WITHOUT ao-types
      const allEntries = []

      for (const [key, value] of Object.entries(bodyPart)) {
        if (key === "body") continue
        // Handle Buffer values properly
        let valueStr = value
        if (Buffer.isBuffer(value)) {
          // Use binary/latin1 encoding to preserve all byte values 0-255
          valueStr = value.toString("binary")
        }
        allEntries.push({ key: key, line: `${key}: ${valueStr}` })
      }

      const lines = []

      if (isInline) {
        // Inline parts without ao-types: sort ALL fields alphabetically including content-disposition
        allEntries.push({
          key: "content-disposition",
          line: `content-disposition: ${disposition}`,
        })

        // Sort by key
        allEntries.sort((a, b) => a.key.localeCompare(b.key))

        // Extract the lines
        lines.push(...allEntries.map(entry => entry.line))
      } else {
        // Regular parts: content-disposition first, then fields
        lines.push(`content-disposition: ${disposition}`)
        lines.push(...allEntries.map(entry => entry.line))
      }

      // Body handling
      const body = bodyPart.body || ""
      if (body) {
        lines.push("") // Always add empty line before body
        lines.push(body)
      }

      return lines.join(CRLF)
    }
  } else if (typeof bodyPart === "string" || Buffer.isBuffer(bodyPart)) {
    return `content-disposition: ${disposition}${DOUBLE_CRLF}${bodyPart}`
  }
  return ""
}

// Helper to detect if a string contains binary data
function isBinaryData(str) {
  // Check first 100 chars for binary indicators
  for (let i = 0; i < Math.min(str.length, 100); i++) {
    const code = str.charCodeAt(i)
    // Non-printable chars (except CR/LF/TAB)
    if (code < 32 && code !== 9 && code !== 10 && code !== 13) return true
    // High byte range that's not valid text
    if (code > 126 && code < 160) return true
    // Null byte is definitely binary
    if (code === 0) return true
  }
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
      // Find next line ending (but be careful with binary data)
      let lineEnd = headerBlock.indexOf("\r\n", currentPos)
      if (lineEnd === -1) lineEnd = headerBlock.indexOf("\n", currentPos)
      if (lineEnd === -1) lineEnd = headerBlock.length

      const line = headerBlock.substring(currentPos, lineEnd)
      const colonIndex = line.indexOf(": ")

      if (colonIndex > 0) {
        const name = line.substring(0, colonIndex).toLowerCase()
        let value = line.substring(colonIndex + 2)

        // Special handling for known binary fields or detected binary data
        if (name === "owner" || name === "signature") {
          // These fields contain binary data that may have embedded newlines
          // We need to read until we find the next header or end of headers
          let valueStart = currentPos + colonIndex + 2

          // Look ahead to find where this field really ends
          // The next header will start with a valid header name followed by ": "
          let searchPos = valueStart
          let valueEnd = headerBlock.length

          // Look for the next valid header pattern
          while (searchPos < headerBlock.length) {
            let nextNewline = headerBlock.indexOf("\n", searchPos)
            if (nextNewline === -1) break

            // Check if what follows looks like a header
            let nextLineStart = nextNewline + 1
            let nextColon = headerBlock.indexOf(":", nextLineStart)

            // Valid header should have colon relatively close to line start
            if (nextColon > nextLineStart && nextColon < nextLineStart + 50) {
              // Check if the text before colon looks like a header name (ASCII text)
              let possibleHeaderName = headerBlock.substring(
                nextLineStart,
                nextColon
              )
              let looksLikeHeader = /^[a-zA-Z0-9-]+$/.test(possibleHeaderName)

              if (looksLikeHeader) {
                // Found the next header, value ends at the newline before it
                valueEnd = nextNewline
                break
              }
            }
            searchPos = nextNewline + 1
          }

          // Extract the full value, trimming any trailing whitespace
          value = headerBlock.substring(valueStart, valueEnd)

          // Remove trailing CR if present (since we found the LF)
          if (value.endsWith("\r")) {
            value = value.substring(0, value.length - 1)
          }

          // Convert to Buffer to preserve binary
          headers[name] = Buffer.from(value, "binary")
          currentPos = valueEnd + 1
        } else {
          // Regular text field
          if (isBinaryData(value)) {
            headers[name] = Buffer.from(value, "binary")
          } else {
            headers[name] = value
          }
          currentPos = lineEnd + (headerBlock[lineEnd] === "\r" ? 2 : 1)
        }
      } else {
        // No colon found, skip this line
        currentPos = lineEnd + (headerBlock[lineEnd] === "\r" ? 2 : 1)
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
        result[partName] = partBody
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
        result[partName] = partBody
      } else if (!partBody) {
        result[partName] = restHeaders
      } else {
        result[partName] = { ...restHeaders, body: partBody }
      }
    }
  }

  if (bodyKeysList.length > 0) {
    result["body-keys"] = bodyKeysList.map(k => `"${k}"`).join(", ")
  }

  return result
}

// Add content-digest header
function addContentDigest(msg) {
  if (!msg.body) return msg

  const hash = crypto.createHash("sha256")

  // Handle both string and Buffer bodies
  if (Buffer.isBuffer(msg.body)) {
    hash.update(msg.body)
  } else {
    // For strings, use binary encoding to match how the multipart body is encoded
    hash.update(msg.body, "binary")
  }

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

    // Convert flat structure to nested using flat.js
    const flat = {}
    for (const [key, value] of Object.entries(withBodyKeys)) {
      if (key.includes("/")) {
        flat[key] = value
      }
    }

    if (Object.keys(flat).length > 0) {
      // Use flat_from to convert flat structure to nested
      const nested = flat_from(flat)
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

  // Check if this is a flat structure that should stay as headers
  // A flat structure has no nested objects (maps)
  const hasNestedMaps = Object.values(stripped).some(
    value =>
      typeof value === "object" &&
      value !== null &&
      !Array.isArray(value) &&
      !Buffer.isBuffer(value)
  )

  // If it's just a flat map with strings/primitives, keep as headers
  // This matches Erlang's behavior where flat maps don't become multipart
  if (!hasNestedMaps) {
    // For flat structures, just return with normalized keys
    // This matches Erlang which returns the map unchanged
    const result = { ...inlineFieldHdrs }

    for (const [key, value] of Object.entries(stripped)) {
      // Convert Buffers to strings if they're UTF-8 text
      if (Buffer.isBuffer(value)) {
        try {
          const str = value.toString("utf8")
          // Check if it's valid UTF-8 that can be safely converted
          if (Buffer.from(str, "utf8").equals(value)) {
            // Check if all characters are printable
            let isPrintable = true
            for (let i = 0; i < str.length; i++) {
              const code = str.charCodeAt(i)
              if (
                !(code >= 32 && code <= 126) &&
                code !== 9 &&
                code !== 10 &&
                code !== 13
              ) {
                isPrintable = false
                break
              }
            }
            if (isPrintable) {
              result[key] = str
            } else {
              result[key] = value
            }
          } else {
            result[key] = value
          }
        } catch (e) {
          result[key] = value
        }
      } else {
        result[key] = value
      }
    }

    // Handle inline body key - move data from inline key to body
    if (inlineKeyVal && inlineKeyVal !== "body" && result[inlineKeyVal]) {
      result.body = result[inlineKeyVal]
      delete result[inlineKeyVal]
    }

    // If there's a body, add content-digest
    if (result.body) {
      return addContentDigest(result)
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
      // Convert Buffer to string if needed
      if (Buffer.isBuffer(value)) {
        headers[key] = value.toString("utf8")
      } else {
        headers[key] = value
      }
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
      // Convert Buffers to strings for headers
      const str = value.toString("utf8")
      headers[normalizeKey(key)] = str
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
      "body-keys": bodyKeysList.map(k => `"${k}"`).join(", "),
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
