import base64url from "base64url"
import { hash } from "fast-sha256"

function isBytes(value) {
  return (
    value instanceof ArrayBuffer ||
    ArrayBuffer.isView(value) ||
    Buffer.isBuffer(value) ||
    (value &&
      typeof value === "object" &&
      value.type === "Buffer" &&
      Array.isArray(value.data))
  )
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
  let uint8Array
  if (data instanceof ArrayBuffer) {
    uint8Array = new Uint8Array(data)
  } else if (data instanceof Uint8Array) {
    uint8Array = data
  } else if (ArrayBuffer.isView(data)) {
    uint8Array = new Uint8Array(data.buffer, data.byteOffset, data.byteLength)
  } else {
    throw new Error("sha256 expects ArrayBuffer or ArrayBufferView")
  }

  const hashResult = hash(uint8Array)
  return hashResult.buffer.slice(
    hashResult.byteOffset,
    hashResult.byteOffset + hashResult.byteLength
  )
}

function formatFloat(num) {
  // Format float in scientific notation with proper padding
  let exp = num.toExponential(20)
  // Replace "1.23e+0" with "1.23e+00"
  exp = exp.replace(/e\+(\d)$/, "e+0$1")
  exp = exp.replace(/e-(\d)$/, "e-0$1")
  return exp
}

function encodeArrayItem(item) {
  if (typeof item === "number") {
    if (Number.isInteger(item)) {
      return `"(ao-type-integer) ${item}"`
    } else {
      return `"(ao-type-float) ${formatFloat(item)}"`
    }
  } else if (typeof item === "string") {
    return `"${item}"`
  } else if (item === null) {
    return `"(ao-type-atom) \\"null\\""`
  } else if (item === undefined) {
    return `"(ao-type-atom) \\"undefined\\""`
  } else if (typeof item === "symbol") {
    const desc = item.description || "Symbol.for()"
    return `"(ao-type-atom) \\"${desc}\\""`
  } else if (typeof item === "boolean") {
    return `"(ao-type-atom) \\"${item}\\""`
  } else if (Array.isArray(item)) {
    // Nested array
    const nestedItems = item
      .map(nestedItem => {
        if (typeof nestedItem === "number") {
          if (Number.isInteger(nestedItem)) {
            return `\\"(ao-type-integer) ${nestedItem}\\"`
          } else {
            return `\\"(ao-type-float) ${formatFloat(nestedItem)}\\"`
          }
        } else if (typeof nestedItem === "string") {
          return `\\"${nestedItem}\\"`
        } else if (nestedItem === null) {
          return `\\"(ao-type-atom) \\\\\\"null\\\\\\"\\"`
        } else if (typeof nestedItem === "symbol") {
          const desc = nestedItem.description || "Symbol.for()"
          return `\\"(ao-type-atom) \\\\\\"${desc}\\\\\\"\\"`
        } else {
          return `\\"${String(nestedItem)}\\"`
        }
      })
      .join(", ")
    return `"(ao-type-list) ${nestedItems}"`
  } else if (isBytes(item)) {
    // For empty binaries in arrays, return empty string
    const buffer = toBuffer(item)
    if (buffer.length === 0 || buffer.byteLength === 0) {
      return `""`
    }
    // For non-empty binaries, we can't include them in headers
    return `"(ao-type-binary)"`
  } else if (isPojo(item)) {
    const json = JSON.stringify(item)
    const escaped = json.replace(/\\/g, "\\\\").replace(/"/g, '\\"')
    return `"(ao-type-map) ${escaped}"`
  } else {
    return `"${String(item)}"`
  }
}

function needsOwnBodyPart(value) {
  if (Array.isArray(value)) return true
  if (isBytes(value)) return true
  if (isPojo(value)) {
    // Check if object has complex fields
    return Object.values(value).some(
      v =>
        Array.isArray(v) ||
        isPojo(v) ||
        isBytes(v) ||
        v === null ||
        v === undefined ||
        typeof v === "symbol"
    )
  }
  return false
}

function collectBodyKeys(obj, prefix = "") {
  const keys = []

  function traverse(current, path) {
    // Track if current level has simple fields or empty objects
    let hasSimpleFields = false
    // Track nested paths that need body parts
    const nestedPaths = []

    for (const [key, value] of Object.entries(current)) {
      const fullPath = path ? `${path}/${key}` : key

      if (Array.isArray(value)) {
        const hasObjects = value.some(item => isPojo(item))
        const hasNonObjects = value.some(item => !isPojo(item))

        if (hasObjects) {
          // Each object in array gets its own key
          value.forEach((item, index) => {
            if (isPojo(item)) {
              nestedPaths.push(`${fullPath}/${index + 1}`)
            }
          })

          // If array ALSO has non-object items, it needs its own body part
          if (hasNonObjects) {
            hasSimpleFields = true
          }
        } else {
          // Simple array - parent needs body part
          hasSimpleFields = true
        }
      } else if (isPojo(value)) {
        // Check if this is an empty object
        if (Object.keys(value).length === 0) {
          // Empty objects need a body part
          hasSimpleFields = true
        } else {
          // Non-empty objects are processed recursively
          nestedPaths.push(fullPath)
        }
      } else if (isBytes(value)) {
        hasSimpleFields = true
      } else if (
        typeof value === "string" ||
        typeof value === "number" ||
        typeof value === "boolean" ||
        value === null ||
        value === undefined ||
        typeof value === "symbol"
      ) {
        hasSimpleFields = true
      }
    }

    // Add current path if it has simple fields or empty objects
    if (hasSimpleFields) {
      keys.push(path)
    }

    // Process nested paths
    for (const nestedPath of nestedPaths) {
      const parts = nestedPath.split("/")
      let nestedObj = obj

      for (const part of parts) {
        if (/^\d+$/.test(part)) {
          nestedObj = nestedObj[parseInt(part) - 1]
        } else {
          nestedObj = nestedObj[part]
        }
      }

      if (isPojo(nestedObj)) {
        traverse(nestedObj, nestedPath)
      }
    }
  }

  // Handle top-level fields
  for (const [key, value] of Object.entries(obj)) {
    if (Array.isArray(value)) {
      const hasObjects = value.some(item => isPojo(item))
      const hasArrays = value.some(item => Array.isArray(item))
      const hasNonObjects = value.some(item => !isPojo(item))

      if (hasObjects) {
        value.forEach((item, index) => {
          if (isPojo(item)) {
            keys.push(`${key}/${index + 1}`)
            // Also need to traverse into nested objects within array items
            for (const [nestedKey, nestedValue] of Object.entries(item)) {
              if (isPojo(nestedValue)) {
                keys.push(`${key}/${index + 1}/${nestedKey}`)
              }
            }
          }
        })

        // Mixed arrays also need their own body part
        if (hasNonObjects) {
          keys.push(key)
        }
      } else if (hasArrays) {
        // Array containing arrays needs body part
        keys.push(key)
      } else {
        // Simple array at top level - DO NOT add to body keys
        // It will go in headers instead
      }
    } else if (isPojo(value)) {
      // Top-level object that may have nested structures
      traverse(value, key)
    } else if (isBytes(value)) {
      // All binary data needs body parts, even empty ones
      keys.push(key)
    } else if (typeof value === "string" && value.includes("\n")) {
      // Multiline string
      keys.push(key)
    }
  }

  return [...new Set(keys)].filter(k => k !== "")
}

function toBuffer(value) {
  if (Buffer.isBuffer(value)) {
    return value
  } else if (
    value &&
    typeof value === "object" &&
    value.type === "Buffer" &&
    Array.isArray(value.data)
  ) {
    return Buffer.from(value.data)
  } else if (value instanceof ArrayBuffer || ArrayBuffer.isView(value)) {
    return Buffer.from(value)
  } else {
    return Buffer.from(value)
  }
}

async function encode(obj = {}) {
  // Convert symbols to strings for logging
  const processValue = value => {
    if (typeof value === "symbol") {
      return value.description || "Symbol.for()"
    } else if (Array.isArray(value)) {
      return value.map(processValue)
    } else if (isPojo(value)) {
      const result = {}
      for (const [k, v] of Object.entries(value)) {
        result[k] = processValue(v)
      }
      return result
    }
    return value
  }

  const processedObj = {}
  for (const [k, v] of Object.entries(obj)) {
    processedObj[k] = processValue(v)
  }

  // Remove debug logging for cleaner output
  console.log("[encode] START with obj:", JSON.stringify(processedObj))

  if (Object.keys(obj).length === 0) {
    return { headers: {}, body: undefined }
  }

  // Check for special case: body field with binary + other simple fields or empty binaries
  const hasBodyBinary = obj.body && isBytes(obj.body)
  const otherFields = Object.keys(obj).filter(k => k !== "body")
  const allOthersSimpleOrEmptyBinary = otherFields.every(k => {
    const v = obj[k]
    // Allow empty binaries as "simple"
    if (isBytes(v) && (v.length === 0 || v.byteLength === 0)) return true
    return (
      !isBytes(v) &&
      !isPojo(v) &&
      !(Array.isArray(v) && v.some(item => isPojo(item) || isBytes(item)))
    )
  })

  if (hasBodyBinary && allOthersSimpleOrEmptyBinary) {
    console.log("[encode] Special case: body with binary + simple fields")
    // Special case: body with binary + other simple fields
    const headers = {}
    const headerTypes = []

    // Process other fields into headers
    for (const [key, value] of Object.entries(obj)) {
      if (key === "body") continue
      console.log(
        `[encode] Processing special case field: ${key} = ${JSON.stringify(value)}`
      )

      if (value === null) {
        headers[key] = '"null"'
        headerTypes.push(`${key}="atom"`)
      } else if (value === undefined) {
        headers[key] = '"undefined"'
        headerTypes.push(`${key}="atom"`)
      } else if (typeof value === "boolean") {
        headers[key] = `"${value}"`
        headerTypes.push(`${key}="atom"`)
      } else if (typeof value === "symbol") {
        headers[key] = `"${value.description || "Symbol.for()"}"`
        headerTypes.push(`${key}="atom"`)
      } else if (typeof value === "number") {
        headers[key] = String(value)
        headerTypes.push(
          `${key}="${Number.isInteger(value) ? "integer" : "float"}"`
        )
      } else if (typeof value === "string") {
        if (value.length === 0) {
          // Empty strings only go in ao-types, not as headers
          console.log(`[encode] Adding empty string type for key: ${key}`)
          headerTypes.push(`${key}="empty-binary"`)
        } else {
          headers[key] = value
        }
      } else if (Array.isArray(value) && value.length === 0) {
        // Empty array only goes in ao-types, not as a header
        headerTypes.push(`${key}="empty-list"`)
      } else if (Array.isArray(value) && !value.some(item => isPojo(item))) {
        const encodedItems = value.map(item => encodeArrayItem(item)).join(", ")
        headers[key] = encodedItems
        headerTypes.push(`${key}="list"`)
      } else if (
        isBytes(value) &&
        (value.length === 0 || value.byteLength === 0)
      ) {
        // Empty binary goes in ao-types only
        headerTypes.push(`${key}="empty-binary"`)
      }
    }

    // Add ao-types if needed
    if (headerTypes.length > 0) {
      headers["ao-types"] = headerTypes.sort().join(", ")
    }

    // Set body to binary
    const bodyBuffer = toBuffer(obj.body)
    const bodyArrayBuffer = bodyBuffer.buffer.slice(
      bodyBuffer.byteOffset,
      bodyBuffer.byteOffset + bodyBuffer.byteLength
    )

    const contentDigest = await sha256(bodyArrayBuffer)
    const base64 = base64url.toBase64(base64url.encode(contentDigest))
    headers["content-digest"] = `sha-256=:${base64}:`

    console.log(
      "[encode] FINAL (body with binary) - headers:",
      headers,
      "body:",
      obj.body
    )
    return { headers, body: obj.body }
  }

  // Check if single binary field
  const objKeys = Object.keys(obj)
  if (objKeys.length === 1 && isBytes(obj[objKeys[0]])) {
    const fieldName = objKeys[0]
    const binaryData = obj[fieldName]

    const headers = {}
    const bodyBuffer = toBuffer(binaryData)
    const bodyArrayBuffer = bodyBuffer.buffer.slice(
      bodyBuffer.byteOffset,
      bodyBuffer.byteOffset + bodyBuffer.byteLength
    )

    const contentDigest = await sha256(bodyArrayBuffer)
    const base64 = base64url.toBase64(base64url.encode(contentDigest))
    headers["content-digest"] = `sha-256=:${base64}:`

    // Add inline-body-key header to preserve the field name
    if (fieldName !== "body") {
      headers["inline-body-key"] = fieldName
    }

    console.log(
      "[encode] FINAL (simple binary field) - headers:",
      headers,
      "body:",
      binaryData
    )
    return { headers, body: binaryData }
  }

  // Continue with normal multipart processing
  const headers = {}
  const headerTypes = []

  // Collect all body keys
  const bodyKeys = collectBodyKeys(obj)

  // Process simple header fields AND collect types for body fields
  for (const [key, value] of Object.entries(obj)) {
    console.log(
      `[encode] Processing field: ${key} = ${JSON.stringify(value)}, type: ${typeof value}`
    )
    const needsBody =
      bodyKeys.includes(key) || bodyKeys.some(k => k.startsWith(`${key}/`))

    if (!needsBody) {
      console.log(`[encode] Field ${key} doesn't need body, adding to headers`)
      // Simple value goes in header
      if (value === null) {
        headers[key] = '"null"'
        headerTypes.push(`${key}="atom"`)
      } else if (value === undefined) {
        headers[key] = '"undefined"'
        headerTypes.push(`${key}="atom"`)
      } else if (typeof value === "boolean") {
        headers[key] = `"${value}"`
        headerTypes.push(`${key}="atom"`)
      } else if (typeof value === "symbol") {
        headers[key] = `"${value.description || "Symbol.for()"}"`
        headerTypes.push(`${key}="atom"`)
      } else if (typeof value === "number") {
        headers[key] = String(value)
        headerTypes.push(
          `${key}="${Number.isInteger(value) ? "integer" : "float"}"`
        )
      } else if (typeof value === "string") {
        if (value.length === 0) {
          headerTypes.push(`${key}="empty-binary"`)
          // Don't add empty strings as headers
        } else {
          headers[key] = value
        }
      } else if (Array.isArray(value) && !value.some(item => isPojo(item))) {
        // Simple array (no objects) goes in header
        const encodedItems = value.map(item => encodeArrayItem(item)).join(", ")
        headers[key] = encodedItems
        headerTypes.push(`${key}="list"`)
      }
    } else {
      // Field needs body - still need to add type info to ao-types
      if (isBytes(value) && (value.length === 0 || value.byteLength === 0)) {
        headerTypes.push(`${key}="empty-binary"`)
      } else if (typeof value === "string" && value.length === 0) {
        headerTypes.push(`${key}="empty-binary"`)
      } else if (Array.isArray(value) && value.length === 0) {
        headerTypes.push(`${key}="empty-list"`)
      } else if (isPojo(value) && Object.keys(value).length === 0) {
        headerTypes.push(`${key}="empty-message"`)
      }
    }
  }

  // Add ao-types for arrays that go in body
  for (const [key, value] of Object.entries(obj)) {
    if (Array.isArray(value)) {
      // Check if this array goes in the body
      if (
        bodyKeys.includes(key) ||
        bodyKeys.some(k => k.startsWith(`${key}/`))
      ) {
        // Don't add list type if it's already been added
        if (!headerTypes.some(t => t.startsWith(`${key}=`))) {
          headerTypes.push(`${key}="list"`)
        }
      }
    }
  }

  // If no body needed
  if (bodyKeys.length === 0) {
    if (headerTypes.length > 0) {
      headers["ao-types"] = headerTypes.sort().join(", ")
    }
    console.log("[encode] FINAL - headers:", headers, "body:", undefined)
    return { headers, body: undefined }
  }

  // Check if all body keys are for empty binaries - if so, treat as no body needed
  const allBodyKeysAreEmptyBinaries = bodyKeys.every(key => {
    const pathParts = key.split("/")
    let value = obj
    for (const part of pathParts) {
      if (/^\d+$/.test(part)) {
        value = value[parseInt(part) - 1]
      } else {
        value = value[part]
      }
    }
    return isBytes(value) && (value.length === 0 || value.byteLength === 0)
  })

  if (allBodyKeysAreEmptyBinaries) {
    // Treat as header-only encoding
    if (headerTypes.length > 0) {
      headers["ao-types"] = headerTypes.sort().join(", ")
    }
    console.log(
      "[encode] FINAL (all empty binaries) - headers:",
      headers,
      "body:",
      undefined
    )
    return { headers, body: undefined }
  }

  // Sort body keys and add to headers
  const sortedBodyKeys = bodyKeys.sort((a, b) => {
    // Special sorting to ensure parent paths come before child paths
    if (a.startsWith(b + "/")) return 1
    if (b.startsWith(a + "/")) return -1
    return a.localeCompare(b)
  })

  // Special case: if we have both 'data' and 'body' keys where data.body is binary
  // then we need special handling per Erlang behavior
  const hasSpecialDataBody =
    sortedBodyKeys.includes("data") &&
    sortedBodyKeys.includes("body") &&
    obj.data &&
    obj.data.body &&
    isBytes(obj.data.body) &&
    obj.body &&
    obj.body.data &&
    isBytes(obj.body.data)

  headers["body-keys"] = sortedBodyKeys.map(k => `"${k}"`).join(", ")

  // Check for inline keys - but not in the special data/body case
  if (!hasSpecialDataBody) {
    const inlineKey = headers["inline-body-key"]
    if (!inlineKey) {
      // Only set inline-body-key if we have ONLY body (not data)
      if (sortedBodyKeys.includes("body") && !sortedBodyKeys.includes("data")) {
        headers["inline-body-key"] = "body"
      }
    }
  }

  // Add ao-types header if needed
  if (headerTypes.length > 0) {
    headers["ao-types"] = headerTypes.sort().join(", ")
  }

  // Create multipart body parts
  const bodyParts = []

  for (const bodyKey of sortedBodyKeys) {
    const lines = []

    // Parse the path to get to the value
    const pathParts = bodyKey.split("/")
    let value = obj
    let parent = null

    // Get the actual value at this path
    for (let i = 0; i < pathParts.length; i++) {
      parent = value
      const part = pathParts[i]

      if (/^\d+$/.test(part)) {
        value = value[parseInt(part) - 1]
      } else {
        value = value[part]
      }
    }

    console.log(
      "[encode] Processing body key:",
      bodyKey,
      "value type:",
      Array.isArray(value) ? "array" : typeof value
    )

    // Skip if value is an array with only objects (no content for this body part)
    if (Array.isArray(value) && value.every(item => isPojo(item))) {
      continue
    }

    // Skip if this is an empty object
    if (isPojo(value) && Object.keys(value).length === 0) {
      continue
    }

    // Special handling for the data/body pattern
    if (
      hasSpecialDataBody &&
      bodyKey === "data" &&
      isPojo(value) &&
      Object.keys(value).length === 1 &&
      value.body &&
      isBytes(value.body)
    ) {
      // Skip creating inline content for 'data', will handle data/body separately
      continue
    }

    console.log(
      "[encode] Creating body part for key:",
      bodyKey,
      "value type:",
      typeof value,
      "isBytes:",
      isBytes(value)
    )

    // Determine content-disposition
    const isInline = bodyKey === "body" && headers["inline-body-key"] === "body"
    if (isInline) {
      lines.push(`content-disposition: inline`)
    } else {
      lines.push(`content-disposition: form-data;name="${bodyKey}"`)
    }

    console.log("[encode] Value type checks:", {
      isBytes: isBytes(value),
      isPojo: isPojo(value),
      isArray: Array.isArray(value),
      valueType: typeof value,
    })

    if (isBytes(value)) {
      console.log("[encode] Processing binary value for key:", bodyKey)
      // Binary data
      const buffer = toBuffer(value)

      // Check if this is a nested path like "data/body"
      if (bodyKey.includes("/")) {
        // For nested binary, we need to replace the disposition
        lines[lines.length - 1] =
          `content-disposition: form-data;name="${bodyKey}"`
        lines.push("") // Empty line
        lines.push("") // Another empty line before binary
        const textPart = lines.join("\r\n")
        bodyParts.push(new Blob([textPart, buffer]))
      } else {
        lines.push("") // Empty line after headers
        lines.push("") // Another empty line before binary data
        const textPart = lines.join("\r\n")
        bodyParts.push(new Blob([textPart, buffer]))
      }
    } else if (isPojo(value)) {
      console.log("[encode] Processing object value")
      // Object - only include fields that aren't handled by nested body parts
      const objectTypes = []
      const fieldLines = []
      const binaryFields = []

      for (const [k, v] of Object.entries(value)) {
        const childPath = `${bodyKey}/${k}`

        // Skip if this field has its own body part
        if (sortedBodyKeys.includes(childPath)) {
          continue
        }

        // Skip if this is an array of objects (handled separately)
        if (Array.isArray(v) && v.some(item => isPojo(item))) {
          continue
        }

        // Add type info
        if (Array.isArray(v)) {
          objectTypes.push(`${k}="${v.length === 0 ? "empty-list" : "list"}"`)
        } else if (
          v === null ||
          v === undefined ||
          typeof v === "symbol" ||
          typeof v === "boolean"
        ) {
          objectTypes.push(`${k}="atom"`)
        } else if (typeof v === "number") {
          objectTypes.push(
            `${k}="${Number.isInteger(v) ? "integer" : "float"}"`
          )
        } else if (typeof v === "string" && v.length === 0) {
          objectTypes.push(`${k}="empty-binary"`)
        } else if (isBytes(v) && (v.length === 0 || v.byteLength === 0)) {
          objectTypes.push(`${k}="empty-binary"`)
        } else if (isPojo(v) && Object.keys(v).length === 0) {
          objectTypes.push(`${k}="empty-message"`)
        }

        // Add field value
        if (typeof v === "string") {
          fieldLines.push(`${k}: ${v}`)
        } else if (typeof v === "number") {
          fieldLines.push(`${k}: ${v}`)
        } else if (typeof v === "boolean") {
          fieldLines.push(`${k}: "${v}"`)
        } else if (v === null) {
          fieldLines.push(`${k}: "null"`)
        } else if (v === undefined) {
          fieldLines.push(`${k}: "undefined"`)
        } else if (typeof v === "symbol") {
          fieldLines.push(`${k}: "${v.description || "Symbol.for()"}"`)
        } else if (isBytes(v)) {
          const buffer = toBuffer(v)
          // For inline data/body parts, binary fields get raw bytes
          if (isInline) {
            // Skip here - will be handled specially
            continue
          } else {
            // For non-inline parts, we need to add raw bytes, not base64
            // Store the binary field for later processing
            binaryFields.push({ key: k, buffer })
            continue
          }
        } else if (Array.isArray(v)) {
          if (v.length === 0) {
            fieldLines.push(`${k}: `)
          } else {
            const encodedItems = v.map(item => encodeArrayItem(item)).join(", ")
            fieldLines.push(`${k}: ${encodedItems}`)
          }
        } else if (isPojo(v) && Object.keys(v).length === 0) {
          // Empty object - no content line needed, just ao-type
        }
      }

      // Check if this object only has empty collections
      const onlyEmptyCollections = Object.entries(value).every(([k, v]) => {
        const childPath = `${bodyKey}/${k}`
        if (sortedBodyKeys.includes(childPath)) return true
        if (Array.isArray(v) && v.some(item => isPojo(item))) return true

        return (
          (Array.isArray(v) && v.length === 0) ||
          (isPojo(v) && Object.keys(v).length === 0) ||
          (isBytes(v) && (v.length === 0 || v.byteLength === 0))
        )
      })

      // Special handling for inline body
      if (isInline) {
        // For inline: fields first, then ao-types, then content-disposition
        const orderedLines = []

        // First: field lines
        if (!onlyEmptyCollections) {
          for (const line of fieldLines) {
            orderedLines.push(line)
          }
        }

        // Then: ao-types
        if (objectTypes.length > 0) {
          orderedLines.push(`ao-types: ${objectTypes.sort().join(", ")}`)
        }

        // Finally: content-disposition
        orderedLines.push("content-disposition: inline")

        // Check if this has binary fields
        const binaryFields = Object.entries(value)
          .filter(
            ([k, v]) =>
              isBytes(v) && !sortedBodyKeys.includes(`${bodyKey}/${k}`)
          )
          .map(([k, v]) => ({
            key: k,
            buffer: toBuffer(v),
          }))

        if (binaryFields.length > 0) {
          // Build the parts
          const parts = []

          // Add the text part
          parts.push(Buffer.from(orderedLines.join("\r\n")))

          // Add binary fields with raw bytes
          for (const { key, buffer } of binaryFields) {
            parts.push(Buffer.from(`\r\n${key}: `))
            parts.push(buffer)
          }

          // Add trailing \r\n for inline parts with binary
          parts.push(Buffer.from("\r\n"))

          const fullBody = Buffer.concat(parts)
          bodyParts.push(new Blob([fullBody]))
        } else {
          orderedLines.push("") // Add empty line for trailing \r\n
          bodyParts.push(new Blob([orderedLines.join("\r\n")]))
        }
      } else {
        // Normal handling (non-inline)
        // ao-types first if needed
        if (objectTypes.length > 0) {
          lines.unshift(`ao-types: ${objectTypes.sort().join(", ")}`)
        }

        // Only add field lines if not all collections are empty
        if (!onlyEmptyCollections) {
          for (const line of fieldLines) {
            lines.push(line)
          }
        }

        // Then handle binary fields with raw bytes if any
        if (binaryFields && binaryFields.length > 0) {
          // Create parts array for proper ordering
          const parts = []

          // Add headers and text fields
          const headerText = lines.join("\r\n") + "\r\n"
          parts.push(Buffer.from(headerText))

          // Add binary fields with raw bytes
          for (const { key, buffer } of binaryFields) {
            parts.push(Buffer.from(`${key}: `))
            parts.push(buffer)
            parts.push(Buffer.from("\r\n"))
          }

          const fullBody = Buffer.concat(parts)
          bodyParts.push(new Blob([fullBody]))
        } else {
          lines.push("")
          bodyParts.push(new Blob([lines.join("\r\n")]))
        }
      }
    } else if (Array.isArray(value)) {
      // Array field - check if it's a mixed array or array of arrays
      const hasObjects = value.some(item => isPojo(item))
      const hasArrays = value.some(item => Array.isArray(item))
      const nonObjectItems = value
        .map((item, index) => ({ item, index: index + 1 }))
        .filter(({ item }) => !isPojo(item))

      if (hasObjects && nonObjectItems.length > 0) {
        // Mixed array - only include non-object items
        const fieldLines = []
        const partTypes = []

        for (const { item, index } of nonObjectItems) {
          if (typeof item === "number") {
            if (Number.isInteger(item)) {
              partTypes.push(`${index}="integer"`)
              fieldLines.push(`${index}: ${item}`)
            } else {
              partTypes.push(`${index}="float"`)
              fieldLines.push(`${index}: ${formatFloat(item)}`)
            }
          } else if (typeof item === "string") {
            fieldLines.push(`${index}: ${item}`)
          } else if (
            item === null ||
            item === undefined ||
            typeof item === "symbol" ||
            typeof item === "boolean"
          ) {
            partTypes.push(`${index}="atom"`)
            if (item === null) {
              fieldLines.push(`${index}: "null"`)
            } else if (item === undefined) {
              fieldLines.push(`${index}: "undefined"`)
            } else if (typeof item === "symbol") {
              fieldLines.push(
                `${index}: "${item.description || "Symbol.for()"}"`
              )
            } else {
              fieldLines.push(`${index}: "${item}"`)
            }
          } else if (isBytes(item)) {
            // Binary items in arrays need special handling
            const buffer = toBuffer(item)
            if (buffer.length === 0) {
              partTypes.push(`${index}="empty-binary"`)
            }
            // For now, skip binary items in mixed arrays
            // They should be handled differently
            partTypes.push(`${index}="binary"`)
          } else if (Array.isArray(item)) {
            partTypes.push(`${index}="list"`)
            const encodedItems = item
              .map(subItem => encodeArrayItem(subItem))
              .join(", ")
            fieldLines.push(`${index}: ${encodedItems}`)
          }
        }

        // For inline arrays, use different order
        if (isInline) {
          console.log("[encode] Reordering for inline array:", {
            bodyKey,
            fieldLines,
            partTypes,
          })

          // Rebuild in correct order: field lines, ao-types, content-disposition
          const orderedLines = []

          // First: field lines
          for (const line of fieldLines) {
            orderedLines.push(line)
          }

          // Then: ao-types
          if (partTypes.length > 0) {
            orderedLines.push(
              `ao-types: ${partTypes
                .sort((a, b) => {
                  const aNum = parseInt(a.split("=")[0])
                  const bNum = parseInt(b.split("=")[0])
                  return aNum - bNum
                })
                .join(", ")}`
            )
          }

          // Finally: content-disposition (from lines[0])
          orderedLines.push(lines[0])
          orderedLines.push("")

          console.log("[encode] Ordered lines:", orderedLines)

          bodyParts.push(new Blob([orderedLines.join("\r\n")]))
        } else {
          // Normal order for non-inline parts
          if (partTypes.length > 0) {
            lines.unshift(
              `ao-types: ${partTypes
                .sort((a, b) => {
                  const aNum = parseInt(a.split("=")[0])
                  const bNum = parseInt(b.split("=")[0])
                  return aNum - bNum
                })
                .join(", ")}`
            )
          }

          for (const line of fieldLines) {
            lines.push(line)
          }

          lines.push("")
          bodyParts.push(new Blob([lines.join("\r\n")]))
        }
      } else if (hasArrays || (!hasObjects && value.length > 0)) {
        // Array of arrays or simple array - use indexed format
        const fieldLines = []
        const partTypes = []

        value.forEach((item, idx) => {
          const index = idx + 1
          if (Array.isArray(item)) {
            if (item.length === 0) {
              partTypes.push(`${index}="empty-list"`)
            } else {
              partTypes.push(`${index}="list"`)
              const encodedItems = item
                .map(subItem => encodeArrayItem(subItem))
                .join(", ")
              fieldLines.push(`${index}: ${encodedItems}`)
            }
          } else if (typeof item === "number") {
            if (Number.isInteger(item)) {
              partTypes.push(`${index}="integer"`)
              fieldLines.push(`${index}: ${item}`)
            } else {
              partTypes.push(`${index}="float"`)
              fieldLines.push(`${index}: ${formatFloat(item)}`)
            }
          } else if (typeof item === "string") {
            if (item.length === 0) {
              partTypes.push(`${index}="empty-binary"`)
            }
            fieldLines.push(`${index}: ${item}`)
          } else if (
            item === null ||
            item === undefined ||
            typeof item === "symbol" ||
            typeof item === "boolean"
          ) {
            partTypes.push(`${index}="atom"`)
            if (item === null) {
              fieldLines.push(`${index}: "null"`)
            } else if (item === undefined) {
              fieldLines.push(`${index}: "undefined"`)
            } else if (typeof item === "symbol") {
              fieldLines.push(
                `${index}: "${item.description || "Symbol.for()"}"`
              )
            } else {
              fieldLines.push(`${index}: "${item}"`)
            }
          } else if (isBytes(item)) {
            const buffer = toBuffer(item)
            if (buffer.length === 0) {
              partTypes.push(`${index}="empty-binary"`)
            } else {
              partTypes.push(`${index}="binary"`)
            }
            // For indexed format, we also can't include raw bytes inline
            // This is a limitation of the format
          }
        })

        // For inline arrays, use different order
        if (isInline) {
          console.log("[encode] Reordering for inline array - indexed format")

          const orderedLines = []

          // First: field lines
          for (const line of fieldLines) {
            orderedLines.push(line)
          }

          // Then: ao-types
          if (partTypes.length > 0) {
            orderedLines.push(
              `ao-types: ${partTypes
                .sort((a, b) => {
                  const aNum = parseInt(a.split("=")[0])
                  const bNum = parseInt(b.split("=")[0])
                  return aNum - bNum
                })
                .join(", ")}`
            )
          }

          // Finally: content-disposition
          orderedLines.push(lines[0])
          orderedLines.push("")

          console.log("[encode] Final ordered lines:", orderedLines)
          bodyParts.push(new Blob([orderedLines.join("\r\n")]))
        } else {
          // Normal order for non-inline parts
          if (partTypes.length > 0) {
            console.log("[encode] Adding ao-types to beginning of lines array")
            lines.unshift(
              `ao-types: ${partTypes
                .sort((a, b) => {
                  const aNum = parseInt(a.split("=")[0])
                  const bNum = parseInt(b.split("=")[0])
                  return aNum - bNum
                })
                .join(", ")}`
            )
          }

          console.log("[encode] Adding field lines:", fieldLines)
          for (const line of fieldLines) {
            lines.push(line)
          }

          console.log("[encode] Final lines before blob:", lines)
          lines.push("")
          bodyParts.push(new Blob([lines.join("\r\n")]))
        }
      } else if (!hasObjects && value.length === 0) {
        // Empty array
        const fieldName = pathParts[pathParts.length - 1]
        const partTypes = [`${fieldName}="empty-list"`]
        lines.unshift(`ao-types: ${partTypes.join(", ")}`)
        lines.push("")
        bodyParts.push(new Blob([lines.join("\r\n")]))
      }
    } else if (typeof value === "string") {
      // String with newlines or too long
      lines.push("")
      lines.push(value)
      lines.push("")
      bodyParts.push(new Blob([lines.join("\r\n")]))
    }
  }

  // Special case: add data/body as a separate form-data part if needed
  if (
    hasSpecialDataBody &&
    obj.data &&
    obj.data.body &&
    isBytes(obj.data.body)
  ) {
    const buffer = toBuffer(obj.data.body)
    const specialPart = [
      `content-disposition: form-data;name="data/body"`,
      "",
      "",
    ].join("\r\n")
    bodyParts.push(new Blob([specialPart, buffer]))
  }

  // Calculate boundary from content
  const partsContent = await Promise.all(bodyParts.map(part => part.text()))
  const allContent = partsContent.join("")
  const boundaryHash = await sha256(new TextEncoder().encode(allContent))
  const boundary = base64url.encode(Buffer.from(boundaryHash))

  // Assemble final multipart body - NO newlines after each part except the last
  const finalParts = []
  for (let i = 0; i < bodyParts.length; i++) {
    if (i === 0) {
      finalParts.push(new Blob([`--${boundary}\r\n`]))
    } else {
      finalParts.push(new Blob([`\r\n--${boundary}\r\n`]))
    }
    finalParts.push(bodyParts[i])
  }
  finalParts.push(new Blob([`\r\n--${boundary}--`]))

  headers["content-type"] = `multipart/form-data; boundary="${boundary}"`
  const body = new Blob(finalParts)

  // Calculate content digest
  const finalContent = await body.arrayBuffer()
  const contentDigest = await sha256(finalContent)
  const base64 = base64url.toBase64(base64url.encode(contentDigest))
  headers["content-digest"] = `sha-256=:${base64}:`
  headers["content-length"] = String(finalContent.byteLength)

  console.log("[encode] FINAL - headers:", headers, "body:", body)

  // Debug: decode the multipart body to verify structure
  const bodyText = await body.text()
  console.log("\n[encode] DEBUG - Full body text:")
  console.log(bodyText)

  // Parse multipart body
  const boundaryMatch = headers["content-type"].match(/boundary="([^"]+)"/)
  if (boundaryMatch) {
    const debugBoundary = boundaryMatch[1]
    const parts = bodyText.split(`--${debugBoundary}`)
    console.log("\n[encode] DEBUG - Multipart parts:")
    parts.forEach((part, idx) => {
      console.log(`Part ${idx}:`, JSON.stringify(part))
    })

    // Show the actual content part
    if (parts[1]) {
      console.log("\n[encode] DEBUG - Content part structure:")
      const lines = parts[1].trim().split("\r\n")
      lines.forEach((line, idx) => {
        if (line.includes("\u0000")) {
          console.log(
            `Line ${idx}: "${line.substring(0, line.indexOf("\u0000"))}" + [${line.length - line.indexOf("\u0000")} bytes]`
          )
        } else {
          console.log(`Line ${idx}: "${line}"`)
        }
      })
    }
  }

  return { headers, body }
}

export async function enc(fields) {
  return await encode(fields)
}
