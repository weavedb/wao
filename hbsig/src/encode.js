import base64url from "base64url"
import {
  getValueByPath,
  getAoType,
  isEmpty,
  encodePrimitiveContent,
  sortTypeAnnotations,
  analyzeArray,
  toBuffer,
  formatFloat,
  hasNonAscii,
  sha256,
  hasNewline,
  isBytes,
  isPojo,
} from "./encode-utils.js"

import encodeArrayItem from "./encode-array-item.js"
import collectBodyKeys from "./collect-body-keys.js"
const MAX_HEADER_LENGTH = 4096

// Step 1: Process and normalize input values (handle symbols, nested objects/arrays)
function processInputValues(obj) {
  // Currently this is a no-op, but will be used for input validation/normalization
  return obj
}

// Step 2: Handle empty object case
function handleEmptyObject(obj) {
  if (Object.keys(obj).length === 0) {
    return { headers: {}, body: undefined }
  }
  return null
}

// Step 3: Handle single field with empty binary
function handleSingleEmptyBinaryField(obj) {
  const objKeys = Object.keys(obj)

  if (objKeys.length === 1) {
    const fieldName = objKeys[0]
    const fieldValue = obj[fieldName]

    if (
      isBytes(fieldValue) &&
      (fieldValue.length === 0 || fieldValue.byteLength === 0)
    ) {
      const headers = {}
      headers["ao-types"] = `${fieldName.toLowerCase()}="empty-binary"`
      return { headers, body: undefined }
    }
  }

  return null
}

// Step 4: Handle single field with binary data
async function handleSingleBinaryField(obj) {
  const hasBodyBinary = obj.body && isBytes(obj.body)
  const otherFields = Object.keys(obj).filter(k => k !== "body")

  if (hasBodyBinary && otherFields.length === 0) {
    const headers = {}
    const bodyBuffer = toBuffer(obj.body)
    const bodyArrayBuffer = bodyBuffer.buffer.slice(
      bodyBuffer.byteOffset,
      bodyBuffer.byteOffset + bodyBuffer.byteLength
    )

    const contentDigest = await sha256(bodyArrayBuffer)
    const base64 = base64url.toBase64(base64url.encode(contentDigest))
    headers["content-digest"] = `sha-256=:${base64}:`

    return { headers, body: obj.body }
  }

  return null
}

// Step 5: Handle single field with primitive value (string/number/boolean/null/undefined/symbol)
async function handleSinglePrimitiveField(obj) {
  const objKeys = Object.keys(obj)

  if (objKeys.length === 1) {
    const fieldName = objKeys[0]
    const fieldValue = obj[fieldName]

    if (
      (fieldName === "data" || fieldName === "body") &&
      (typeof fieldValue === "string" ||
        typeof fieldValue === "boolean" ||
        typeof fieldValue === "number" ||
        fieldValue === null ||
        fieldValue === undefined ||
        typeof fieldValue === "symbol")
    ) {
      const headers = {}
      const bodyContent = encodePrimitiveContent(fieldValue)

      const encoder = new TextEncoder()
      const encoded = encoder.encode(bodyContent)
      const contentDigest = await sha256(encoded.buffer)
      const base64 = base64url.toBase64(base64url.encode(contentDigest))
      headers["content-digest"] = `sha-256=:${base64}:`

      const aoType = getAoType(fieldValue)
      if (aoType === "atom" || aoType === "integer" || aoType === "float") {
        headers["ao-types"] = `${fieldName.toLowerCase()}="${aoType}"`
      }

      if (fieldName !== "body") {
        headers["inline-body-key"] = fieldName
      }

      return { headers, body: bodyContent }
    }
  }

  return null
}

// Step 6a: Handle single field with non-empty binary (not body field)
async function handleSingleNonEmptyBinaryField(obj) {
  const objKeys = Object.keys(obj)

  if (objKeys.length === 1) {
    const fieldName = objKeys[0]
    const fieldValue = obj[fieldName]

    if (isBytes(fieldValue) && fieldValue.length > 0) {
      const headers = {}
      const bodyBuffer = toBuffer(fieldValue)
      const bodyArrayBuffer = bodyBuffer.buffer.slice(
        bodyBuffer.byteOffset,
        bodyBuffer.byteOffset + bodyBuffer.byteLength
      )

      const contentDigest = await sha256(bodyArrayBuffer)
      const base64 = base64url.toBase64(base64url.encode(contentDigest))
      headers["content-digest"] = `sha-256=:${base64}:`

      if (fieldName !== "body") {
        headers["inline-body-key"] = fieldName
      }

      return { headers, body: fieldValue }
    }
  }

  return null
}

// Step 6: Handle single field with non-ASCII string
async function handleSingleNonAsciiStringField(obj) {
  const objKeys = Object.keys(obj)

  if (objKeys.length === 1) {
    const fieldName = objKeys[0]
    const fieldValue = obj[fieldName]

    if (typeof fieldValue === "string" && hasNonAscii(fieldValue)) {
      const headers = {}
      const encoder = new TextEncoder()
      const encoded = encoder.encode(fieldValue)
      const contentDigest = await sha256(encoded.buffer)
      const base64 = base64url.toBase64(base64url.encode(contentDigest))
      headers["content-digest"] = `sha-256=:${base64}:`

      if (fieldName !== "body") {
        headers["inline-body-key"] = fieldName
      }

      return { headers, body: fieldValue }
    }
  }

  return null
}

// Step 7: Collect all keys that need to go in body
function collectBodyKeysStep(obj) {
  return collectBodyKeys(obj)
}

// Step 8: Process fields that can go in headers
function processHeaderFields(obj, bodyKeys, headers, headerTypes) {
  for (const [key, value] of Object.entries(obj)) {
    const needsBody =
      bodyKeys.includes(key) || bodyKeys.some(k => k.startsWith(`${key}/`))

    if (!needsBody) {
      if (value === null) {
        headers[key] = '"null"'
        headerTypes.push(`${key.toLowerCase()}="atom"`)
      } else if (value === undefined) {
        headers[key] = '"undefined"'
        headerTypes.push(`${key.toLowerCase()}="atom"`)
      } else if (typeof value === "boolean") {
        headers[key] = `"${value}"`
        headerTypes.push(`${key.toLowerCase()}="atom"`)
      } else if (typeof value === "symbol") {
        headers[key] = `"${value.description || "Symbol.for()"}"`
        headerTypes.push(`${key.toLowerCase()}="atom"`)
      } else if (typeof value === "number") {
        headers[key] = String(value)
        headerTypes.push(
          `${key.toLowerCase()}="${Number.isInteger(value) ? "integer" : "float"}"`
        )
      } else if (typeof value === "string") {
        if (value.length === 0) {
          headerTypes.push(`${key.toLowerCase()}="empty-binary"`)
        } else if (hasNonAscii(value)) {
          continue
        } else {
          headers[key] = value
        }
      } else if (Array.isArray(value) && value.length === 0) {
        headerTypes.push(`${key.toLowerCase()}="empty-list"`)
      } else if (Array.isArray(value) && !value.some(item => isPojo(item))) {
        const hasNonAsciiItems = value.some(
          item => typeof item === "string" && hasNonAscii(item)
        )
        if (!hasNonAsciiItems) {
          const encodedItems = value
            .map(item => encodeArrayItem(item))
            .join(", ")
          headers[key] = encodedItems
          headerTypes.push(`${key.toLowerCase()}="list"`)
        }
      } else if (
        isBytes(value) &&
        (value.length === 0 || value.byteLength === 0)
      ) {
        headerTypes.push(`${key.toLowerCase()}="empty-binary"`)
      } else if (isPojo(value) && Object.keys(value).length === 0) {
        headerTypes.push(`${key.toLowerCase()}="empty-message"`)
      }
    } else {
      // Fields that need body still get type annotations
      const aoType = getAoType(value)
      if (aoType) {
        headerTypes.push(`${key.toLowerCase()}="${aoType}"`)
      }
    }
  }

  // Second pass for array type annotations
  for (const [key, value] of Object.entries(obj)) {
    if (Array.isArray(value)) {
      if (
        bodyKeys.includes(key) ||
        bodyKeys.some(k => k.startsWith(`${key}/`))
      ) {
        if (!headerTypes.some(t => t.startsWith(`${key.toLowerCase()}=`))) {
          headerTypes.push(`${key.toLowerCase()}="list"`)
        }
      }
    }
  }
}

// Step 9: Handle case where all body keys are empty binaries
function handleAllEmptyBinaryBodyKeys(obj, bodyKeys, headers, headerTypes) {
  if (bodyKeys.length === 0) {
    if (headerTypes.length > 0) {
      headers["ao-types"] = headerTypes.sort().join(", ")
    }
    return { headers, body: undefined }
  }

  const allBodyKeysAreEmptyBinaries = bodyKeys.every(key => {
    const value = getValueByPath(obj, key)
    return isBytes(value) && (value.length === 0 || value.byteLength === 0)
  })

  if (allBodyKeysAreEmptyBinaries) {
    if (headerTypes.length > 0) {
      headers["ao-types"] = headerTypes.sort().join(", ")
    }
    return { headers, body: undefined }
  }

  return null
}

// Step 10: Handle single body key optimization
async function handleSingleBodyKeyOptimization(
  obj,
  bodyKeys,
  headers,
  headerTypes
) {
  if (bodyKeys.length === 1) {
    const singleKey = bodyKeys[0]
    const value = getValueByPath(obj, singleKey)

    // Apply optimization for binary data OR strings with newlines
    if (
      (isBytes(value) && value.length > 0) ||
      (typeof value === "string" && value.includes("\n"))
    ) {
      let contentToHash
      let bodyContent = value

      if (isBytes(value)) {
        const bodyBuffer = toBuffer(value)
        contentToHash = bodyBuffer.buffer.slice(
          bodyBuffer.byteOffset,
          bodyBuffer.byteOffset + bodyBuffer.byteLength
        )
      } else {
        // For strings, encode to UTF-8 for hashing
        const encoder = new TextEncoder()
        const encoded = encoder.encode(value)
        contentToHash = encoded.buffer
        bodyContent = value
      }

      const contentDigest = await sha256(contentToHash)
      const base64 = base64url.toBase64(base64url.encode(contentDigest))
      headers["content-digest"] = `sha-256=:${base64}:`

      if (singleKey !== "body") {
        headers["inline-body-key"] = singleKey
      }

      if (headerTypes.length > 0) {
        headers["ao-types"] = headerTypes.sort().join(", ")
      }

      return { headers, body: bodyContent }
    }
  }

  return null
}

// Step 11: Sort body keys
function sortBodyKeys(bodyKeys) {
  return bodyKeys.sort((a, b) => {
    const aIsArrayElement = /\/\d+$/.test(a)
    const bIsArrayElement = /\/\d+$/.test(b)
    const aBase = a.split("/")[0]
    const bBase = b.split("/")[0]
    if (aBase === bBase) {
      if (!aIsArrayElement && bIsArrayElement) {
        return -1
      }
      if (aIsArrayElement && !bIsArrayElement) {
        return 1
      }
      if (aIsArrayElement && bIsArrayElement) {
        const aIndex = parseInt(a.split("/")[1])
        const bIndex = parseInt(b.split("/")[1])
        return aIndex - bIndex
      }
      return a.localeCompare(b)
    }
    return a.localeCompare(b)
  })
}

// Step 12: Check for special data/body case
function checkSpecialDataBodyCase(obj, sortedBodyKeys) {
  return (
    sortedBodyKeys.includes("data") &&
    sortedBodyKeys.includes("body") &&
    obj.data &&
    obj.data.body &&
    isBytes(obj.data.body) &&
    obj.body &&
    obj.body.data &&
    isBytes(obj.body.data)
  )
}

// Step 13.2.2: Handle empty string in nested path
function handleEmptyStringInNestedPath(bodyKey, value, pathParts) {
  if (typeof value === "string" && value === "" && pathParts.length > 1) {
    const lines = []
    lines.push(`content-disposition: form-data;name="${bodyKey}"`)
    lines.push("")
    lines.push("")
    return new Blob([lines.join("\r\n")])
  }
  return null
}

// Step 13.2.3.2: Handle arrays with only empty elements
function handleArrayWithOnlyEmptyElements(
  bodyKey,
  value,
  headers,
  sortedBodyKeys
) {
  const fieldLines = []
  const partTypes = []

  value.forEach((item, idx) => {
    const index = idx + 1
    const itemType = getAoType(item)
    if (itemType) {
      partTypes.push(`${index}="${itemType}"`)
    }
  })

  const isInline = bodyKey === "body" && headers["inline-body-key"] === "body"

  if (isInline) {
    const orderedLines = []
    if (partTypes.length > 0) {
      orderedLines.push(
        `ao-types: ${sortTypeAnnotations(partTypes).join(", ")}`
      )
    }
    orderedLines.push("content-disposition: inline")
    orderedLines.push("")
    return new Blob([orderedLines.join("\r\n")])
  } else {
    const orderedLines = []
    if (partTypes.length > 0) {
      orderedLines.push(
        `ao-types: ${sortTypeAnnotations(partTypes).join(", ")}`
      )
    }
    orderedLines.push(`content-disposition: form-data;name="${bodyKey}"`)

    const isLastBodyPart =
      sortedBodyKeys.indexOf(bodyKey) === sortedBodyKeys.length - 1
    const hasOnlyTypes = partTypes.length > 0 && fieldLines.length === 0

    if (isLastBodyPart && hasOnlyTypes) {
      return new Blob([orderedLines.join("\r\n")])
    } else {
      orderedLines.push("")
      return new Blob([orderedLines.join("\r\n")])
    }
  }
}

// Step 13.2.3.3: Build indices with own parts
function buildIndicesWithOwnParts(bodyKey, sortedBodyKeys) {
  const indicesWithOwnParts = new Set()
  sortedBodyKeys.forEach(key => {
    if (key.startsWith(bodyKey + "/")) {
      const subPath = key.substring(bodyKey.length + 1)
      const match = subPath.match(/^(\d+)/)
      if (match) {
        indicesWithOwnParts.add(parseInt(match[1]))
      }
    }
  })
  return indicesWithOwnParts
}

// Step 13.2.3.4: Process array items
function processArrayItems(
  value,
  indicesWithOwnParts,
  hasNestedObjectParts,
  pathParts
) {
  const fieldLines = []
  const partTypes = []

  if (hasNestedObjectParts) {
    value.forEach((item, idx) => {
      const index = idx + 1
      if (Array.isArray(item)) {
        partTypes.push(`${index}="list"`)
      }
    })
  }

  value.forEach((item, idx) => {
    const index = idx + 1

    if (indicesWithOwnParts.has(index)) {
      // This item has its own part - skip it here
      // Don't add type annotation for items that have their own parts
      return
    }
    if (
      hasNestedObjectParts &&
      Array.isArray(item) &&
      item.some(subItem => isPojo(subItem))
    ) {
      return
    }

    if (typeof item === "string" && item === "") {
      partTypes.push(`${index}="empty-binary"`)
    } else if (isPojo(item) && Object.keys(item).length === 0) {
      partTypes.push(`${index}="empty-message"`)
    } else if (isPojo(item)) {
      // Non-empty objects are handled elsewhere
    } else if (Array.isArray(item)) {
      if (item.length === 0) {
        partTypes.push(`${index}="empty-list"`)
      } else {
        partTypes.push(`${index}="list"`)
        const encodedItems = item
          .map(subItem => {
            if (typeof subItem === "number") {
              if (Number.isInteger(subItem)) {
                return `"(ao-type-integer) ${subItem}"`
              } else {
                return `"(ao-type-float) ${formatFloat(subItem)}"`
              }
            } else if (typeof subItem === "string") {
              return `"${subItem}"`
            } else if (subItem === null) {
              return `"(ao-type-atom) \\"null\\""`
            } else if (subItem === undefined) {
              return `"(ao-type-atom) \\"undefined\\""`
            } else if (typeof subItem === "symbol") {
              const desc = subItem.description || "Symbol.for()"
              return `"(ao-type-atom) \\"${desc}\\""`
            } else if (typeof subItem === "boolean") {
              return `"(ao-type-atom) \\"${subItem}\\""`
            } else if (Array.isArray(subItem)) {
              return encodeArrayItem(subItem)
            } else if (isBytes(subItem)) {
              const buffer = toBuffer(subItem)
              if (buffer.length === 0 || buffer.byteLength === 0) {
                return `""`
              }
              return `"(ao-type-binary)"`
            } else if (isPojo(subItem)) {
              const json = JSON.stringify(subItem)
              const escaped = json.replace(/\\/g, "\\\\").replace(/"/g, '\\"')
              return `"(ao-type-map) ${escaped}"`
            } else {
              return `"${String(subItem)}"`
            }
          })
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
      fieldLines.push(`${index}: ${item}`)
    } else if (
      item === null ||
      item === undefined ||
      typeof item === "symbol" ||
      typeof item === "boolean"
    ) {
      partTypes.push(`${index}="atom"`)
      if (item === null) {
        fieldLines.push(`${index}: null`)
      } else if (item === undefined) {
        fieldLines.push(`${index}: undefined`)
      } else if (typeof item === "symbol") {
        const desc = item.description || "Symbol.for()"
        fieldLines.push(`${index}: ${desc}`)
      } else {
        fieldLines.push(`${index}: ${item}`)
      }
    } else if (isBytes(item)) {
      const buffer = toBuffer(item)
      if (buffer.length === 0) {
        partTypes.push(`${index}="empty-binary"`)
      } else {
        partTypes.push(`${index}="binary"`)
      }
    }
  })

  return { fieldLines, partTypes }
}

// Step 13.2.3.5: Create array body part
function createArrayBodyPart(bodyKey, fieldLines, partTypes, headers) {
  const isInline = bodyKey === "body" && headers["inline-body-key"] === "body"

  if (isInline) {
    const orderedLines = []
    if (partTypes.length > 0) {
      orderedLines.push(
        `ao-types: ${sortTypeAnnotations(partTypes).join(", ")}`
      )
    }
    orderedLines.push("content-disposition: inline")
    if (fieldLines.length > 0) {
      orderedLines.push("")
      for (const line of fieldLines) {
        orderedLines.push(line)
      }
    }
    return new Blob([orderedLines.join("\r\n") + "\r\n"])
  } else {
    const orderedLines = []
    if (partTypes.length > 0) {
      orderedLines.push(
        `ao-types: ${sortTypeAnnotations(partTypes).join(", ")}`
      )
    }
    orderedLines.push(`content-disposition: form-data;name="${bodyKey}"`)
    for (const line of fieldLines) {
      orderedLines.push(line)
    }
    if (fieldLines.length > 0) {
      return new Blob([orderedLines.join("\r\n") + "\r\n"])
    } else {
      return new Blob([orderedLines.join("\r\n")])
    }
  }
}

// Step 13.2.3: Handle array values
function handleArrayValue(bodyKey, value, headers, sortedBodyKeys, pathParts) {
  const arrayInfo = analyzeArray(value)

  if (arrayInfo.hasOnlyNonEmptyObjects) {
    return null
  }

  if (arrayInfo.hasOnlyEmptyElements) {
    return handleArrayWithOnlyEmptyElements(
      bodyKey,
      value,
      headers,
      sortedBodyKeys
    )
  }

  const indicesWithOwnParts = buildIndicesWithOwnParts(bodyKey, sortedBodyKeys)
  const hasNestedObjectParts = sortedBodyKeys.some(
    key =>
      key.startsWith(bodyKey + "/") &&
      key.split("/").length > pathParts.length + 1
  )

  const { fieldLines, partTypes } = processArrayItems(
    value,
    indicesWithOwnParts,
    hasNestedObjectParts,
    pathParts
  )
  return createArrayBodyPart(bodyKey, fieldLines, partTypes, headers)
}

// Step 13.2.4.3: Process object fields
function processObjectFields(value, bodyKey, sortedBodyKeys) {
  const objectTypes = []
  const fieldLines = []
  const binaryFields = []
  const arrayTypes = []

  // First collect array types
  for (const [k, v] of Object.entries(value)) {
    if (Array.isArray(v)) {
      arrayTypes.push(
        `${k.toLowerCase()}="${v.length === 0 ? "empty-list" : "list"}"`
      )
    }
  }

  // Then process other fields
  for (const [k, v] of Object.entries(value)) {
    const childPath = `${bodyKey}/${k}`

    if (sortedBodyKeys.includes(childPath)) {
      continue
    }

    if (Array.isArray(v) && v.some(item => isPojo(item))) {
      const hasOnlyEmpty = v.every(item => isEmpty(item))
      if (hasOnlyEmpty) {
        continue
      }
    }

    if (Array.isArray(v)) {
      // Type already added in arrayTypes
    } else if (
      v === null ||
      v === undefined ||
      typeof v === "symbol" ||
      typeof v === "boolean"
    ) {
      objectTypes.push(`${k.toLowerCase()}="atom"`)
    } else if (typeof v === "number") {
      objectTypes.push(
        `${k.toLowerCase()}="${Number.isInteger(v) ? "integer" : "float"}"`
      )
    } else if (typeof v === "string" && v.length === 0) {
      objectTypes.push(`${k.toLowerCase()}="empty-binary"`)
    } else if (isBytes(v) && (v.length === 0 || v.byteLength === 0)) {
      objectTypes.push(`${k.toLowerCase()}="empty-binary"`)
    } else if (isPojo(v) && Object.keys(v).length === 0) {
      objectTypes.push(`${k.toLowerCase()}="empty-message"`)
    }

    if (typeof v === "string") {
      if (v.length === 0) {
        fieldLines.push(`${k}: `)
      } else {
        fieldLines.push(`${k}: ${v}`)
      }
    } else if (typeof v === "number") {
      fieldLines.push(`${k}: ${v}`)
    } else if (typeof v === "boolean") {
      fieldLines.push(`${k}: "${v}"`)
    } else if (v === null) {
      fieldLines.push(`${k}: "null"`)
    } else if (v === undefined) {
      fieldLines.push(`${k}: "undefined"`)
    } else if (typeof v === "symbol") {
      const desc = v.description || "Symbol.for()"
      fieldLines.push(`${k}: "${desc}"`)
    } else if (isBytes(v)) {
      const buffer = toBuffer(v)
      binaryFields.push({ key: k, buffer })
    } else if (Array.isArray(v) && v.length > 0) {
      const childPath = `${bodyKey}/${k}`
      if (!sortedBodyKeys.includes(childPath)) {
        const hasObjects = v.some(item => isPojo(item))
        if (!hasObjects) {
          const encodedItems = v.map(item => encodeArrayItem(item)).join(", ")
          fieldLines.push(`${k}: ${encodedItems}`)
        }
      }
    }
  }

  const allTypes = [...arrayTypes, ...objectTypes]
  return { allTypes, fieldLines, binaryFields }
}

// Step 13.2.4.5: Create object body part
function createObjectBodyPart(
  bodyKey,
  value,
  allTypes,
  fieldLines,
  binaryFields,
  headers,
  sortedBodyKeys
) {
  const isInline = bodyKey === "body" && headers["inline-body-key"] === "body"
  const lines = []

  if (isInline) {
    const orderedLines = []

    // For inline mode: fields first, then headers
    for (const line of fieldLines) {
      orderedLines.push(line)
    }

    if (allTypes.length > 0) {
      orderedLines.push(`ao-types: ${allTypes.sort().join(", ")}`)
    }
    orderedLines.push("content-disposition: inline")

    const binaryFieldsForInline = Object.entries(value)
      .filter(
        ([k, v]) => isBytes(v) && !sortedBodyKeys.includes(`${bodyKey}/${k}`)
      )
      .map(([k, v]) => ({
        key: k,
        buffer: toBuffer(v),
      }))

    if (binaryFieldsForInline.length > 0) {
      const parts = []
      // Join all text lines first
      parts.push(Buffer.from(orderedLines.join("\r\n")))
      // Then add binary fields
      for (const { key, buffer } of binaryFieldsForInline) {
        parts.push(Buffer.from(`\r\n${key}: `))
        parts.push(buffer)
      }
      parts.push(Buffer.from("\r\n"))
      const fullBody = Buffer.concat(parts)
      return new Blob([fullBody])
    } else {
      return new Blob([orderedLines.join("\r\n") + "\r\n"])
    }
  } else {
    // Non-inline mode remains the same
    const orderedLines = []
    if (allTypes.length > 0) {
      orderedLines.push(`ao-types: ${allTypes.sort().join(", ")}`)
    }
    orderedLines.push(`content-disposition: form-data;name="${bodyKey}"`)

    const hasBinaryFields = binaryFields && binaryFields.length > 0
    if (hasBinaryFields || fieldLines.length === 0) {
      orderedLines.push("")
    }

    for (const line of fieldLines) {
      orderedLines.push(line)
    }

    if (binaryFields && binaryFields.length > 0) {
      const parts = []
      const headerText = orderedLines.join("\r\n")
      parts.push(Buffer.from(headerText))
      for (let i = 0; i < binaryFields.length; i++) {
        const { key, buffer } = binaryFields[i]
        if (i > 0) {
          parts.push(Buffer.from("\r\n"))
        }
        parts.push(Buffer.from(`${key}: `))
        parts.push(buffer)
      }
      parts.push(Buffer.from("\r\n"))
      const fullBody = Buffer.concat(parts)
      return new Blob([fullBody])
    } else {
      if (fieldLines.length > 0) {
        return new Blob([orderedLines.join("\r\n") + "\r\n"])
      } else {
        return new Blob([orderedLines.join("\r\n")])
      }
    }
  }
}

// Step 13.2.4: Handle object values
function handleObjectValue(
  obj,
  bodyKey,
  value,
  headers,
  sortedBodyKeys,
  pathParts,
  hasSpecialDataBody
) {
  if (Object.keys(value).length === 0) {
    // Skip empty objects in certain contexts
    const parentPath = pathParts.slice(0, -1).join("/")
    const parentValue = parentPath ? getValueByPath(obj, parentPath) : obj

    if (Array.isArray(parentValue)) {
      const parentArrayInfo = analyzeArray(parentValue)
      if (
        parentArrayInfo.hasObjects &&
        (parentArrayInfo.hasEmptyStrings || parentArrayInfo.hasEmptyObjects)
      ) {
        return null
      }
    }
    return null
  }

  // Skip special data/body case
  if (
    hasSpecialDataBody &&
    bodyKey === "data" &&
    Object.keys(value).length === 1 &&
    value.body &&
    isBytes(value.body)
  ) {
    return null
  }

  const { allTypes, fieldLines, binaryFields } = processObjectFields(
    value,
    bodyKey,
    sortedBodyKeys
  )

  // Check if object should be skipped
  const hasOnlyEmptyCollections = Object.entries(value).every(([k, v]) =>
    isEmpty(v)
  )
  const hasArraysWithOnlyEmptyElements = Object.entries(value).some(
    ([k, v]) =>
      Array.isArray(v) && v.length > 0 && v.every(item => isEmpty(item))
  )

  const shouldSkipObject = Object.entries(value).every(([k, v]) => {
    const childPath = `${bodyKey}/${k}`
    if (sortedBodyKeys.includes(childPath)) return true
    if (Array.isArray(v) && v.some(item => isPojo(item))) {
      const hasOnlyEmpty = v.every(item => isEmpty(item))
      return hasOnlyEmpty || sortedBodyKeys.includes(childPath)
    }
    return false
  })

  if (
    shouldSkipObject &&
    !hasOnlyEmptyCollections &&
    !hasArraysWithOnlyEmptyElements
  ) {
    return null
  }

  return createObjectBodyPart(
    bodyKey,
    value,
    allTypes,
    fieldLines,
    binaryFields,
    headers,
    sortedBodyKeys
  )
}

// Step 13.2.5: Handle primitive values
function handlePrimitiveValue(bodyKey, value, headers) {
  const isInline = bodyKey === "body" && headers["inline-body-key"] === "body"
  const lines = []

  if (isInline) {
    lines.push(`content-disposition: inline`)
  } else {
    lines.push(`content-disposition: form-data;name="${bodyKey}"`)
  }

  if (typeof value === "string") {
    lines.push("")
    lines.push(value)
    return new Blob([lines.join("\r\n")])
  } else {
    const content = encodePrimitiveContent(value)
    lines.push("")
    lines.push(content)
    return new Blob([lines.join("\r\n")])
  }
}

// Step 13.2.6: Handle binary values
function handleBinaryValue(bodyKey, value, headers) {
  const isInline = bodyKey === "body" && headers["inline-body-key"] === "body"
  const lines = []

  if (isInline) {
    lines.push(`content-disposition: inline`)
  } else {
    lines.push(`content-disposition: form-data;name="${bodyKey}"`)
  }

  const buffer = toBuffer(value)
  // Always keep binary data as raw binary, regardless of whether it's in an array
  const headerText = lines.join("\r\n") + "\r\n\r\n"
  return new Blob([headerText, buffer])
}

// Step 13.3: Handle special data/body case
function handleSpecialDataBodyCase(obj, hasSpecialDataBody) {
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
    return new Blob([specialPart, buffer])
  }
  return null
}

// Step 13: Build body parts for each body key
function buildBodyParts(obj, sortedBodyKeys, headers, hasSpecialDataBody) {
  // Step 13.1: Initialize body parts collection
  const bodyParts = []

  // Step 13.2: Process each body key
  for (const bodyKey of sortedBodyKeys) {
    // Step 13.2.1: Get value for current body key
    const value = getValueByPath(obj, bodyKey)
    const pathParts = bodyKey.split("/")

    // Step 13.2.2: Handle empty string in nested path
    const emptyStringPart = handleEmptyStringInNestedPath(
      bodyKey,
      value,
      pathParts
    )
    if (emptyStringPart) {
      bodyParts.push(emptyStringPart)
      continue
    }

    // Step 13.2.3: Handle array values
    if (Array.isArray(value)) {
      const arrayPart = handleArrayValue(
        bodyKey,
        value,
        headers,
        sortedBodyKeys,
        pathParts
      )
      if (arrayPart) {
        bodyParts.push(arrayPart)
      }
      continue
    }

    // Step 13.2.4: Handle object values
    if (isPojo(value)) {
      const objectPart = handleObjectValue(
        obj,
        bodyKey,
        value,
        headers,
        sortedBodyKeys,
        pathParts,
        hasSpecialDataBody
      )
      if (objectPart) {
        bodyParts.push(objectPart)
      }
      continue
    }

    // Step 13.2.5: Handle primitive values
    if (
      typeof value === "string" ||
      typeof value === "boolean" ||
      typeof value === "number" ||
      value === null ||
      value === undefined ||
      typeof value === "symbol"
    ) {
      const primitivePart = handlePrimitiveValue(bodyKey, value, headers)
      bodyParts.push(primitivePart)
      continue
    }

    // Step 13.2.6: Handle binary values
    if (isBytes(value)) {
      const binaryPart = handleBinaryValue(bodyKey, value, headers)
      bodyParts.push(binaryPart)
      continue
    }
  }

  // Step 13.3: Handle special data/body case
  const specialPart = handleSpecialDataBodyCase(obj, hasSpecialDataBody)
  if (specialPart) {
    bodyParts.push(specialPart)
  }

  // Step 13.4: Return body parts
  return bodyParts
}

// Step 14: Generate multipart boundary
async function generateBoundary(bodyParts) {
  const partsContent = await Promise.all(bodyParts.map(part => part.text()))
  const allContent = partsContent.join("")
  const boundaryHash = await sha256(new TextEncoder().encode(allContent))
  const boundary = base64url.encode(Buffer.from(boundaryHash))
  return boundary
}

// Step 15: Assemble final multipart body
function assembleMultipartBody(bodyParts, boundary) {
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

  return new Blob(finalParts)
}

// Step 16: Calculate content digest
async function calculateContentDigest(body) {
  const finalContent = await body.arrayBuffer()

  if (finalContent.byteLength > 0) {
    const contentDigest = await sha256(finalContent)
    const base64 = base64url.toBase64(base64url.encode(contentDigest))
    return { digest: base64, byteLength: finalContent.byteLength }
  }

  return { digest: null, byteLength: finalContent.byteLength }
}

// Step 17: Set final headers (content-type, content-length)
function setFinalHeaders(headers, boundary, contentDigest, byteLength) {
  headers["content-type"] = `multipart/form-data; boundary="${boundary}"`

  if (contentDigest) {
    headers["content-digest"] = `sha-256=:${contentDigest}:`
  }

  headers["content-length"] = String(byteLength)
}

export async function enc(obj = {}) {
  // Step 1: Process and normalize input values
  const processedObj = processInputValues(obj)

  // Step 2: Handle empty object case
  const emptyResult = handleEmptyObject(processedObj)
  if (emptyResult) return emptyResult

  // Step 3: Handle single field with empty binary
  const emptyBinaryResult = handleSingleEmptyBinaryField(processedObj)
  if (emptyBinaryResult) return emptyBinaryResult

  // Step 4: Handle single field with binary data
  const singleBinaryResult = await handleSingleBinaryField(processedObj)
  if (singleBinaryResult) return singleBinaryResult

  // Step 5: Handle single field with primitive value
  const primitiveResult = await handleSinglePrimitiveField(processedObj)
  if (primitiveResult) return primitiveResult

  // Step 6a: Handle single field with non-empty binary
  const nonEmptyBinaryResult =
    await handleSingleNonEmptyBinaryField(processedObj)
  if (nonEmptyBinaryResult) return nonEmptyBinaryResult

  // Step 6: Handle single field with non-ASCII string
  const nonAsciiResult = await handleSingleNonAsciiStringField(processedObj)
  if (nonAsciiResult) return nonAsciiResult

  // Step 7: Collect all keys that need to go in body
  const bodyKeys = collectBodyKeysStep(processedObj)

  const objKeys = Object.keys(obj)
  const headers = {}
  const headerTypes = []

  // Step 8: Process fields that can go in headers
  processHeaderFields(obj, bodyKeys, headers, headerTypes)

  // Step 9: Handle case where all body keys are empty binaries
  const emptyBinaryBodyResult = handleAllEmptyBinaryBodyKeys(
    obj,
    bodyKeys,
    headers,
    headerTypes
  )
  if (emptyBinaryBodyResult) return emptyBinaryBodyResult

  // Step 10: Handle single body key optimization
  const singleBodyKeyResult = await handleSingleBodyKeyOptimization(
    obj,
    bodyKeys,
    headers,
    headerTypes
  )
  if (singleBodyKeyResult) return singleBodyKeyResult

  // Step 11: Sort body keys
  const sortedBodyKeys = sortBodyKeys(bodyKeys)

  // Step 12: Check for special data/body case
  const hasSpecialDataBody = checkSpecialDataBodyCase(obj, sortedBodyKeys)

  // Only add body-keys header if there are actual body keys
  if (sortedBodyKeys.length > 0) {
    headers["body-keys"] = sortedBodyKeys.map(k => `"${k}"`).join(", ")
  }

  // Special case: single body key named "body" containing an object
  if (
    !hasSpecialDataBody &&
    sortedBodyKeys.length === 1 &&
    sortedBodyKeys[0] === "body"
  ) {
    const bodyValue = obj.body
    if (isPojo(bodyValue)) {
      headers["inline-body-key"] = "body"
    }
  }

  if (headerTypes.length > 0) {
    headers["ao-types"] = headerTypes.sort().join(", ")
  }

  // Step 13: Build body parts for each body key
  const bodyParts = buildBodyParts(
    obj,
    sortedBodyKeys,
    headers,
    hasSpecialDataBody
  )

  // If no body parts were created, return headers only
  if (bodyParts.length === 0) {
    return { headers, body: undefined }
  }

  // Step 14: Generate multipart boundary
  const boundary = await generateBoundary(bodyParts)

  // Step 15: Assemble final multipart body
  const body = assembleMultipartBody(bodyParts, boundary)

  // Step 16: Calculate content digest
  const { digest: contentDigest, byteLength } =
    await calculateContentDigest(body)

  // Step 17: Set final headers (content-type, content-length)
  setFinalHeaders(headers, boundary, contentDigest, byteLength)

  // Step 18: Return result
  return { headers, body }
}
