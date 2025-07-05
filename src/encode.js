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
  let exp = num.toExponential(20)
  exp = exp.replace(/e\+(\d)$/, "e+0$1")
  exp = exp.replace(/e-(\d)$/, "e-0$1")
  return exp
}

function hasNonAscii(str) {
  return /[^\x00-\x7F]/.test(str)
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
    const buffer = toBuffer(item)
    if (buffer.length === 0 || buffer.byteLength === 0) {
      return `""`
    }
    return `"(ao-type-binary)"`
  } else if (isPojo(item)) {
    const json = JSON.stringify(item)
    const escaped = json.replace(/\\/g, "\\\\").replace(/"/g, '\\"')
    return `"(ao-type-map) ${escaped}"`
  } else {
    return `"${String(item)}"`
  }
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

function collectBodyKeys(obj, prefix = "") {
  const keys = []

  function traverse(current, path) {
    let hasSimpleFields = false
    const nestedPaths = []

    for (const [key, value] of Object.entries(current)) {
      const fullPath = path ? `${path}/${key}` : key

      if (Array.isArray(value)) {
        const hasObjects = value.some(item => isPojo(item))
        const hasNonObjects = value.some(item => !isPojo(item))

        if (hasObjects) {
          value.forEach((item, index) => {
            if (isPojo(item)) {
              nestedPaths.push(`${fullPath}/${index + 1}`)
            }
          })

          if (hasNonObjects) {
            hasSimpleFields = true
          }
        } else {
          hasSimpleFields = true
        }
      } else if (isPojo(value)) {
        if (Object.keys(value).length === 0) {
          hasSimpleFields = true
        } else {
          nestedPaths.push(fullPath)
        }
      } else if (isBytes(value) && value.length > 0) {
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

    if (hasSimpleFields) {
      keys.push(path)
    }

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

  const objKeys = Object.keys(obj)
  for (const [key, value] of Object.entries(obj)) {
    if (
      (key === "data" || key === "body") &&
      (typeof value === "string" ||
        typeof value === "boolean" ||
        typeof value === "number" ||
        value === null ||
        value === undefined ||
        typeof value === "symbol") &&
      objKeys.length > 1
    ) {
      if (
        key === "data" &&
        obj.body &&
        isPojo(obj.body) &&
        Object.keys(obj.body).length > 0
      ) {
      } else {
        keys.push(key)
      }
    } else if (Array.isArray(value)) {
      const hasObjects = value.some(item => isPojo(item))
      const hasArrays = value.some(item => Array.isArray(item))
      const hasNonObjects = value.some(item => !isPojo(item))

      if (hasObjects) {
        value.forEach((item, index) => {
          if (isPojo(item)) {
            keys.push(`${key}/${index + 1}`)
            for (const [nestedKey, nestedValue] of Object.entries(item)) {
              if (isPojo(nestedValue)) {
                keys.push(`${key}/${index + 1}/${nestedKey}`)
              }
            }
          }
        })

        if (hasNonObjects) {
          keys.push(key)
        }
      } else if (hasArrays) {
        keys.push(key)
      } else {
      }
    } else if (isPojo(value)) {
      traverse(value, key)
    } else if (isBytes(value) && value.length > 0) {
      keys.push(key)
    } else if (typeof value === "string" && value.includes("\n")) {
      keys.push(key)
    } else if (typeof value === "string" && hasNonAscii(value)) {
      keys.push(key)
    }
  }

  return [...new Set(keys)].filter(k => k !== "")
}

async function encode(obj = {}) {
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

  if (Object.keys(obj).length === 0) {
    return { headers: {}, body: undefined }
  }

  const objKeys = Object.keys(obj)

  if (objKeys.length === 1) {
    const fieldName = objKeys[0]
    const fieldValue = obj[fieldName]

    if (
      isBytes(fieldValue) &&
      (fieldValue.length === 0 || fieldValue.byteLength === 0)
    ) {
      const headers = {}
      headers["ao-types"] = `${fieldName}="empty-binary"`
      return { headers, body: undefined }
    }
  }

  if (
    obj.body &&
    isBytes(obj.body) &&
    (obj.body.length === 0 || obj.body.byteLength === 0) &&
    objKeys.length > 1
  ) {
  }

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
    } else if (
      (fieldName === "data" || fieldName === "body") &&
      (typeof fieldValue === "string" ||
        typeof fieldValue === "boolean" ||
        typeof fieldValue === "number" ||
        fieldValue === null ||
        fieldValue === undefined ||
        typeof fieldValue === "symbol")
    ) {
      const headers = {}

      let bodyContent
      if (typeof fieldValue === "string") {
        bodyContent = fieldValue
      } else if (typeof fieldValue === "boolean") {
        bodyContent = `"${fieldValue}"`
      } else if (typeof fieldValue === "number") {
        bodyContent = String(fieldValue)
      } else if (fieldValue === null) {
        bodyContent = '"null"'
      } else if (fieldValue === undefined) {
        bodyContent = '"undefined"'
      } else if (typeof fieldValue === "symbol") {
        bodyContent = `"${fieldValue.description || "Symbol.for()"}"`
      }

      const encoder = new TextEncoder()
      const encoded = encoder.encode(bodyContent)
      const contentDigest = await sha256(encoded.buffer)
      const base64 = base64url.toBase64(base64url.encode(contentDigest))
      headers["content-digest"] = `sha-256=:${base64}:`

      if (
        typeof fieldValue === "boolean" ||
        fieldValue === null ||
        fieldValue === undefined ||
        typeof fieldValue === "symbol"
      ) {
        headers["ao-types"] = `${fieldName}="atom"`
      } else if (typeof fieldValue === "number") {
        headers["ao-types"] =
          `${fieldName}="${Number.isInteger(fieldValue) ? "integer" : "float"}"`
      }

      if (fieldName !== "body") {
        headers["inline-body-key"] = fieldName
      }

      return { headers, body: bodyContent }
    } else if (typeof fieldValue === "string" && hasNonAscii(fieldValue)) {
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

  const headers = {}
  const headerTypes = []

  const bodyKeys = collectBodyKeys(obj)

  for (const [key, value] of Object.entries(obj)) {
    const needsBody =
      bodyKeys.includes(key) || bodyKeys.some(k => k.startsWith(`${key}/`))

    if (!needsBody) {
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
        } else if (hasNonAscii(value)) {
          continue
        } else {
          headers[key] = value
        }
      } else if (Array.isArray(value) && value.length === 0) {
        headerTypes.push(`${key}="empty-list"`)
      } else if (Array.isArray(value) && !value.some(item => isPojo(item))) {
        const hasNonAsciiItems = value.some(
          item => typeof item === "string" && hasNonAscii(item)
        )
        if (!hasNonAsciiItems) {
          const encodedItems = value
            .map(item => encodeArrayItem(item))
            .join(", ")
          headers[key] = encodedItems
          headerTypes.push(`${key}="list"`)
        }
      } else if (
        isBytes(value) &&
        (value.length === 0 || value.byteLength === 0)
      ) {
        headerTypes.push(`${key}="empty-binary"`)
      } else if (isPojo(value) && Object.keys(value).length === 0) {
        headerTypes.push(`${key}="empty-message"`)
      }
    } else {
      if (isBytes(value) && (value.length === 0 || value.byteLength === 0)) {
        headerTypes.push(`${key}="empty-binary"`)
      } else if (typeof value === "string" && value.length === 0) {
        headerTypes.push(`${key}="empty-binary"`)
      } else if (Array.isArray(value) && value.length === 0) {
        headerTypes.push(`${key}="empty-list"`)
      } else if (isPojo(value) && Object.keys(value).length === 0) {
        headerTypes.push(`${key}="empty-message"`)
      } else if (
        typeof value === "boolean" ||
        value === null ||
        value === undefined ||
        typeof value === "symbol"
      ) {
        headerTypes.push(`${key}="atom"`)
      } else if (typeof value === "number") {
        headerTypes.push(
          `${key}="${Number.isInteger(value) ? "integer" : "float"}"`
        )
      }
    }
  }

  for (const [key, value] of Object.entries(obj)) {
    if (Array.isArray(value)) {
      if (
        bodyKeys.includes(key) ||
        bodyKeys.some(k => k.startsWith(`${key}/`))
      ) {
        if (!headerTypes.some(t => t.startsWith(`${key}=`))) {
          headerTypes.push(`${key}="list"`)
        }
      }
    }
  }

  if (bodyKeys.length === 0) {
    for (const [key, value] of Object.entries(obj)) {
      if (isBytes(value) && (value.length === 0 || value.byteLength === 0)) {
        if (!headerTypes.some(t => t.startsWith(`${key}=`))) {
          headerTypes.push(`${key}="empty-binary"`)
        }
      } else if (Array.isArray(value) && value.length === 0) {
        if (!headerTypes.some(t => t.startsWith(`${key}=`))) {
          headerTypes.push(`${key}="empty-list"`)
        }
      } else if (isPojo(value) && Object.keys(value).length === 0) {
        if (!headerTypes.some(t => t.startsWith(`${key}=`))) {
          headerTypes.push(`${key}="empty-message"`)
        }
      } else if (typeof value === "string" && value.length === 0) {
        if (!headerTypes.some(t => t.startsWith(`${key}=`))) {
          headerTypes.push(`${key}="empty-binary"`)
        }
      }
    }

    if (headerTypes.length > 0) {
      headers["ao-types"] = headerTypes.sort().join(", ")
    }
    return { headers, body: undefined }
  }

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
    if (headerTypes.length > 0) {
      headers["ao-types"] = headerTypes.sort().join(", ")
    }
    return { headers, body: undefined }
  }

  if (bodyKeys.length === 1) {
    const singleKey = bodyKeys[0]
    const pathParts = singleKey.split("/")
    let value = obj
    for (const part of pathParts) {
      if (/^\d+$/.test(part)) {
        value = value[parseInt(part) - 1]
      } else {
        value = value[part]
      }
    }

    const otherFieldsAreEmpty = Object.entries(obj).every(([key, val]) => {
      if (key === singleKey) return true
      return (
        (Array.isArray(val) && val.length === 0) ||
        (isPojo(val) && Object.keys(val).length === 0) ||
        (isBytes(val) && (val.length === 0 || val.byteLength === 0)) ||
        (typeof val === "string" && val.length === 0)
      )
    })

    if (otherFieldsAreEmpty && isBytes(value) && value.length > 0) {
      const bodyBuffer = toBuffer(value)
      const bodyArrayBuffer = bodyBuffer.buffer.slice(
        bodyBuffer.byteOffset,
        bodyBuffer.byteOffset + bodyBuffer.byteLength
      )

      const contentDigest = await sha256(bodyArrayBuffer)
      const base64 = base64url.toBase64(base64url.encode(contentDigest))
      headers["content-digest"] = `sha-256=:${base64}:`

      if (singleKey !== "body") {
        headers["inline-body-key"] = singleKey
      }

      if (headerTypes.length > 0) {
        headers["ao-types"] = headerTypes.sort().join(", ")
      }

      return { headers, body: value }
    }
  }

  const sortedBodyKeys = bodyKeys.sort((a, b) => {
    if (a.startsWith(b + "/")) return 1
    if (b.startsWith(a + "/")) return -1
    return a.localeCompare(b)
  })

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

  if (!hasSpecialDataBody) {
    if (sortedBodyKeys.includes("body") && sortedBodyKeys.length === 1) {
      headers["inline-body-key"] = "body"
    }
  }

  if (headerTypes.length > 0) {
    headers["ao-types"] = headerTypes.sort().join(", ")
  }

  const bodyParts = []

  for (const bodyKey of sortedBodyKeys) {
    const lines = []

    const pathParts = bodyKey.split("/")
    let value = obj
    let parent = null

    for (let i = 0; i < pathParts.length; i++) {
      parent = value
      const part = pathParts[i]

      if (/^\d+$/.test(part)) {
        value = value[parseInt(part) - 1]
      } else {
        value = value[part]
      }
    }

    if (Array.isArray(value) && value.every(item => isPojo(item))) {
      continue
    }

    if (isPojo(value) && Object.keys(value).length === 0) {
      continue
    }

    if (
      hasSpecialDataBody &&
      bodyKey === "data" &&
      isPojo(value) &&
      Object.keys(value).length === 1 &&
      value.body &&
      isBytes(value.body)
    ) {
      continue
    }

    const isInline = bodyKey === "body" && headers["inline-body-key"] === "body"
    if (isInline) {
      lines.push(`content-disposition: inline`)
    } else {
      lines.push(`content-disposition: form-data; name="${bodyKey}"`)
    }

    if (isBytes(value)) {
      const buffer = toBuffer(value)
      const headerText = lines.join("\r\n") + "\r\n\r\n"
      bodyParts.push(new Blob([headerText, buffer]))
    } else if (isPojo(value)) {
      const objectTypes = []
      const fieldLines = []
      const binaryFields = []

      for (const [k, v] of Object.entries(value)) {
        const childPath = `${bodyKey}/${k}`

        if (sortedBodyKeys.includes(childPath)) {
          continue
        }

        if (Array.isArray(v) && v.some(item => isPojo(item))) {
          continue
        }

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
          if (isInline) {
            continue
          } else {
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
        }
      }

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

      if (isInline) {
        const orderedLines = []

        if (!onlyEmptyCollections) {
          for (const line of fieldLines) {
            orderedLines.push(line)
          }
        }

        if (objectTypes.length > 0) {
          orderedLines.push(`ao-types: ${objectTypes.sort().join(", ")}`)
        }

        orderedLines.push("content-disposition: inline")

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
          const parts = []
          parts.push(Buffer.from(orderedLines.join("\r\n")))

          for (const { key, buffer } of binaryFields) {
            parts.push(Buffer.from(`\r\n${key}: `))
            parts.push(buffer)
          }

          parts.push(Buffer.from("\r\n"))

          const fullBody = Buffer.concat(parts)
          bodyParts.push(new Blob([fullBody]))
        } else {
          bodyParts.push(new Blob([orderedLines.join("\r\n")]))
        }
      } else {
        if (objectTypes.length > 0) {
          lines.unshift(`ao-types: ${objectTypes.sort().join(", ")}`)
        }

        if (!onlyEmptyCollections) {
          for (const line of fieldLines) {
            lines.push(line)
          }
        }

        if (binaryFields && binaryFields.length > 0) {
          const parts = []
          const headerText = lines.join("\r\n") + "\r\n"
          parts.push(Buffer.from(headerText))

          for (let i = 0; i < binaryFields.length; i++) {
            const { key, buffer } = binaryFields[i]
            if (i > 0) {
              parts.push(Buffer.from("\r\n"))
            }
            parts.push(Buffer.from(`${key}: `))
            parts.push(buffer)
          }

          const fullBody = Buffer.concat(parts)
          bodyParts.push(new Blob([fullBody]))
        } else {
          lines.push("")
          bodyParts.push(new Blob([lines.join("\r\n")]))
        }
      }
    } else if (Array.isArray(value)) {
      const hasObjects = value.some(item => isPojo(item))
      const hasArrays = value.some(item => Array.isArray(item))
      const nonObjectItems = value
        .map((item, index) => ({ item, index: index + 1 }))
        .filter(({ item }) => !isPojo(item))

      if (hasObjects && nonObjectItems.length > 0) {
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
            const buffer = toBuffer(item)
            if (buffer.length === 0) {
              partTypes.push(`${index}="empty-binary"`)
            }
            partTypes.push(`${index}="binary"`)
          } else if (Array.isArray(item)) {
            partTypes.push(`${index}="list"`)
            const encodedItems = item
              .map(subItem => encodeArrayItem(subItem))
              .join(", ")
            fieldLines.push(`${index}: ${encodedItems}`)
          }
        }

        if (isInline) {
          const orderedLines = []

          for (const line of fieldLines) {
            orderedLines.push(line)
          }

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

          orderedLines.push(lines[0])
          orderedLines.push("")

          bodyParts.push(new Blob([orderedLines.join("\r\n")]))
        } else {
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
        const fieldLines = []
        const partTypes = []

        const allEmpty = value.every(item => {
          if (Array.isArray(item) && item.length === 0) return true
          if (isBytes(item) && (item.length === 0 || item.byteLength === 0))
            return true
          if (isPojo(item) && Object.keys(item).length === 0) return true
          return false
        })

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
          } else if (isPojo(item) && Object.keys(item).length === 0) {
            partTypes.push(`${index}="empty-message"`)
          } else if (isBytes(item)) {
            const buffer = toBuffer(item)
            if (buffer.length === 0 || buffer.byteLength === 0) {
              partTypes.push(`${index}="empty-binary"`)
            } else {
              partTypes.push(`${index}="binary"`)
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
            } else {
              fieldLines.push(`${index}: ${item}`)
            }
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
          }
        })

        if (isInline) {
          const orderedLines = []

          if (fieldLines.length > 0) {
            for (const line of fieldLines) {
              orderedLines.push(line)
            }
          }

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

          orderedLines.push("content-disposition: inline")

          if (!allEmpty) {
            orderedLines.push("")
          }

          bodyParts.push(new Blob([orderedLines.join("\r\n")]))
        } else {
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
      } else if (!hasObjects && value.length === 0) {
        const fieldName = pathParts[pathParts.length - 1]
        const partTypes = [`${fieldName}="empty-list"`]
        lines.unshift(`ao-types: ${partTypes.join(", ")}`)
        lines.push("")
        bodyParts.push(new Blob([lines.join("\r\n")]))
      }
    } else if (typeof value === "string") {
      lines.push("")
      lines.push(value)
      bodyParts.push(new Blob([lines.join("\r\n")]))
    } else if (
      typeof value === "boolean" ||
      typeof value === "number" ||
      value === null ||
      value === undefined ||
      typeof value === "symbol"
    ) {
      let content
      if (typeof value === "boolean") {
        content = `"${value}"`
      } else if (typeof value === "number") {
        content = String(value)
      } else if (value === null) {
        content = '"null"'
      } else if (value === undefined) {
        content = '"undefined"'
      } else if (typeof value === "symbol") {
        content = `"${value.description || "Symbol.for()"}"`
      }

      lines.push("")
      lines.push(content)
      bodyParts.push(new Blob([lines.join("\r\n")]))
    }
  }

  if (
    hasSpecialDataBody &&
    obj.data &&
    obj.data.body &&
    isBytes(obj.data.body)
  ) {
    const buffer = toBuffer(obj.data.body)
    const specialPart = [
      `content-disposition: form-data; name="data/body"`,
      "",
      "",
    ].join("\r\n")
    bodyParts.push(new Blob([specialPart, buffer]))
  }

  const partsContent = await Promise.all(bodyParts.map(part => part.text()))
  const allContent = partsContent.join("")
  const boundaryHash = await sha256(new TextEncoder().encode(allContent))
  const boundary = base64url.encode(Buffer.from(boundaryHash))

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

  const finalContent = await body.arrayBuffer()

  if (finalContent.byteLength > 0) {
    const contentDigest = await sha256(finalContent)
    const base64 = base64url.toBase64(base64url.encode(contentDigest))
    headers["content-digest"] = `sha-256=:${base64}:`
  }

  headers["content-length"] = String(finalContent.byteLength)

  return { headers, body }
}

export async function enc(fields) {
  return await encode(fields)
}
