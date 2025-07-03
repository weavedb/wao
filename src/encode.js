import base64url from "base64url"
import { hash } from "fast-sha256"

function isBytes(value) {
  return (
    value instanceof ArrayBuffer ||
    ArrayBuffer.isView(value) ||
    Buffer.isBuffer(value)
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

function hbEncodeValue(value) {
  if (isBytes(value)) {
    const length = value.byteLength || value.length || 0
    if (length === 0) return ["empty-binary", ""]
    return [undefined, value]
  }

  if (typeof value === "string") {
    if (value.length === 0) return ["empty-string", ""]
    return [undefined, value]
  }

  if (Array.isArray(value)) {
    if (value.length === 0) return ["empty-list", "[]"]

    const hasObjects = value.some(item => isPojo(item))
    if (hasObjects) {
      return ["list_with_objects", value]
    }

    const items = value.map(item => {
      if (typeof item === "string") {
        return `"${item}"`
      } else if (typeof item === "number") {
        if (Number.isInteger(item)) {
          return `"(ao-type-integer) ${item}"`
        } else {
          return `"(ao-type-float) ${item.toExponential(20).replace("e+0", "e+00")}"`
        }
      } else if (typeof item === "boolean") {
        return `"(ao-type-atom) \\"${item ? "true" : "false"}\\""`
      } else if (typeof item === "symbol") {
        const desc = item.description || "symbol"
        return `"(ao-type-atom) \\"${desc}\\""`
      } else if (item === null) {
        return `"(ao-type-atom) \\"null\\""`
      } else if (item === undefined) {
        return `"(ao-type-atom) \\"undefined\\""`
      } else if (isBytes(item)) {
        const length = item.byteLength || item.length || 0
        if (length === 0) {
          return `""`
        } else {
          const base64 = base64url.encode(Buffer.from(item))
          return `"(ao-type-binary) ${base64}"`
        }
      } else if (Array.isArray(item)) {
        const [, encoded] = hbEncodeValue(item)
        const escapedEncoded = encoded.replace(/"/g, '\\"')
        return `"(ao-type-list) ${escapedEncoded}"`
      } else {
        return `"${String(item)}"`
      }
    })

    return ["list", items.join(", ")]
  }

  if (typeof value === "number") {
    if (!Number.isInteger(value)) return ["float", `${value}`]
    return ["integer", String(value)]
  }

  if (typeof value === "boolean") {
    return ["atom", value ? "true" : "false"]
  }

  if (typeof value === "symbol") {
    const desc = value.description || "symbol"
    return ["atom", desc]
  }

  if (value === null) return ["atom", "null"]
  if (value === undefined) return ["atom", "undefined"]

  if (isPojo(value)) {
    throw new Error("Objects must be lifted")
  }

  throw new Error(`Cannot encode value: ${String(value)}`)
}

const MAX_HEADER_LENGTH = 4096

function encode_body_keys(bodyKeys) {
  if (!bodyKeys || bodyKeys.length === 0) return ""
  const items = bodyKeys.map(key => {
    const escaped = key.replace(/"/g, '\\"')
    return `"${escaped}"`
  })
  return items.join(", ")
}

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

function collectParts(obj, path = "", parts = {}, types = {}) {
  for (const [key, value] of Object.entries(obj)) {
    const currentPath = path ? `${path}/${key}` : key

    if (value === null || value === undefined) {
      if (!parts[path]) parts[path] = {}
      parts[path][key] = value === null ? '"null"' : '"undefined"'
      types[currentPath] = "atom"
    } else if (isBytes(value)) {
      if (!parts[path]) parts[path] = {}
      parts[path][key] = value

      const length = value.byteLength || value.length || 0
      if (length === 0) {
        types[currentPath] = "empty-binary"
      }
    } else if (typeof value === "string") {
      if (value.length === 0) {
        types[currentPath] = "empty-binary"
        if (!parts[path]) parts[path] = {}
        parts[path][key] = ""
      } else {
        if (!parts[path]) parts[path] = {}
        parts[path][key] = value
      }
    } else if (Array.isArray(value)) {
      if (value.length === 0) {
        types[currentPath] = "empty-list"
        if (!parts[path]) parts[path] = {}
        parts[path][key] = "[]"
      } else {
        const hasObjects = value.some(item => isPojo(item))

        if (hasObjects) {
          // Arrays with objects: create separate parts for each indexed item
          types[currentPath] = "list"

          value.forEach((item, index) => {
            const indexKey = String(index + 1)
            const indexPath = `${currentPath}/${indexKey}`

            if (isPojo(item)) {
              // Each object in the array becomes a separate part
              if (!parts[indexPath]) parts[indexPath] = {}

              for (const [objKey, objValue] of Object.entries(item)) {
                parts[indexPath][objKey] = objValue

                // Set types for object fields
                const fieldPath = `${indexPath}/${objKey}`
                if (typeof objValue === "number") {
                  types[fieldPath] = Number.isInteger(objValue)
                    ? "integer"
                    : "float"
                } else if (typeof objValue === "boolean") {
                  types[fieldPath] = "atom"
                } else if (typeof objValue === "symbol") {
                  types[fieldPath] = "atom"
                  // Store the symbol's description for later use
                  parts[indexPath][objKey] = objValue
                } else if (
                  typeof objValue === "string" &&
                  objValue.length === 0
                ) {
                  types[fieldPath] = "empty-binary"
                } else if (Array.isArray(objValue) && objValue.length === 0) {
                  types[fieldPath] = "empty-list"
                } else if (
                  isPojo(objValue) &&
                  Object.keys(objValue).length === 0
                ) {
                  types[fieldPath] = "empty-message"
                }
              }
            } else {
              // Non-object items in the array
              if (!parts[currentPath]) parts[currentPath] = {}
              parts[currentPath][indexKey] = item

              if (typeof item === "number") {
                types[`${currentPath}/${indexKey}`] = Number.isInteger(item)
                  ? "integer"
                  : "float"
              } else if (typeof item === "boolean") {
                types[`${currentPath}/${indexKey}`] = "atom"
              }
            }
          })
        } else {
          // Simple arrays without objects
          const [type, encoded] = hbEncodeValue(value)
          if (!parts[path]) parts[path] = {}
          parts[path][key] = encoded
          types[currentPath] = type || "list"
        }
      }
    } else if (isPojo(value)) {
      if (Object.keys(value).length === 0) {
        types[currentPath] = "empty-message"
        if (!parts[path]) parts[path] = {}
        parts[path][key] = "{}"
      } else {
        const hasOnlyEmptyChildren = Object.entries(value).every(([k, v]) => {
          return (
            (Array.isArray(v) && v.length === 0) ||
            (isPojo(v) && Object.keys(v).length === 0)
          )
        })

        if (hasOnlyEmptyChildren) {
          if (!parts[currentPath]) parts[currentPath] = {}
        }

        collectParts(value, currentPath, parts, types)
      }
    } else if (typeof value === "symbol") {
      if (!parts[path]) parts[path] = {}
      parts[path][key] = value.description || "symbol"
      types[currentPath] = "atom"
    } else if (typeof value === "boolean") {
      if (!parts[path]) parts[path] = {}
      parts[path][key] = value
      types[currentPath] = "atom"
    } else {
      if (!parts[path]) parts[path] = {}
      parts[path][key] = value
      if (typeof value === "number") {
        types[currentPath] = Number.isInteger(value) ? "integer" : "float"
      }
    }
  }

  return { parts, types }
}

async function encode(obj = {}) {
  console.log("[encode] START with obj:", JSON.stringify(obj))

  if (Object.keys(obj).length === 0) return { headers: {}, body: undefined }

  // Check if we have a simple binary field
  const objKeys = Object.keys(obj)
  if (objKeys.length === 1 && isBytes(obj[objKeys[0]])) {
    // Single binary field - return it directly
    const fieldName = objKeys[0]
    const binaryData = obj[fieldName]

    const headers = {}
    const bodyBuffer = Buffer.isBuffer(binaryData)
      ? binaryData
      : Buffer.from(binaryData)
    const bodyArrayBuffer = bodyBuffer.buffer.slice(
      bodyBuffer.byteOffset,
      bodyBuffer.byteOffset + bodyBuffer.byteLength
    )

    const contentDigest = await sha256(bodyArrayBuffer)
    const base64 = base64url.toBase64(base64url.encode(contentDigest))
    headers["content-digest"] = `sha-256=:${base64}:`

    console.log(
      "[encode] FINAL (simple binary field) - headers:",
      headers,
      "body:",
      binaryData
    )
    return { headers, body: binaryData }
  }

  if ("body" in obj && isBytes(obj.body)) {
    const headers = {}
    const types = []
    let needsMultipart = false

    for (const [key, value] of Object.entries(obj)) {
      if (key === "body") continue

      if (value === null) {
        headers[key] = "null"
        types.push(`${key}="atom"`)
      } else if (value === undefined) {
        headers[key] = "undefined"
        types.push(`${key}="atom"`)
      } else if (typeof value === "string" && value.length === 0) {
        types.push(`${key}="empty-binary"`)
      } else if (Array.isArray(value)) {
        if (value.length === 0) {
          types.push(`${key}="empty-list"`)
        } else if (value.some(item => isPojo(item))) {
          // Arrays with objects need multipart
          types.push(`${key}="list"`)
          needsMultipart = true
          break
        } else {
          const [type, encoded] = hbEncodeValue(value)
          headers[key] = encoded
          types.push(`${key}="${type}"`)
        }
      } else if (isBytes(value)) {
        if (value.length === 0 || value.byteLength === 0) {
          types.push(`${key}="empty-binary"`)
        } else {
          needsMultipart = true
          break
        }
      } else if (typeof value === "boolean") {
        headers[key] = `"${value}"`
        types.push(`${key}="atom"`)
      } else if (typeof value === "symbol") {
        headers[key] = value.description || "symbol"
        types.push(`${key}="atom"`)
      } else if (typeof value === "number") {
        headers[key] = String(value)
        types.push(`${key}="${Number.isInteger(value) ? "integer" : "float"}"`)
      } else if (typeof value === "string") {
        headers[key] = value
      } else if (isPojo(value) && Object.keys(value).length === 0) {
        types.push(`${key}="empty-message"`)
      } else if (isPojo(value)) {
        needsMultipart = true
        break
      }
    }

    if (needsMultipart) {
      return encodeMultipart(obj)
    }

    if (types.length > 0) {
      headers["ao-types"] = types.sort().join(", ")
    }

    const body = obj.body
    const bodyBuffer = Buffer.isBuffer(body) ? body : Buffer.from(body)
    const bodyArrayBuffer = bodyBuffer.buffer.slice(
      bodyBuffer.byteOffset,
      bodyBuffer.byteOffset + bodyBuffer.byteLength
    )

    const contentDigest = await sha256(bodyArrayBuffer)
    const base64 = base64url.toBase64(base64url.encode(contentDigest))
    headers["content-digest"] = `sha-256=:${base64}:`

    console.log(
      "[encode] FINAL (simple body encoding) - headers:",
      headers,
      "body:",
      body
    )
    return { headers, body }
  }

  return encodeMultipart(obj)
}

async function encodeMultipart(obj) {
  const { parts, types } = collectParts(obj)
  console.log("[encode] Parts:", parts, "Types:", types)

  const headers = {}
  const bodyParts = []
  const bodyKeys = []

  // Collect header types for arrays with objects
  const headerTypes = []
  for (const [key, value] of Object.entries(obj)) {
    if (Array.isArray(value) && value.some(item => isPojo(item))) {
      headerTypes.push(`${key}="list"`)
    }
  }

  for (const [path, content] of Object.entries(parts)) {
    if (path === "") {
      for (const [key, value] of Object.entries(content)) {
        if (isBytes(value)) {
          const length = value.byteLength || value.length || 0
          if (length === 0) {
            // Empty binaries stay in headers
            console.log(`[encode] Empty binary field ${key} staying in headers`)
            continue
          } else {
            console.log(`[encode] Binary field ${key} going to body`)
            bodyKeys.push(key)
          }
        } else {
          const valueStr = String(value)
          const type = types[key]

          if (type === "empty-binary" && valueStr === "") {
            continue
          }

          if (
            !(await hasNewline(valueStr)) &&
            !key.includes("/") &&
            Buffer.from(valueStr).byteLength <= MAX_HEADER_LENGTH &&
            !isPojo(value) &&
            !(key === "data" || key === "body") // Don't put inline keys in headers
          ) {
            if (typeof value === "number") {
              headers[key] = String(value)
            } else if (typeof value === "boolean") {
              headers[key] = `"${value}"`
            } else if (value === "[]" && type === "empty_list") {
              headers[key] = "[]"
            } else if (value === '"null"' || value === '"undefined"') {
              headers[key] = value
            } else {
              headers[key] = value
            }
          } else {
            bodyKeys.push(key)
          }
        }
      }
    } else {
      bodyKeys.push(path)
    }
  }

  console.log(
    "[encode] Headers before filtering:",
    headers,
    "BodyKeys:",
    bodyKeys
  )

  bodyKeys.sort()

  // Add types for values in headers
  for (const [key, value] of Object.entries(headers)) {
    const type = types[key]
    if (type) {
      headerTypes.push(`${key}="${type}"`)
    }
  }

  // Add types for empty values not in headers
  for (const [key, type] of Object.entries(types)) {
    if (!key.includes("/") && type.startsWith("empty") && !headers[key]) {
      headerTypes.push(`${key}="${type.replace("empty_", "empty-")}"`)
    }
  }

  if (headerTypes.length > 0) {
    headers["ao-types"] = headerTypes.sort().join(", ")
  }

  if (bodyKeys.length > 0) {
    headers["body-keys"] = encode_body_keys(bodyKeys)

    if (bodyKeys.includes("data") || bodyKeys.includes("body")) {
      const inlineKey = bodyKeys.find(k => k === "data" || k === "body")
      headers["inline-body-key"] = inlineKey
    }

    // Create multipart body parts
    for (const path of bodyKeys) {
      console.log(`[encode] Processing bodyKey: ${path}`)

      // Handle root-level fields
      if (!path.includes("/")) {
        // This is a root-level field
        const fieldValue = parts[""] && parts[""][path]
        console.log(`[encode] Root field ${path} value:`, fieldValue)

        if (fieldValue !== undefined) {
          if (isBytes(fieldValue)) {
            // Binary field - create a simple multipart section
            // The format should be:
            // content-disposition: form-data;name="fieldname"
            // [empty line]
            // [binary data]
            const headerStr = `content-disposition: form-data;name="${path}"\r\n\r\n`
            const headerBlob = new Blob([headerStr])
            const dataBlob = new Blob([fieldValue])
            bodyParts.push(new Blob([headerBlob, dataBlob]))
            console.log(`[encode] Added binary field ${path} to bodyParts`)
            continue
          } else {
            // Non-binary root field that needs to go in body
            const lines = []

            // Check if this is an inline key
            const isInlineKey = path === "data" || path === "body"

            if (isInlineKey) {
              lines.push(`content-disposition: inline`)
            } else {
              lines.push(`content-disposition: form-data;name="${path}"`)
            }

            // Add the field type if available
            const type = types[path]
            if (type) {
              lines.push(`ao-types: ${path}="${type}"`)
            }

            // Add empty line before content
            lines.push("")

            // Add the actual value
            lines.push(String(fieldValue))

            bodyParts.push(new Blob([lines.join("\r\n")]))
            console.log(`[encode] Added non-binary field ${path} to bodyParts`)
            continue
          }
        }
      }

      // Handle nested paths
      const content = parts[path]
      if (!content) {
        console.log(`[encode] No content found for path: ${path}`)
        continue
      }

      const lines = []
      const binaryParts = []
      const isInlineKey =
        (path === "data" || path === "body") && !path.includes("/")

      const sortedKeys = Object.keys(content).sort()

      // Collect ao-types for this part
      const partTypes = []
      for (const key of sortedKeys) {
        const fullPath = path ? `${path}/${key}` : key
        const type = types[fullPath]
        if (type) {
          partTypes.push(`${key}="${type}"`)
        }
      }

      if (partTypes.length > 0) {
        lines.push(`ao-types: ${partTypes.sort().join(", ")}`)
      }

      if (isInlineKey) {
        lines.push(`content-disposition: inline`)
      } else {
        lines.push(`content-disposition: form-data;name="${path}"`)
      }

      // Add content fields
      for (const key of sortedKeys) {
        const value = content[key]
        const fullPath = path ? `${path}/${key}` : key
        const type = types[fullPath]

        if (
          type === "empty-message" ||
          type === "empty-list" ||
          type === "empty-binary"
        ) {
          continue
        }

        if (
          (value === "[]" || value === "{}" || value === "") &&
          type &&
          type.startsWith("empty")
        ) {
          continue
        }

        if (isBytes(value)) {
          binaryParts.push({ key, value })
        } else if (type === "atom" && typeof value === "boolean") {
          lines.push(`${key}: "${value}"`)
        } else if (typeof value === "symbol") {
          // Handle Symbol values
          const symbolValue = value.description || "symbol"
          lines.push(`${key}: ${symbolValue}`)
        } else {
          lines.push(`${key}: ${value}`)
        }
      }

      if (lines.length >= 1 || binaryParts.length > 0) {
        if (binaryParts.length > 0) {
          const allParts = []

          // Add text headers first
          if (lines.length > 0) {
            allParts.push(lines.join("\r\n"))
            allParts.push("\r\n")
          }

          // Add binary data
          for (const binaryPart of binaryParts) {
            allParts.push(`${binaryPart.key}: `)
            allParts.push(binaryPart.value)
            if (binaryParts.indexOf(binaryPart) < binaryParts.length - 1) {
              allParts.push("\r\n")
            }
          }

          bodyParts.push(new Blob(allParts))
        } else {
          bodyParts.push(new Blob([lines.join("\r\n")]))
        }
      }
    }

    console.log(`[encode] Total bodyParts: ${bodyParts.length}`)

    // Create boundary based on parts content
    const partsForBoundary = []
    for (const part of bodyParts) {
      const partContent = await part.text()
      partsForBoundary.push(partContent)
    }
    const allPartsContent = partsForBoundary.join("")
    console.log(`[encode] All parts content length: ${allPartsContent.length}`)

    const allPartsBuffer = Buffer.from(allPartsContent)
    const hashResult = await sha256(
      allPartsBuffer.buffer.slice(
        allPartsBuffer.byteOffset,
        allPartsBuffer.byteOffset + allPartsBuffer.byteLength
      )
    )
    const boundary = base64url.encode(Buffer.from(hashResult))

    // Create final multipart body
    const finalParts = []
    for (let i = 0; i < bodyParts.length; i++) {
      finalParts.push(`--${boundary}`)
      finalParts.push(`\r\n`)
      finalParts.push(bodyParts[i])
      if (i < bodyParts.length - 1) {
        finalParts.push(`\r\n`)
      }
    }
    finalParts.push(`\r\n--${boundary}--`)

    headers["content-type"] = `multipart/form-data; boundary="${boundary}"`
    const body = new Blob(finalParts)

    const finalContent = await body.arrayBuffer()
    const contentDigest = await sha256(finalContent)
    const base64 = base64url.toBase64(base64url.encode(contentDigest))
    headers["content-digest"] = `sha-256=:${base64}:`
    headers["content-length"] = String(finalContent.byteLength)

    console.log("[encode] FINAL - headers:", headers, "body:", body)
    return { headers, body }
  }

  console.log("[encode] FINAL - headers:", headers, "body:", undefined)
  return { headers, body: undefined }
}

export async function enc(fields) {
  return await encode(fields)
}
