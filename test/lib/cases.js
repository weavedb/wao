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
        } else if (nestedItem === undefined) {
          return `\\"(ao-type-atom) \\\\\\"undefined\\\\\\"\\"`
        } else if (typeof nestedItem === "symbol") {
          const desc = nestedItem.description || "Symbol.for()"
          return `\\"(ao-type-atom) \\\\\\"${desc}\\\\\\"\\"`
        } else if (typeof nestedItem === "boolean") {
          return `\\"(ao-type-atom) \\\\\\"${nestedItem}\\\\\\"\\"`
        } else if (Array.isArray(nestedItem)) {
          // Handle nested arrays recursively
          const deeperItems = nestedItem
            .map(deepItem => {
              if (typeof deepItem === "number") {
                if (Number.isInteger(deepItem)) {
                  return `\\\\\\"(ao-type-integer) ${deepItem}\\\\\\"`
                } else {
                  return `\\\\\\"(ao-type-float) ${formatFloat(deepItem)}\\\\\\"`
                }
              } else if (typeof deepItem === "string") {
                return `\\\\\\"${deepItem}\\\\\\"`
              } else if (Array.isArray(deepItem)) {
                // Even deeper nesting - need to escape more
                const deepestItems = deepItem
                  .map(deepestItem => {
                    if (typeof deepestItem === "number") {
                      if (Number.isInteger(deepestItem)) {
                        return `\\\\\\\\\\\\\\"(ao-type-integer) ${deepestItem}\\\\\\\\\\\\\\"`
                      } else {
                        return `\\\\\\\\\\\\\\"(ao-type-float) ${formatFloat(deepestItem)}\\\\\\\\\\\\\\"`
                      }
                    } else if (typeof deepestItem === "string") {
                      return `\\\\\\\\\\\\\\"${deepestItem}\\\\\\\\\\\\\\"`
                    } else {
                      return `\\\\\\\\\\\\\\"${String(deepestItem)}\\\\\\\\\\\\\\"`
                    }
                  })
                  .join(", ")
                return `\\\\\\"(ao-type-list) ${deepestItems}\\\\\\"`
              } else if (deepItem === null) {
                return `\\\\\\"(ao-type-atom) \\\\\\\\\\\\\\"null\\\\\\\\\\\\\\"\\\\\\"`
              } else if (deepItem === undefined) {
                return `\\\\\\"(ao-type-atom) \\\\\\\\\\\\\\"undefined\\\\\\\\\\\\\\"\\\\\\"`
              } else if (typeof deepItem === "symbol") {
                const desc = deepItem.description || "Symbol.for()"
                return `\\\\\\"(ao-type-atom) \\\\\\\\\\\\\\"${desc}\\\\\\\\\\\\\\"\\\\\\"`
              } else if (typeof deepItem === "boolean") {
                return `\\\\\\"(ao-type-atom) \\\\\\\\\\\\\\"${deepItem}\\\\\\\\\\\\\\"\\\\\\"`
              } else {
                return `\\\\\\"${String(deepItem)}\\\\\\"`
              }
            })
            .join(", ")
          return `\\"(ao-type-list) ${deeperItems}\\"`
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
  console.log("=== collectBodyKeys START ===")
  console.log("Input object:", JSON.stringify(obj))

  const keys = []

  function traverse(current, path) {
    console.log(`[traverse] Called with path: "${path}"`)
    let hasSimpleFields = false
    const nestedPaths = []
    let hasArraysWithObjects = false

    for (const [key, value] of Object.entries(current)) {
      const fullPath = path ? `${path}/${key}` : key

      if (Array.isArray(value)) {
        console.log(
          `[traverse] Found array at ${fullPath}, length: ${value.length}`
        )
        const hasObjects = value.some(item => isPojo(item))
        const hasNonObjects = value.some(item => !isPojo(item))

        if (value.length === 0) {
          console.log(
            `[traverse] Empty array at ${fullPath} - marking parent as having simple fields`
          )
          hasSimpleFields = true
        } else if (hasObjects) {
          hasArraysWithObjects = true
          // Check if we need special handling for mixed arrays
          const hasEmptyStrings = value.some(
            item => typeof item === "string" && item === ""
          )
          const hasEmptyObjects = value.some(
            item => isPojo(item) && Object.keys(item).length === 0
          )
          const hasNonEmptyObjects = value.some(
            item => isPojo(item) && Object.keys(item).length > 0
          )

          // Check if objects contain only empty values (not empty objects)
          const hasObjectsWithOnlyEmptyValues = value.some(item => {
            if (!isPojo(item) || Object.keys(item).length === 0) return false
            return Object.values(item).every(
              v =>
                (typeof v === "string" && v === "") ||
                (Array.isArray(v) && v.length === 0) ||
                (isPojo(v) && Object.keys(v).length === 0)
            )
          })

          // Only use special handling if we have BOTH empty elements AND non-empty objects
          if ((hasEmptyStrings || hasEmptyObjects) && hasNonEmptyObjects) {
            // Special case: mixed array with empty strings/objects - only non-empty objects get parts
            value.forEach((item, index) => {
              if (isPojo(item) && Object.keys(item).length > 0) {
                const itemPath = `${fullPath}/${index + 1}`
                keys.push(itemPath)
                nestedPaths.push(itemPath)
              }
            })
            if (hasNonObjects) {
              hasSimpleFields = true
              keys.push(fullPath)
            }
          } else if (hasObjectsWithOnlyEmptyValues && !hasNonObjects) {
            // Special case: objects that contain only empty values should get parts
            value.forEach((item, index) => {
              if (isPojo(item)) {
                const itemPath = `${fullPath}/${index + 1}`
                keys.push(itemPath)
                if (Object.keys(item).length > 0) {
                  nestedPaths.push(itemPath)
                }
              }
            })
          } else {
            // Normal case: all objects get parts
            value.forEach((item, index) => {
              if (isPojo(item)) {
                const itemPath = `${fullPath}/${index + 1}`
                keys.push(itemPath)
                if (Object.keys(item).length > 0) {
                  nestedPaths.push(itemPath)
                }
              }
            })
            if (hasNonObjects) {
              hasSimpleFields = true
              keys.push(fullPath)
            }
          }
        } else {
          console.log(
            `[traverse] Non-empty array without objects at ${fullPath} - marking as simple field`
          )
          hasSimpleFields = true
        }
      } else if (isPojo(value)) {
        if (Object.keys(value).length === 0) {
          console.log(
            `[traverse] Empty object at ${fullPath} - marking parent as having simple fields`
          )
          hasSimpleFields = true
        } else {
          // Don't traverse into the object if it only contains empty values
          const containsOnlyEmptyCollections = Object.entries(value).every(
            ([k, v]) => {
              return (
                (Array.isArray(v) && v.length === 0) ||
                (isPojo(v) && Object.keys(v).length === 0) ||
                (isBytes(v) && (v.length === 0 || v.byteLength === 0)) ||
                (typeof v === "string" && v.length === 0)
              )
            }
          )

          if (containsOnlyEmptyCollections && Object.keys(value).length > 0) {
            console.log(
              `[traverse] Object at ${fullPath} contains only empty collections - adding as body key`
            )
            keys.push(fullPath)
          } else {
            // Check if this object contains arrays with only empty elements
            const hasArraysWithOnlyEmptyElements = Object.entries(value).some(
              ([k, v]) => {
                return (
                  Array.isArray(v) &&
                  v.length > 0 &&
                  v.every(
                    item =>
                      (Array.isArray(item) && item.length === 0) ||
                      (isPojo(item) && Object.keys(item).length === 0) ||
                      (typeof item === "string" && item === "")
                  )
                )
              }
            )

            if (hasArraysWithOnlyEmptyElements) {
              // This object needs a body part to show its array types
              console.log(
                `[traverse] Object at ${fullPath} has arrays with empty elements - adding as body key`
              )
              keys.push(fullPath)
            }

            console.log(
              `[traverse] Non-empty object at ${fullPath} - will traverse into it`
            )
            nestedPaths.push(fullPath)
          }
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
      console.log(`[traverse] Adding "${path}" to keys (has simple fields)`)
      keys.push(path)
    } else if (hasArraysWithObjects && path) {
      // If the object only contains arrays with objects, we still need to add it as a body key
      console.log(
        `[traverse] Adding "${path}" to keys (contains arrays with objects)`
      )
      keys.push(path)
    }

    // Check for arrays with only empty elements that need their own body parts
    for (const [key, value] of Object.entries(current)) {
      const fullPath = path ? `${path}/${key}` : key

      if (Array.isArray(value) && value.length > 0) {
        const hasOnlyEmptyElements = value.every(
          item =>
            (Array.isArray(item) && item.length === 0) ||
            (isPojo(item) && Object.keys(item).length === 0) ||
            (typeof item === "string" && item === "")
        )

        if (hasOnlyEmptyElements) {
          console.log(
            `[traverse] Array at ${fullPath} has only empty elements - adding as body key`
          )
          keys.push(fullPath)
        }
      }
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
    console.log(`\n[main loop] Processing key: "${key}"`)
    console.log(
      `[main loop] Value type: ${Array.isArray(value) ? "array" : typeof value}`
    )
    console.log(
      `[main loop] Array length: ${Array.isArray(value) ? value.length : "N/A"}`
    )

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
      // Special handling: only add to body keys if there's no other data/body field with an object
      if (
        key === "data" &&
        obj.body &&
        isPojo(obj.body) &&
        Object.keys(obj.body).length > 0
      ) {
        console.log(`[main loop] Skipping special data field`)
      } else if (
        key === "body" &&
        obj.data &&
        isPojo(obj.data) &&
        Object.keys(obj.data).length > 0
      ) {
        console.log(`[main loop] Skipping special body field`)
      } else {
        console.log(`[main loop] Adding special data/body key: "${key}"`)
        keys.push(key)
      }
    } else if (Array.isArray(value)) {
      if (value.length === 0) {
        console.log(`[main loop] SKIPPING empty array for key: "${key}"`)
        continue
      }

      const hasObjects = value.some(item => isPojo(item))
      const hasArrays = value.some(item => Array.isArray(item))
      const hasNonObjects = value.some(item => !isPojo(item))

      // Check if this is an array of arrays containing objects
      const hasArraysOfObjects = value.some(
        item => Array.isArray(item) && item.some(subItem => isPojo(subItem))
      )

      console.log(
        `[main loop] Array analysis: hasObjects=${hasObjects}, hasArrays=${hasArrays}, hasNonObjects=${hasNonObjects}, hasArraysOfObjects=${hasArraysOfObjects}`
      )

      if (value.length > 0) {
        let bodyPartCounter = 1 // Start counting from 1

        // Check for special mixed array case
        const hasEmptyStrings = value.some(
          item => typeof item === "string" && item === ""
        )
        const hasEmptyObjects = value.some(
          item => isPojo(item) && Object.keys(item).length === 0
        )

        // Check for objects that contain only empty values
        const hasObjectsWithOnlyEmptyValues = value.some(item => {
          if (!isPojo(item) || Object.keys(item).length === 0) return false
          return Object.values(item).every(
            v =>
              (typeof v === "string" && v === "") ||
              (Array.isArray(v) && v.length === 0) ||
              (isPojo(v) && Object.keys(v).length === 0)
          )
        })

        if (hasArraysOfObjects) {
          // Handle arrays of arrays containing objects
          value.forEach((item, index) => {
            if (Array.isArray(item)) {
              item.forEach((subItem, subIndex) => {
                if (isPojo(subItem)) {
                  const path = `${key}/${index + 1}/${subIndex + 1}`
                  console.log(
                    `[main loop] Adding nested object path: "${path}"`
                  )
                  keys.push(path)
                }
              })
            }
            bodyPartCounter++
          })
          // Always add the main array key
          console.log(`[main loop] ADDING main array key: "${key}"`)
          keys.push(key)
        } else if (
          hasObjects &&
          (hasEmptyStrings || hasEmptyObjects) &&
          !hasObjectsWithOnlyEmptyValues
        ) {
          // Special handling: only non-empty objects get parts
          value.forEach((item, index) => {
            if (isPojo(item) && Object.keys(item).length > 0) {
              const path = `${key}/${bodyPartCounter}`
              console.log(
                `[main loop] Adding non-empty object path: "${path}" (array index ${index})`
              )
              keys.push(path)
              // Add paths for nested objects
              for (const [nestedKey, nestedValue] of Object.entries(item)) {
                if (isPojo(nestedValue)) {
                  const nestedPath = `${key}/${bodyPartCounter}/${nestedKey}`
                  console.log(
                    `[main loop] Adding nested object path: "${nestedPath}"`
                  )
                  keys.push(nestedPath)
                }
              }
            }
            bodyPartCounter++
          })
          // Always add the main array key
          console.log(`[main loop] ADDING main array key: "${key}"`)
          keys.push(key)
        } else if (hasObjects) {
          // Normal handling: all objects get parts (except if parent array has only empty elements)
          let skipEmptyObjects = false

          // Check if this array contains only empty elements
          const arrayHasOnlyEmptyElements = value.every(
            item =>
              (Array.isArray(item) && item.length === 0) ||
              (isPojo(item) && Object.keys(item).length === 0) ||
              (typeof item === "string" && item === "")
          )

          if (arrayHasOnlyEmptyElements) {
            skipEmptyObjects = true
          }

          value.forEach((item, index) => {
            if (isPojo(item)) {
              // Skip empty objects if array has only empty elements
              if (skipEmptyObjects && Object.keys(item).length === 0) {
                bodyPartCounter++
                return
              }

              const path = `${key}/${bodyPartCounter}`
              console.log(
                `[main loop] Adding object path: "${path}" (array index ${index}, empty=${Object.keys(item).length === 0})`
              )
              keys.push(path)
              // Add paths for nested objects (but not empty ones)
              if (Object.keys(item).length > 0) {
                for (const [nestedKey, nestedValue] of Object.entries(item)) {
                  if (
                    isPojo(nestedValue) &&
                    Object.keys(nestedValue).length > 0
                  ) {
                    const nestedPath = `${key}/${bodyPartCounter}/${nestedKey}`
                    console.log(
                      `[main loop] Adding nested object path: "${nestedPath}"`
                    )
                    keys.push(nestedPath)
                  }
                }
              }
            } else if (typeof item === "string" && item === "") {
              // Empty strings may get parts in some formats
              const path = `${key}/${bodyPartCounter}`
              console.log(
                `[main loop] Adding empty string path: "${path}" (array index ${index})`
              )
              keys.push(path)
            }
            bodyPartCounter++
          })
          // Don't add main array key for arrays with only objects containing empty values
          if (
            !hasObjectsWithOnlyEmptyValues ||
            value.some(item => !isPojo(item))
          ) {
            // Check if array has only empty elements
            const hasOnlyEmptyElements = value.every(
              item =>
                (Array.isArray(item) && item.length === 0) ||
                (isPojo(item) && Object.keys(item).length === 0) ||
                (typeof item === "string" && item === "")
            )

            if (!hasOnlyEmptyElements) {
              // Always add the main array key
              console.log(`[main loop] ADDING main array key: "${key}"`)
              keys.push(key)
            }
          }
        } else {
          // Check if array has only empty elements
          const hasOnlyEmptyArraysOrObjects = value.every(
            item =>
              (Array.isArray(item) && item.length === 0) ||
              (isPojo(item) && Object.keys(item).length === 0) ||
              (typeof item === "string" && item === "")
          )

          if (hasOnlyEmptyArraysOrObjects && value.length > 0) {
            // Always add the main array key for arrays with only empty elements
            console.log(
              `[main loop] ADDING main array key for empty elements: "${key}"`
            )
            keys.push(key)
          } else if (!hasOnlyEmptyArraysOrObjects) {
            // Always add the main array key
            console.log(`[main loop] ADDING main array key: "${key}"`)
            keys.push(key)
          }
        }
      }
    } else if (isPojo(value)) {
      console.log(`[main loop] Traversing object at key: "${key}"`)
      // Check if this object contains arrays with only empty elements
      let hasArraysWithOnlyEmptyElements = false
      for (const [k, v] of Object.entries(value)) {
        if (
          Array.isArray(v) &&
          v.length > 0 &&
          v.every(
            item =>
              (Array.isArray(item) && item.length === 0) ||
              (isPojo(item) && Object.keys(item).length === 0) ||
              (typeof item === "string" && item === "")
          )
        ) {
          hasArraysWithOnlyEmptyElements = true
          keys.push(`${key}/${k}`)
        }
      }
      traverse(value, key)
    } else if (isBytes(value) && value.length > 0) {
      console.log(`[main loop] Adding key for non-empty bytes: "${key}"`)
      keys.push(key)
    } else if (typeof value === "string" && value.includes("\n")) {
      console.log(`[main loop] Adding key for string with newline: "${key}"`)
      keys.push(key)
    } else if (typeof value === "string" && hasNonAscii(value)) {
      console.log(`[main loop] Adding key for non-ASCII string: "${key}"`)
      keys.push(key)
    } else {
      console.log(`[main loop] Skipping key: "${key}" (no match)`)
    }
  }

  const result = [...new Set(keys)].filter(k => {
    if (k === "") return false

    // Check if this is a path to an empty object inside an array with only empty elements
    const parts = k.split("/")
    if (parts.length >= 2 && /^\d+$/.test(parts[parts.length - 1])) {
      // This is an array element path like "maps/1"
      const arrayPath = parts.slice(0, -1).join("/")
      let arrayValue = obj

      // Navigate to the array
      for (const part of parts.slice(0, -1)) {
        if (/^\d+$/.test(part)) {
          arrayValue = arrayValue[parseInt(part) - 1]
        } else {
          arrayValue = arrayValue[part]
        }
      }

      // Check if this array contains only empty elements
      if (Array.isArray(arrayValue)) {
        const hasOnlyEmptyElements = arrayValue.every(
          item =>
            (Array.isArray(item) && item.length === 0) ||
            (isPojo(item) && Object.keys(item).length === 0) ||
            (typeof item === "string" && item === "")
        )

        if (hasOnlyEmptyElements) {
          // Filter out paths to individual empty elements
          console.log(`[filter] Removing path to empty element: "${k}"`)
          return false
        }
      }
    }

    return true
  })
  console.log("\n=== collectBodyKeys RESULT ===")
  console.log("Final bodyKeys:", JSON.stringify(result))
  console.log("=== collectBodyKeys END ===\n")

  return result
}

async function encode(obj = {}) {
  console.log("\n=== ENCODE START ===")
  console.log("Encoding object:", JSON.stringify(obj))

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
      headers["ao-types"] = `${fieldName.toLowerCase()}="empty-binary"`
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
        headers["ao-types"] = `${fieldName.toLowerCase()}="atom"`
      } else if (typeof fieldValue === "number") {
        headers["ao-types"] =
          `${fieldName.toLowerCase()}="${Number.isInteger(fieldValue) ? "integer" : "float"}"`
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
      if (isBytes(value) && (value.length === 0 || value.byteLength === 0)) {
        headerTypes.push(`${key.toLowerCase()}="empty-binary"`)
      } else if (typeof value === "string" && value.length === 0) {
        headerTypes.push(`${key.toLowerCase()}="empty-binary"`)
      } else if (Array.isArray(value) && value.length === 0) {
        headerTypes.push(`${key.toLowerCase()}="empty-list"`)
      } else if (isPojo(value) && Object.keys(value).length === 0) {
        headerTypes.push(`${key.toLowerCase()}="empty-message"`)
      } else if (
        typeof value === "boolean" ||
        value === null ||
        value === undefined ||
        typeof value === "symbol"
      ) {
        headerTypes.push(`${key.toLowerCase()}="atom"`)
      } else if (typeof value === "number") {
        headerTypes.push(
          `${key.toLowerCase()}="${Number.isInteger(value) ? "integer" : "float"}"`
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
        if (!headerTypes.some(t => t.startsWith(`${key.toLowerCase()}=`))) {
          headerTypes.push(`${key.toLowerCase()}="list"`)
        }
      }
    }
  }

  if (bodyKeys.length === 0) {
    console.log("No bodyKeys, returning headers only")
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
        const index = parseInt(part) - 1
        console.log(
          `[Body part] Getting array element at index ${index} from part ${part}`
        )
        value = value[index]
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

  // Sort body keys: main array comes first, then element parts by index
  const sortedBodyKeys = bodyKeys.sort((a, b) => {
    const aIsArrayElement = /\/\d+$/.test(a)
    const bIsArrayElement = /\/\d+$/.test(b)
    const aBase = a.split("/")[0]
    const bBase = b.split("/")[0]

    // If both are for the same array
    if (aBase === bBase) {
      // Main array comes before element parts
      if (!aIsArrayElement && bIsArrayElement) {
        return -1 // main array comes first
      }
      if (aIsArrayElement && !bIsArrayElement) {
        return 1 // element parts come after
      }
      // Both are elements - sort by index
      if (aIsArrayElement && bIsArrayElement) {
        const aIndex = parseInt(a.split("/")[1])
        const bIndex = parseInt(b.split("/")[1])
        return aIndex - bIndex
      }
      return a.localeCompare(b)
    }

    // Different arrays, sort by base name
    return a.localeCompare(b)
  })

  // Check if we have the special case where data contains body with bytes and body contains data with bytes
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
    console.log(`\n[Body part] Processing bodyKey: ${bodyKey}`)
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

    console.log(`[Body part] Value at ${bodyKey}:`, JSON.stringify(value))

    // Special handling for empty strings in arrays
    if (typeof value === "string" && value === "" && pathParts.length > 1) {
      console.log(`[Body part] Creating part for empty string at ${bodyKey}`)
      lines.push(`content-disposition: form-data;name="${bodyKey}"`)
      lines.push("")
      lines.push("")
      bodyParts.push(new Blob([lines.join("\r\n")]))
      continue
    }

    if (Array.isArray(value)) {
      const hasOnlyEmptyElements =
        value.length > 0 &&
        value.every(
          item =>
            (Array.isArray(item) && item.length === 0) ||
            (isPojo(item) && Object.keys(item).length === 0) ||
            (typeof item === "string" && item === "")
        )
      const hasOnlyNonEmptyObjects =
        value.length > 0 &&
        value.every(item => isPojo(item) && Object.keys(item).length > 0)
      const hasOnlyEmptyObjects =
        value.length > 0 &&
        value.every(item => isPojo(item) && Object.keys(item).length === 0)
      const hasObjects = value.some(item => isPojo(item))
      const hasArrays = value.some(item => Array.isArray(item))
      const nonObjectItems = value
        .map((item, index) => ({ item, index: index + 1 }))
        .filter(({ item }) => !isPojo(item))

      if (hasOnlyNonEmptyObjects) {
        continue
      }

      // For arrays containing only empty elements, we still need to show type info
      if (hasOnlyEmptyElements) {
        const fieldLines = []
        const partTypes = []

        // Process items for type information
        value.forEach((item, idx) => {
          const index = idx + 1
          if (Array.isArray(item) && item.length === 0) {
            partTypes.push(`${index}="empty-list"`)
          } else if (isPojo(item) && Object.keys(item).length === 0) {
            partTypes.push(`${index}="empty-message"`)
          } else if (typeof item === "string" && item === "") {
            partTypes.push(`${index}="empty-binary"`)
          }
        })

        const isInline =
          bodyKey === "body" && headers["inline-body-key"] === "body"

        if (isInline) {
          const orderedLines = []
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
          orderedLines.push("")
          bodyParts.push(new Blob([orderedLines.join("\r\n")]))
        } else {
          const orderedLines = []
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
          orderedLines.push(`content-disposition: form-data;name="${bodyKey}"`)

          // Check if this is the last body part
          const isLastBodyPart =
            sortedBodyKeys.indexOf(bodyKey) === sortedBodyKeys.length - 1
          const hasOnlyTypes = partTypes.length > 0 && fieldLines.length === 0

          if (isLastBodyPart && hasOnlyTypes) {
            // Don't add empty line for last part with only types
            bodyParts.push(new Blob([orderedLines.join("\r\n")]))
          } else {
            orderedLines.push("")
            bodyParts.push(new Blob([orderedLines.join("\r\n")]))
          }
        }
        continue
      }

      // Build list of which indices have their own parts
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

      // Check if this array contains sub-arrays with objects that have their own parts
      const hasNestedObjectParts = sortedBodyKeys.some(
        key =>
          key.startsWith(bodyKey + "/") &&
          key.split("/").length > pathParts.length + 1
      )

      const fieldLines = []
      const partTypes = []

      // For arrays that contain sub-arrays with objects, we need to add type info for the sub-arrays
      if (hasNestedObjectParts) {
        value.forEach((item, idx) => {
          const index = idx + 1
          if (Array.isArray(item)) {
            partTypes.push(`${index}="list"`)
          }
        })
      }

      // Check if this array has mixed content with empty strings/objects
      const hasEmptyStrings = value.some(
        item => typeof item === "string" && item === ""
      )
      const hasEmptyObjects = value.some(
        item => isPojo(item) && Object.keys(item).length === 0
      )

      // Check if we have objects with only empty values (like {empty: ""})
      const hasObjectsWithOnlyEmptyValues = value.some(item => {
        if (!isPojo(item) || Object.keys(item).length === 0) return false
        return Object.values(item).every(
          v =>
            (typeof v === "string" && v === "") ||
            (Array.isArray(v) && v.length === 0) ||
            (isPojo(v) && Object.keys(v).length === 0)
        )
      })

      const isMixedArray =
        hasObjects &&
        (hasEmptyStrings || hasEmptyObjects) &&
        !hasObjectsWithOnlyEmptyValues

      // Process ALL items for type information
      value.forEach((item, idx) => {
        const index = idx + 1

        // Skip type info for elements that have their own parts
        if (indicesWithOwnParts.has(index)) {
          return
        }

        // If we have nested object parts and this is an array with objects, skip processing it inline
        if (
          hasNestedObjectParts &&
          Array.isArray(item) &&
          item.some(subItem => isPojo(subItem))
        ) {
          // Type info already added above
          return
        }

        // For all arrays (not just mixed ones), we need to process all items
        if (typeof item === "string" && item === "") {
          // Empty strings get type annotation but no field line
          partTypes.push(`${index}="empty-binary"`)
        } else if (isPojo(item) && Object.keys(item).length === 0) {
          // Empty objects don't get field lines but do get type annotations
          partTypes.push(`${index}="empty-message"`)
        } else if (isPojo(item)) {
          // Non-empty objects might have parts
        } else if (Array.isArray(item)) {
          if (item.length === 0) {
            // Empty arrays don't get field lines but do get type annotations
            partTypes.push(`${index}="empty-list"`)
          } else {
            partTypes.push(`${index}="list"`)
            // Encode array
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
                  // Use the full encodeArrayItem for nested arrays
                  return encodeArrayItem(subItem)
                } else if (isBytes(subItem)) {
                  const buffer = toBuffer(subItem)
                  if (buffer.length === 0 || buffer.byteLength === 0) {
                    return `""`
                  }
                  return `"(ao-type-binary)"`
                } else if (isPojo(subItem)) {
                  const json = JSON.stringify(subItem)
                  const escaped = json
                    .replace(/\\/g, "\\\\")
                    .replace(/"/g, '\\"')
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
          // Non-empty strings just get field lines, no type annotation
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

      const isInline =
        bodyKey === "body" && headers["inline-body-key"] === "body"

      if (isInline) {
        const orderedLines = []

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

        if (fieldLines.length > 0) {
          orderedLines.push("")
          for (const line of fieldLines) {
            orderedLines.push(line)
          }
        }

        bodyParts.push(new Blob([orderedLines.join("\r\n") + "\r\n"]))
      } else {
        // Put ao-types first, then content-disposition, then field lines
        const orderedLines = []

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

        orderedLines.push(`content-disposition: form-data;name="${bodyKey}"`)

        // Add field lines directly without blank line
        for (const line of fieldLines) {
          orderedLines.push(line)
        }

        // Add trailing blank line if we have field lines
        if (fieldLines.length > 0) {
          bodyParts.push(new Blob([orderedLines.join("\r\n") + "\r\n"]))
        } else {
          bodyParts.push(new Blob([orderedLines.join("\r\n")]))
        }
      }
      continue
    }

    if (isPojo(value)) {
      // Handle non-empty objects - skip empty ones
      if (Object.keys(value).length === 0) {
        // Check if this is a parent array context where empty objects should get parts
        const parentPath = pathParts.slice(0, -1).join("/")
        let parentValue = obj
        for (const part of pathParts.slice(0, -1)) {
          if (/^\d+$/.test(part)) {
            parentValue = parentValue[parseInt(part) - 1]
          } else {
            parentValue = parentValue[part]
          }
        }

        if (Array.isArray(parentValue)) {
          // Check if parent array has mixed content
          const hasEmptyStrings = parentValue.some(
            item => typeof item === "string" && item === ""
          )
          const hasEmptyObjects = parentValue.some(
            item => isPojo(item) && Object.keys(item).length === 0
          )
          const hasObjects = parentValue.some(item => isPojo(item))

          if (hasObjects && (hasEmptyStrings || hasEmptyObjects)) {
            // Special mixed array case - empty objects don't get parts
            console.log(
              `[Body part] Skipping empty object in mixed array at ${bodyKey}`
            )
            continue
          }
        }

        // Normal case - empty objects might get parts
        console.log(`[Body part] Empty object at ${bodyKey}`)
        // For now, skip empty objects
        continue
      }

      // Skip the special case where bodyKey is "data" and has only body:Buffer
      if (
        hasSpecialDataBody &&
        bodyKey === "data" &&
        Object.keys(value).length === 1 &&
        value.body &&
        isBytes(value.body)
      ) {
        continue
      }

      // Handle non-empty objects
      const isInline =
        bodyKey === "body" && headers["inline-body-key"] === "body"
      if (isInline) {
        lines.push(`content-disposition: inline`)
      } else {
        lines.push(`content-disposition: form-data;name="${bodyKey}"`)
      }

      // Continue with object processing...
      const objectTypes = []
      const fieldLines = []
      const binaryFields = []

      // First check if this object only contains empty collections
      const hasOnlyEmptyCollections = Object.entries(value).every(([k, v]) => {
        return (
          (Array.isArray(v) && v.length === 0) ||
          (isPojo(v) && Object.keys(v).length === 0) ||
          (isBytes(v) && (v.length === 0 || v.byteLength === 0)) ||
          (typeof v === "string" && v.length === 0)
        )
      })

      // Also check if it contains arrays with only empty elements
      const hasArraysWithOnlyEmptyElements = Object.entries(value).some(
        ([k, v]) => {
          return (
            Array.isArray(v) &&
            v.length > 0 &&
            v.every(
              item =>
                (Array.isArray(item) && item.length === 0) ||
                (isPojo(item) && Object.keys(item).length === 0) ||
                (typeof item === "string" && item === "")
            )
          )
        }
      )

      // Collect type information for arrays in the object BEFORE processing fields
      const arrayTypes = []
      for (const [k, v] of Object.entries(value)) {
        if (Array.isArray(v)) {
          arrayTypes.push(
            `${k.toLowerCase()}="${v.length === 0 ? "empty-list" : "list"}"`
          )
        }
      }

      for (const [k, v] of Object.entries(value)) {
        const childPath = `${bodyKey}/${k}`

        if (sortedBodyKeys.includes(childPath)) {
          continue
        }

        if (Array.isArray(v) && v.some(item => isPojo(item))) {
          // Check if this array will have its own body part
          if (sortedBodyKeys.includes(childPath)) {
            continue
          }
          // Check if array has only empty elements
          const hasOnlyEmpty = v.every(
            item =>
              (Array.isArray(item) && item.length === 0) ||
              (isPojo(item) && Object.keys(item).length === 0) ||
              (typeof item === "string" && item === "")
          )
          if (hasOnlyEmpty) {
            continue
          }
        }

        if (Array.isArray(v)) {
          // Type info already added above in arrayTypes
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
            // Empty strings are shown as "empty: " (with trailing space)
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
          continue
        } else if (Array.isArray(v)) {
          if (v.length === 0) {
            // Don't add field line for empty array
          } else {
            // Check if this array will have its own body part
            const childPath = `${bodyKey}/${k}`
            if (!sortedBodyKeys.includes(childPath)) {
              // Check if this array contains objects - if so, don't add field line
              const hasObjects = v.some(item => isPojo(item))
              if (!hasObjects) {
                const encodedItems = v
                  .map(item => encodeArrayItem(item))
                  .join(", ")
                fieldLines.push(`${k}: ${encodedItems}`)
              }
            }
          }
        } else if (isPojo(v) && Object.keys(v).length === 0) {
          // Empty object - don't add field line
        }
      }

      // Combine arrayTypes with objectTypes
      const allTypes = [...arrayTypes, ...objectTypes]

      // Check if this object should be skipped entirely
      const shouldSkipObject = Object.entries(value).every(([k, v]) => {
        const childPath = `${bodyKey}/${k}`
        if (sortedBodyKeys.includes(childPath)) return true
        if (Array.isArray(v) && v.some(item => isPojo(item))) {
          // Check if array has only empty elements
          const hasOnlyEmpty = v.every(
            item =>
              (Array.isArray(item) && item.length === 0) ||
              (isPojo(item) && Object.keys(item).length === 0) ||
              (typeof item === "string" && item === "")
          )
          return hasOnlyEmpty || sortedBodyKeys.includes(childPath)
        }
        return false
      })

      if (
        shouldSkipObject &&
        !hasOnlyEmptyCollections &&
        !hasArraysWithOnlyEmptyElements
      ) {
        continue
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

        if (allTypes.length > 0) {
          orderedLines.push(`ao-types: ${allTypes.sort().join(", ")}`)
        }

        orderedLines.push("content-disposition: inline")

        if (fieldLines.length > 0) {
          orderedLines.push("")
          for (const line of fieldLines) {
            orderedLines.push(line)
          }
        }

        const binaryFieldsForInline = Object.entries(value)
          .filter(
            ([k, v]) =>
              isBytes(v) && !sortedBodyKeys.includes(`${bodyKey}/${k}`)
          )
          .map(([k, v]) => ({
            key: k,
            buffer: toBuffer(v),
          }))

        if (binaryFieldsForInline.length > 0) {
          const parts = []
          parts.push(Buffer.from(orderedLines.join("\r\n")))

          for (const { key, buffer } of binaryFieldsForInline) {
            parts.push(Buffer.from(`\r\n${key}: `))
            parts.push(buffer)
          }

          parts.push(Buffer.from("\r\n"))

          const fullBody = Buffer.concat(parts)
          bodyParts.push(new Blob([fullBody]))
        } else {
          // Check if this is the last body part for special handling
          const isLastBodyPart =
            sortedBodyKeys.indexOf(bodyKey) === sortedBodyKeys.length - 1
          const hasOnlyTypes = allTypes.length > 0 && fieldLines.length === 0

          if (isLastBodyPart && hasOnlyTypes) {
            // Don't add empty line for last part with only types
            bodyParts.push(new Blob([orderedLines.join("\r\n")]))
          } else if (fieldLines.length === 0) {
            bodyParts.push(new Blob([orderedLines.join("\r\n")]))
          } else {
            bodyParts.push(new Blob([orderedLines.join("\r\n") + "\r\n"]))
          }
        }
      } else {
        // Put ao-types first, then content-disposition, then field lines
        const orderedLines = []

        if (allTypes.length > 0) {
          orderedLines.push(`ao-types: ${allTypes.sort().join(", ")}`)
        }

        orderedLines.push(`content-disposition: form-data;name="${bodyKey}"`)

        // Check if we have any binary fields
        const hasBinaryFields = binaryFields && binaryFields.length > 0

        // Add blank line before field lines if we have binary fields or no field lines
        if (hasBinaryFields || fieldLines.length === 0) {
          orderedLines.push("")
        }

        // Add field lines
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
          bodyParts.push(new Blob([fullBody]))
        } else {
          // Add trailing blank line if we have field lines
          if (fieldLines.length > 0) {
            bodyParts.push(new Blob([orderedLines.join("\r\n") + "\r\n"]))
          } else {
            bodyParts.push(new Blob([orderedLines.join("\r\n")]))
          }
        }
      }

      continue
    }

    const isInline = bodyKey === "body" && headers["inline-body-key"] === "body"
    if (isInline) {
      lines.push(`content-disposition: inline`)
    } else {
      lines.push(`content-disposition: form-data;name="${bodyKey}"`)
    }

    if (isBytes(value)) {
      const buffer = toBuffer(value)
      const headerText = lines.join("\r\n") + "\r\n\r\n"
      bodyParts.push(new Blob([headerText, buffer]))
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

  // Add the special data/body part if needed
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

  console.log("=== ENCODE END ===\n")
  return { headers, body }
}

export async function enc(fields) {
  return await encode(fields)
}

import genSimple from "./genSimple.js"

const bin = Buffer.from([1, 2, 3])
const empty = Buffer.from([])

const ok = [
  { recursive: [1, [2, [3, [4]]]] },
  { types: ["", 0, "", false, Symbol("null"), [], {}, -1] },
  { chaos: [1, "three", true, Symbol("ok"), null, [], {}, 4.5] },
  { mixed_empty_full: ["", [1], {}, { a: 1 }] },
  { alternating: [1, "one", 2, "two", 3, "three"] },
  {
    binary_data: "some binary content",
    symbols: [Symbol("success"), Symbol("failure"), Symbol("pending")],
    numbers: [0, -1, 42, 3.14, -99.99],
    strings: ["hello", "world", "test"],
  },
  {
    count: 10,
    active: true,
    tags: ["alpha", "beta", "gamma"],
  },
  {
    values: [1, 2, 3, 4, 5],
    states: [Symbol("active"), Symbol("inactive"), Symbol("pending")],
  },
  {
    data: [
      1,
      "abc",
      true,
      "ok",
      1.23,
      { bool: false, float: 3.14, int: 1, nested: "ok", str: "abc" },
    ],
  },
  { empty: Buffer.from([]), nested: [Buffer.from([]), 3] },
  { list: [1, [2, 3]] },
  { list: [53.05] },
  { list: [Symbol("ok")] },
  { mixed_empty: ["", 0, [], "", {}, null, false] },

  { "data-field": 1, data_field: 2, dataField: 3 },
  {
    matrix: [
      [
        { x: 1, y: 1 },
        { x: 2, y: 2 },
      ],
      [
        { x: 3, y: 3 },
        { x: 4, y: 4 },
      ],
    ],
  },

  { network: { nodes: [{ ip: "127.0.0.1", port: 8080 }] } },
  { nested_maps: { a: [{ val: 1 }], b: [{ val: 2 }] } },
  { tree: { value: 1, children: [{ value: 2 }, { value: 3 }] } },
  { complex_empty: { lists: [[]], maps: [{}] } },
  { empty_in_maps: [{ empty: "" }, { empty: [] }, { empty: {} }] },
  { empty_nested: [[], [[]], [[], []]] },
  { empty_maps_list: [{}, {}, {}] },
  { empty_map_array: [{}, { a: 1 }, {}, { b: 2 }] },
  { users: [{ id: 1 }, { id: 2 }] },
  {
    items: [
      { name: "a", value: 1 },
      { name: "b", value: 2 },
    ],
  },
  {
    mixed_maps: [
      { type: "binary", data: [1, 2, 3] },
      { type: "text", data: "hello" },
    ],
  },
  { nested_array_maps: [{ items: [1, 2, 3] }, { items: [4, 5, 6] }] },

  { maps_different_keys: [{ x: 1 }, { y: 2 }, { z: 3 }] },
  { deep_map_array: [{ level: { deep: 1 } }, { level: { deep: 2 } }] },
  { maps_with_arrays: [{ arr: [1, 2] }, { arr: [3, 4] }] },
  { max_int32: 2147483647 },
  { min_int32: -2147483648 },
  { _field: 1, field: 2, field_: 3 },
  {
    matrix: [
      [1, 2],
      [3, 4],
      [5, 6],
    ],
  },
  { layers: [{ data: [1, 2] }, { data: [3, 4] }] },
  { mixed_types: [{ int: 2, str: "three" }] },
  {
    config: [
      { setting: "on", value: 1 },
      { setting: "off", value: 0 },
    ],
  },
  { wrapped: [[1], [2], ["three"], [true], [Symbol("ok")]] },
  {
    indexed: [
      { idx: 1, data: 1 },
      { idx: 2, data: "two" },
    ],
  },
  {
    flagged: [
      { flag: true, value: 1 },
      { flag: false, value: 0 },
    ],
  },
  {
    tagged: [
      { tag: "num", val: 123 },
      { tag: "str", val: "hello" },
    ],
  },
  { protocol: { header: [1, 2, 3, 4], body: { data: [9, 10, 11] } } },
  { messages: [{ from: 1, to: 2, data: "hello" }] },

  {
    files: [
      { name: "file1", size: 123 },
      { name: "file2", size: 456 },
    ],
  },
  { stream: { chunks: ["part1", "part2", "part3"], eof: true } },
  {
    levels: {
      1: [1],
      2: [1, 2],
      3: [1, 2, 3],
      empty: [],
    },
  },
  { data: { test: [1.23, "str", 1, Symbol("ok"), [1, 2, 3]] } },
  { key: [{ str: "abc" }] },
  {
    complex: {
      nested: {
        array: [true, false, "text", 42],
        empty_map: {},
        symbol: Symbol("atom_value"),
      },
      simple: "value",
    },
  },
  {
    mixed_array: [{ name: "John", age: 25 }, "standalone_string", 3.14159],
    boolean_flag: true,
    number: 999,
  },
  {
    list_of_maps: [
      { id: 1, status: Symbol("active") },
      {
        id: 2,
        status: Symbol("inactive"),
        meta: { created: "2024-01-01" },
      },
    ],
    metadata: { version: "1.0", timestamp: 1640995200 },
  },
  {
    deep_nesting: {
      level1: {
        level2: {
          level3: [Symbol("error"), Symbol("warning"), Symbol("info")],
          config: { debug: true },
        },
      },
    },
    simple_list: [1, 2, 3, 4, 5],
  },
  {
    body: Buffer.from([1, 2, 3, 255, 0]),
    empty_list: [],
    empty_binary: Buffer.from([]),
  },
  {
    data: {
      bytes: Buffer.from([0, 127, 255]),
      empty: { list: [], map: {} },
    },
  },
  {
    data: "plain text",
    body: { content: Buffer.from([192, 168, 1, 1]) },
  },
  {
    nested_empty: {
      a: {},
      b: { c: [] },
    },
    flag: false,
  },
  {
    data: [[], [1], [1, 2], []],
    meta: null,
  },
  {
    body: {
      raw: Buffer.from([0, 0, 0, 0]),
      encoded: "base64_content",
    },
    empty_string: "",
  },
  {
    items: {
      first: Buffer.from([255]),
      second: Buffer.from([0]),
      third: Buffer.from([128]),
    },
  },
  {
    data: { body: Buffer.from([255]) },
    body: { data: Buffer.from([0]) },
  },
  {
    body: { a: { b: 5, c: 3 } },
    data: { d: { e: 5, f: 3 } },
    key: { g: { h: 5, i: 3 } },
    num: 3,
    list: [1, 2, 3],
    str: "Hello",
  },
  { data: { a: 3, b: 4 } },
  { body: { a: { c: 3 }, b: 4 } },
  { data: { a: 3, b: { c: { d: 4 } } }, body: { a: 3, b: { c: { d: 4 } } } },
  {
    float: 1.23,
    int: 1,
    bool: true,
    atom: Symbol("ok"),
    nest: { bool: true, atom: Symbol("ok") },
    body: { bool: true, nest: { atom: Symbol("ok") } },
    data: { bool: true, nest: { atom: Symbol("ok") } },
  },
  { key: [{ str: "abc" }] },
  { key: [{ int: 1 }] },
  { null: null, undefined: undefined },
  { body: { list: [], map: {} }, nested: { list: [], map: {} } },
  { binary: Buffer.from([1, 2, 3]) },
  {
    str: "abc",
    bool: true,
    num: 123,
    float: 3.14,
    atom: Symbol("ok"),
    list: [1, 2, 3],
    body: bin,
  },
  { map: { a: 1, b: 2, c: { d: 3 } }, body: bin },
  { body: bin, data: bin },
  { map: { a: 3, b: "abc", c: { d: { e: 3 } } } },
  { map: { jntzf: 8.02 } },
  { map: { float: 86.01, bool: true } },
  { key: [[8.02]] },
  { body: bin, data: bin },
  { base64: Buffer.from([1, 2, 3]).toString("base64") },
  { bin: empty },
  { body: empty },
  { bin: empty, body: empty },
  { bin: {} },
  { data: {} },
  { body: {} },
  { data: {}, body: {} },
  { bin: {}, data: {} },
  { bin: {}, body: {} },
  { bin: {}, data: {}, body: {} },
  { bin: [] },
  { data: [] },
  { body: [] },
  { data: [], body: [] },
  { bin: [], data: [] },
  { bin: [], body: [] },
  { bin: [], data: [], body: [] },
  { bin },
  { data: bin },
  { body: bin },
  { data: bin, body: bin },
  { bin, data: bin },
  { bin, body: bin },
  { bin, data: bin, body: bin },
  { num: Symbol("atom") },
  { data: Symbol("atom") },
  { body: Symbol("atom") },
  { data: Symbol("atom"), body: Symbol("atom") },
  { num: Symbol("atom"), data: Symbol("atom") },
  { num: Symbol("atom"), body: Symbol("atom") },
  { num: Symbol("atom"), data: Symbol("atom"), body: Symbol("atom") },
  { num: 3.14 },
  { data: 3.14 },
  { body: 3.14 },
  { data: 3.14, body: 3.14 },
  { num: 3.14, data: 3.14 },
  { num: 3.14, body: 3.14 },
  { num: 3.14, data: 3.14, body: 3.14 },
  { data: "abc" },
  { body: "abc" },
  { data: "abc", body: "abc" },
  { str: "abc" },
  { data: "abc", str: "abc" },
  { body: "abc", str: "abc" },
  { data: "abc", body: "abc", str: "abc" },
  { ok: true },
  { data: true },
  { body: true },
  { data: true, body: true },
  { ok: true, data: true },
  { ok: true, body: true },
  { ok: true, data: true, body: true },
  { num: 1 },
  { data: 1 },
  { body: 1 },
  { data: 1, body: 1 },
  { num: 1, data: 1 },
  { num: 1, body: 1 },
  { num: 1, data: 1, body: 1 },
]

const errs = [
  {
    nested: {
      nums: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
      maps: [{ n: 1 }, { n: 2 }, { n: 3 }, { n: 4 }, { n: 5 }],
      empty: ["", [], {}, null],
    },
  },
  { binary: Buffer.from([1, 2, 3]), binary2: Buffer.from([1, 2, 3]) },
  { bin: [bin, bin] },
  { list: [Symbol("ok"), [bin]] },
  { data: empty },
  { data: empty, body: empty },
  { bin: empty, data: empty },
  { bin: empty, data: empty, body: empty },
  { max_int64: 9223372036854775807 },
  { min_int64: -9223372036854775808 },
  { large_positive: 1000000000000000 },
  { large_negative: -1000000000000000 },
  { int_array: [9223372036854775807, -9223372036854775808, 0] },
  { boundary_ints: { max: 9223372036854775807, min: -9223372036854775808 } },
  { data: 1, Data: 2, DATA: 3 },
  { body: "a", Body: "b", BODY: "c" },
  { test: 1, Test: 2, TEST: 3, TeSt: 4 },
  { a1: 1, A1: 2, "1a": 3, "1A": 4 },
  { 123: "numeric", abc: "alpha", ABC: "ALPHA" },
  { "field!": 1, "field?": 2, "field#": 3 },
  { "": "empty_key", " ": "space_key", "  ": "two_spaces" },
  {
    data: [1, 9223372036854775807, "text", { val: 2 }],
    Data: [3, -9223372036854775808, [], {}],
    DATA: ["", null, Symbol("undefined"), Symbol("ok")],
  },
  {
    users: [
      { id: 9223372036854775807, name: "user1", tags: ["a", "b"] },
      { id: -9223372036854775808, name: "user2", tags: [] },
    ],
  },
  {
    types: [
      { type: "integer", value: 9223372036854775807 },
      { type: "empty", value: "" },
      { type: "list", value: [1, 2] },
    ],
  },
  {
    field: 1,
    Field: [2],
    FIELD: { data: 3 },
    FiElD: [{ val: 4 }],
  },
  {
    all: [
      0,
      255,
      9223372036854775807,
      -9223372036854775808,
      "",
      [],
      {},
      { a: 1 },
      [2, 3],
      null,
      Symbol("undefined"),
      Symbol("ok"),
    ],
  },
  {
    ultimate: [
      { id: 1, data: "small", meta: { empty: "" } },
      { id: 9223372036854775807, data: [4, 5, 6], meta: {} },
      { id: -9223372036854775808, data: "", meta: { list: [7] } },
    ],
    Data: 255,
    data: 0,
    DATA: [128],
  },
]

//export default genSimple()
//export default errs
export default ok
