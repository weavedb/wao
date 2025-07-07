import { hash } from "fast-sha256"

export function isBytes(value) {
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

export function isPojo(value) {
  return (
    !isBytes(value) &&
    !Array.isArray(value) &&
    !(value instanceof Blob) &&
    typeof value === "object" &&
    value !== null
  )
}

export async function hasNewline(value) {
  if (typeof value === "string") return value.includes("\n")
  if (value instanceof Blob) {
    value = await value.text()
    return value.includes("\n")
  }
  if (isBytes(value)) return Buffer.from(value).includes("\n")
  return false
}

export async function sha256(data) {
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

export function formatFloat(num) {
  let exp = num.toExponential(20)
  exp = exp.replace(/e\+(\d)$/, "e+0$1")
  exp = exp.replace(/e-(\d)$/, "e-0$1")
  return exp
}

export function hasNonAscii(str) {
  return /[^\x00-\x7F]/.test(str)
}

export function toBuffer(value) {
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

// Navigate through a path to get a value from nested object/array
export function getValueByPath(obj, path) {
  const pathParts = path.split("/")
  let value = obj
  for (const part of pathParts) {
    if (/^\d+$/.test(part)) {
      value = value[parseInt(part) - 1]
    } else {
      value = value[part]
    }
  }
  return value
}

// Get the ao-type for a value
export function getAoType(value) {
  if (
    typeof value === "boolean" ||
    value === null ||
    value === undefined ||
    typeof value === "symbol"
  ) {
    return "atom"
  } else if (typeof value === "number") {
    return Number.isInteger(value) ? "integer" : "float"
  } else if (typeof value === "string" && value.length === 0) {
    return "empty-binary"
  } else if (isBytes(value) && (value.length === 0 || value.byteLength === 0)) {
    return "empty-binary"
  } else if (Array.isArray(value) && value.length === 0) {
    return "empty-list"
  } else if (Array.isArray(value)) {
    return "list"
  } else if (isPojo(value) && Object.keys(value).length === 0) {
    return "empty-message"
  }
  return null
}

// Check if a value is empty
export function isEmpty(value) {
  return (
    (Array.isArray(value) && value.length === 0) ||
    (isPojo(value) && Object.keys(value).length === 0) ||
    (isBytes(value) && (value.length === 0 || value.byteLength === 0)) ||
    (typeof value === "string" && value.length === 0)
  )
}

// Convert primitive values to their encoded string representation
export function encodePrimitiveContent(value) {
  if (typeof value === "string") {
    return value
  } else if (typeof value === "boolean") {
    return `"${value}"`
  } else if (typeof value === "number") {
    return String(value)
  } else if (value === null) {
    return '"null"'
  } else if (value === undefined) {
    return '"undefined"'
  } else if (typeof value === "symbol") {
    return `"${value.description || "Symbol.for()"}"`
  }
  return value
}

// Sort type annotations by their numeric prefix
export function sortTypeAnnotations(types) {
  return types.sort((a, b) => {
    const aNum = parseInt(a.split("=")[0])
    const bNum = parseInt(b.split("=")[0])
    return aNum - bNum
  })
}

// Analyze array contents
export function analyzeArray(array) {
  return {
    hasObjects: array.some(item => isPojo(item)),
    hasArrays: array.some(item => Array.isArray(item)),
    hasEmptyStrings: array.some(
      item => typeof item === "string" && item === ""
    ),
    hasEmptyObjects: array.some(
      item => isPojo(item) && Object.keys(item).length === 0
    ),
    hasOnlyEmptyElements:
      array.length > 0 &&
      array.every(
        item =>
          (Array.isArray(item) && item.length === 0) ||
          (isPojo(item) && Object.keys(item).length === 0) ||
          (typeof item === "string" && item === "")
      ),
    hasOnlyNonEmptyObjects:
      array.length > 0 &&
      array.every(item => isPojo(item) && Object.keys(item).length > 0),
    hasObjectsWithOnlyEmptyValues: array.some(item => {
      if (!isPojo(item) || Object.keys(item).length === 0) return false
      return Object.values(item).every(v => isEmpty(v))
    }),
  }
}
