import { httpsig_from, httpsig_to } from "./httpsig.js"
import { structured_from, structured_to } from "./structured.js"
import { erl_json_from, erl_json_to, normalize } from "./erl_json.js"
import { enc } from "./encode.js"
import { isBytes } from "./encode-utils.js"

// Helper to check if an array contains binary data
const arrayHasBinaryData = arr => {
  if (!Array.isArray(arr)) return false

  return arr.some(item => {
    if (isBytes(item)) return true
    if (Array.isArray(item)) return arrayHasBinaryData(item)
    return false
  })
}

// Helper to check if a string contains non-printable characters
const hasNonPrintableChars = str => {
  if (typeof str !== "string") return false

  for (let i = 0; i < str.length; i++) {
    const code = str.charCodeAt(i)
    // Allow only printable ASCII (32-126)
    // Note: tabs (9), newlines (10), and carriage returns (13) are not allowed in HTTP headers
    if (code < 32 || code > 126) {
      return true
    }
  }
  return false
}

const isValid = encoded => {
  if (!encoded || typeof encoded !== "object") return false

  // Check if all header values are valid for HTTP headers
  for (const [key, value] of Object.entries(encoded)) {
    if (key === "body") {
      // Body can be string, Buffer, or undefined
      if (
        value !== undefined &&
        typeof value !== "string" &&
        !Buffer.isBuffer(value)
      ) {
        return false
      }
    } else {
      // All other fields (headers) must be strings or numbers
      if (typeof value !== "string" && typeof value !== "number") {
        // Check for Buffer in headers - this will fail HTTP signing
        if (Buffer.isBuffer(value) || isBytes(value)) {
          return false
        }
        return false
      }

      // Check if string contains non-printable characters
      if (typeof value === "string" && hasNonPrintableChars(value)) {
        return false
      }
    }
  }

  return true
}

// Check if object contains any binary data or arrays with binary data
const hasBinaryData = obj => {
  for (const [key, value] of Object.entries(obj)) {
    if (key === "path") continue

    if (isBytes(value)) {
      return true
    } else if (Array.isArray(value) && arrayHasBinaryData(value)) {
      return true
    } else if (
      typeof value === "object" &&
      value !== null &&
      !Array.isArray(value)
    ) {
      // Check nested objects
      for (const [k, v] of Object.entries(value)) {
        if (isBytes(v)) {
          return true
        } else if (Array.isArray(v) && arrayHasBinaryData(v)) {
          return true
        }
      }
    }
  }
  return false
}

const smartSign = async obj => {
  try {
    // Use enc() which properly handles binary data by creating multipart bodies
    const encoded = await enc(obj)

    // Return the encoded result with headers and body
    return {
      ...encoded.headers,
      body: encoded.body,
      path: "/~wao@1.0/httpsig",
    }
  } catch (error) {
    console.error("Encoding failed:", error)

    // Fallback: create a simple structure
    const result = { path: "/~wao@1.0/httpsig" }

    for (const [key, value] of Object.entries(obj)) {
      if (key === "path") continue

      if (!isBytes(value)) {
        result[key] = value
      }
    }

    return result
  }
}

export const sign = async obj => {
  // If object contains binary data, use enc() directly
  if (hasBinaryData(obj)) {
    return await smartSign(obj)
  }

  // Otherwise use the standard pipeline
  const encoded = httpsig_to(
    normalize(structured_from(normalize({ ...obj, path: "/~wao@1.0/httpsig" })))
  )

  // Check if the encoded result is valid for HTTP headers
  if (!isValid(encoded)) {
    return await smartSign(obj)
  }

  return encoded
}
