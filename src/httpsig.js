import { trim } from "ramda"
import { decodeSigInput } from "./signer-utils.js"
import base64url from "base64url"

/**
 * Get multipart boundary from content-type header
 */
const getBoundary = http => {
  const ctype = http.headers["content-type"]
  if (ctype && /^multipart\/form-data;/.test(trim(ctype))) {
    for (const v of trim(ctype).split(";").slice(1)) {
      const sp2 = v.split("=")
      if (trim(sp2[0]) === "boundary") return trim(sp2[1]).replace(/"/g, "")
    }
  }
  return null
}

/**
 * Extract specified components from HTTP message
 * @param {Object} http - HTTP message object with headers and optional body
 * @param {Array} components - Array of component names to extract
 * @returns {Object} Extracted components with their values
 */
const extract = (http, components) => {
  const extracted = {}
  const needsBody = components.some(
    c => c.replace(/"/g, "").toLowerCase() === "content-digest"
  )

  // First extract ao-types if it's signed
  const hasAoTypes = components.some(
    c => c.replace(/"/g, "").toLowerCase() === "ao-types"
  )
  if (hasAoTypes) {
    const aoTypes = http.headers["ao-types"] || http.headers["Ao-Types"]
    if (aoTypes) {
      extracted["ao-types"] = aoTypes
    }
  }

  // Extract ao-ids if it's signed
  const hasAoIds = components.some(
    c => c.replace(/"/g, "").toLowerCase() === "ao-ids"
  )
  if (hasAoIds) {
    const aoIds = http.headers["ao-ids"] || http.headers["Ao-Ids"]
    if (aoIds) {
      extracted["ao-ids"] = aoIds
    }
  }

  for (const component of components) {
    const cleanComponent = component.replace(/"/g, "")

    if (cleanComponent.startsWith("@")) {
      // Handle derived components
      switch (cleanComponent) {
        case "@method":
          extracted[cleanComponent] = http.method || "GET"
          break
        case "@target-uri":
          extracted[cleanComponent] = http.url || ""
          break
        case "@authority":
          extracted[cleanComponent] = http.headers.host || ""
          break
        case "@scheme":
          if (http.url) {
            const url = new URL(http.url)
            extracted[cleanComponent] = url.protocol.replace(":", "")
          }
          break
        case "@request-target":
          if (http.url) {
            const url = new URL(http.url)
            extracted[cleanComponent] = url.pathname + url.search
          }
          break
        case "@path":
          if (http.url) {
            const url = new URL(http.url)
            extracted[cleanComponent] = url.pathname
          }
          break
        case "@query":
          if (http.url) {
            const url = new URL(http.url)
            extracted[cleanComponent] = url.search
          }
          break
        case "@status":
          extracted[cleanComponent] = String(
            http.status || http["@status"] || ""
          )
          break
        case "@query-param":
          // This would need additional parsing logic for specific query parameters
          break
      }
    } else {
      // Handle regular headers - try both exact case and lowercase
      const headerValue =
        http.headers[cleanComponent] ||
        http.headers[cleanComponent.toLowerCase()]
      if (headerValue !== null && headerValue !== undefined) {
        extracted[cleanComponent] = headerValue
      }
    }
  }

  // If content-digest is signed, we need to include the body
  if (needsBody && http.body !== undefined) {
    extracted["body"] = http.body
  }

  // Add flag if body is needed but missing
  if (needsBody && http.body === undefined) {
    extracted["__bodyRequired__"] = true
  }

  return extracted
}

/**
 * Convert body to Buffer from various sources
 * @param {string|Buffer|ArrayBuffer} body - The body to convert
 * @returns {Buffer} The body as a Buffer
 */
export const toBuffer = body => {
  if (!body) {
    return Buffer.alloc(0)
  }

  // If it's already a Buffer, return it
  if (Buffer.isBuffer(body)) {
    return body
  }

  // If it's a string, convert to Buffer
  if (typeof body === "string") {
    return Buffer.from(body, "utf-8")
  }

  // If it's an ArrayBuffer or TypedArray
  if (body instanceof ArrayBuffer || ArrayBuffer.isView(body)) {
    return Buffer.from(body)
  }

  throw new Error("Unsupported body type")
}

/**
 * Parse ao-ids dictionary
 * @param {string} aoIds - The ao-ids header value
 * @returns {Object} Parsed ID mappings
 */
const parseAoIds = aoIds => {
  const result = {}

  // Match pattern: ID="value"
  const regex = /([A-Za-z0-9_-]{43})="([^"]*)"/g
  let match

  while ((match = regex.exec(aoIds)) !== null) {
    const [, id, value] = match
    result[id] = value
  }

  return result
}

/**
 * Convert message to JSON with proper type conversions
 * Following the logic from dev_codec_structured, dev_codec_httpsig_conv, and dev_codec_flat
 * @param {Object} msg - The message to convert
 * @returns {Object} JSON representation
 */
const toJSON = msg => {
  if (!msg || typeof msg !== "object") {
    return msg
  }

  let result = { ...msg }

  // Handle ao-ids parsing
  if (result["ao-ids"]) {
    const parsedIds = parseAoIds(result["ao-ids"])
    // Remove the ao-ids header and merge the parsed IDs
    delete result["ao-ids"]
    result = { ...result, ...parsedIds }
  }

  // First, handle the multipart body if present
  const contentType = result["content-type"]
  const body = result.body

  if (body && contentType && contentType.includes("multipart/form-data")) {
    const boundary = getBoundary({ headers: { "content-type": contentType } })
    if (boundary) {
      // Parse multipart body
      const parts = parseMultipartBody(body, boundary)

      // Remove the raw body since we've parsed it
      delete result.body

      // Merge parsed parts into result
      for (const [partName, partData] of Object.entries(parts)) {
        // Parse ao-types from part data if present
        const partTypes = {}
        if (partData["ao-types"]) {
          // Updated regex to handle spaces and trim the key
          const matches = [
            ...partData["ao-types"].matchAll(/([^=,]+)="([^"]+)"/g),
          ]
          for (const [_, key, type] of matches) {
            partTypes[key.trim()] = type
          }
        }

        // Apply type conversions to part data
        const convertedPartData = {}
        for (const [key, value] of Object.entries(partData)) {
          if (key === "ao-types" || key === "content-disposition") continue

          const type = partTypes[key]
          if (type && typeof value === "string") {
            convertedPartData[key] = convertByType(value, type)
          } else {
            convertedPartData[key] = value
          }
        }

        // Store the result
        if (Object.keys(convertedPartData).length > 0) {
          // Check if the part name suggests it's an array element (ends with /number)
          const isArrayElement = /\/\d+$/.test(partName)

          // For top-level parts (no slash in name) with objects, keep the structure
          const isTopLevel = !partName.includes("/")

          // Check if this is a nested path that will be unflattened
          const isNestedPath = partName.includes("/") && !isArrayElement

          if (isArrayElement || isNestedPath) {
            // Array elements and nested paths keep their structure
            result[partName] = convertedPartData
          } else if (isTopLevel || Object.keys(convertedPartData).length > 1) {
            // Top-level objects or multi-field objects keep their structure
            result[partName] = convertedPartData
          } else {
            // Only lift single values for simple cases
            result[partName] =
              convertedPartData[Object.keys(convertedPartData)[0]]
          }
        }

        // Store type information for nested fields
        if (Object.keys(partTypes).length > 0) {
          for (const [fieldKey, fieldType] of Object.entries(partTypes)) {
            if (fieldKey !== "ao-types" && fieldKey !== "content-disposition") {
              // Add to global typeMap with the full path
              const fullPath = partName + "/" + fieldKey
              // Store in the ao-types for later use
              if (!result["__typeMap"]) result["__typeMap"] = {}
              result["__typeMap"][fullPath] = fieldType
            }
          }
        }
      }
    }
  }

  // Parse global ao-types to get type information
  const typeMap = {}
  if (result["ao-types"]) {
    // Updated regex to handle spaces and trim the key
    const matches = [...result["ao-types"].matchAll(/([^=,]+)="([^"]+)"/g)]
    for (const [_, key, type] of matches) {
      typeMap[key.trim()] = type
    }
  }

  // Merge in types from multipart parsing
  if (result["__typeMap"]) {
    Object.assign(typeMap, result["__typeMap"])
    delete result["__typeMap"]
  }

  // Also collect types from multipart parts for nested fields
  for (const [key, value] of Object.entries(result)) {
    if (
      key.includes("/") &&
      typeof value === "object" &&
      value !== null &&
      value.__partTypes
    ) {
      // This is a multipart part with type information
      for (const [fieldKey, fieldType] of Object.entries(value.__partTypes)) {
        // Create the full path for the type
        const fullPath = `${key}/${fieldKey}`
        typeMap[fullPath] = fieldType
      }
      // Remove the __partTypes after processing
      delete value.__partTypes
    }
  }

  // Add empty values for fields that are in ao-types but not in result
  for (const [key, type] of Object.entries(typeMap)) {
    if (!(key in result) && type.startsWith("empty-")) {
      result[key] = convertByType("", type)
    }
  }

  // Apply type conversions and build final result
  const finalResult = {}

  for (const [key, value] of Object.entries(result)) {
    // Skip internal keys and headers we don't want in the final result
    if (key.startsWith("__")) continue
    if (key.startsWith("@")) continue // Skip all @ fields
    if (key === "ao-types") continue
    if (key === "content-type") continue
    if (key === "content-digest") continue
    if (key === "signature") continue
    if (key === "signature-input") continue
    if (key === "body-keys") continue

    // Get the type for this key
    const type = typeMap[key]

    // Special handling for objects that should be lists
    if (
      type === "list" &&
      typeof value === "object" &&
      value !== null &&
      !Array.isArray(value)
    ) {
      // This is an object that should be converted to a list
      const converted = maybeConvertToArray(value)
      finalResult[key] = converted
    } else if (type && typeof value === "string") {
      // Convert based on type
      finalResult[key] = convertByType(value, type)
    } else if (type && value === undefined) {
      // Handle empty types
      finalResult[key] = convertByType("", type)
    } else if (
      typeof value === "object" &&
      value !== null &&
      !Array.isArray(value)
    ) {
      // For objects without a specific type, keep them as objects
      // Only recurse for processing nested values
      const processedObj = {}
      for (const [k, v] of Object.entries(value)) {
        if (Array.isArray(v)) {
          // Keep arrays as arrays
          processedObj[k] = v
        } else if (typeof v === "object" && v !== null) {
          processedObj[k] = toJSON(v)
        } else {
          processedObj[k] = v
        }
      }
      finalResult[key] = processedObj
    } else {
      // Keep as-is (including arrays)
      finalResult[key] = value
    }
  }

  // Handle flattened paths - pass typeMap for context
  return unflattenPaths(finalResult, typeMap)
}

/**
 * Convert objects with numeric keys to arrays
 */
const maybeConvertToArray = obj => {
  if (Array.isArray(obj)) return obj

  const keys = Object.keys(obj)
  const numericKeys = keys.filter(k => /^\d+$/.test(k))

  // If all keys are numeric and sequential starting from 1
  if (numericKeys.length > 0 && numericKeys.length === keys.length) {
    const sortedNumericKeys = numericKeys.map(Number).sort((a, b) => a - b)
    const maxIndex = Math.max(...sortedNumericKeys)
    const arr = []

    // Fill array based on numeric keys (1-based to 0-based)
    for (let i = 1; i <= maxIndex; i++) {
      if (obj[String(i)] !== undefined) {
        arr[i - 1] = obj[String(i)]
      }
    }

    return arr
  }

  return obj
}

/**
 * Convert value based on its type
 */
const convertByType = (value, type) => {
  switch (type) {
    case "integer":
      // Handle structured field integer format
      if (
        typeof value === "string" &&
        value.match(/^"?\(ao-type-integer\)\s+(\d+)"?$/)
      ) {
        const match = value.match(/(\d+)/)
        return parseInt(match[1], 10)
      }
      return parseInt(value, 10)
    case "float":
    case "decimal":
      return parseFloat(value)
    case "boolean":
      return value === "true" || value === "?1"
    case "atom":
      // Remove quotes from atom values
      let atomValue = value

      // Remove any surrounding quotes (single or double)
      if (
        (atomValue.startsWith('"') && atomValue.endsWith('"')) ||
        (atomValue.startsWith("'") && atomValue.endsWith("'"))
      ) {
        atomValue = atomValue.slice(1, -1)
      }

      // Also handle escaped quotes
      atomValue = atomValue.replace(/\\"/g, '"').replace(/\\'/g, "'")

      // Handle special atom values
      if (atomValue === "true") return true
      if (atomValue === "false") return false
      if (atomValue === "null") return null

      // For other atoms, return as Symbol
      return Symbol.for(atomValue)
    case "list":
      // Handle case where list is a comma-separated string
      if (typeof value === "string" && !value.startsWith("(")) {
        const items = []
        let current = ""
        let inQuotes = false
        let depth = 0

        for (let i = 0; i < value.length; i++) {
          const char = value[i]
          const prevChar = value[i - 1]

          if (char === '"' && prevChar !== "\\") {
            inQuotes = !inQuotes
          }

          if (!inQuotes) {
            if (char === "(") {
              depth++
            } else if (char === ")") {
              depth--
            } else if (char === "," && depth === 0) {
              if (current.trim()) {
                // Parse the item to handle type annotations
                items.push(parseStructuredItem(current.trim()))
              }
              current = ""
              continue
            }
          }

          current += char
        }

        if (current.trim()) {
          items.push(parseStructuredItem(current.trim()))
        }

        return items
      }
      return parseStructuredList(value)
    case "map":
    case "dictionary":
      return parseStructuredDict(value)
    case "empty-binary":
      return ""
    case "empty-list":
      return []
    case "empty-message":
      return {}
    default:
      return value
  }
}

/**
 * Parse structured field list
 */
const parseStructuredList = value => {
  if (!value || value === "()") return []

  // Remove outer quotes if present
  let content = value.trim()
  if (content.startsWith('"') && content.endsWith('"')) {
    content = content.slice(1, -1)
  }

  const items = []
  let current = ""
  let inQuotes = false
  let depth = 0

  for (let i = 0; i < content.length; i++) {
    const char = content[i]
    const prevChar = content[i - 1]

    if (char === '"' && prevChar !== "\\") {
      inQuotes = !inQuotes
      current += char
    } else if (!inQuotes) {
      if (char === "(") {
        depth++
        current += char
      } else if (char === ")") {
        depth--
        current += char
      } else if (char === "," && depth === 0) {
        if (current.trim()) {
          items.push(parseStructuredItem(current.trim()))
        }
        current = ""
      } else {
        current += char
      }
    } else {
      current += char
    }
  }

  if (current.trim()) {
    items.push(parseStructuredItem(current.trim()))
  }

  return items
}

/**
 * Parse structured field item
 */
const parseStructuredItem = item => {
  // Quoted string - handle first to properly process inner content
  if (item.startsWith('"') && item.endsWith('"')) {
    const inner = item.slice(1, -1).replace(/\\"/g, '"')

    // Check if the inner content is ao-type encoded
    const innerAoTypeMatch = inner.match(/^\(ao-type-(\w+)\)\s+(.+)$/)
    if (innerAoTypeMatch) {
      const [, type, value] = innerAoTypeMatch
      // The value here has already had escaped quotes converted to real quotes
      // If it's wrapped in quotes, remove them
      let cleanValue = value
      if (
        (cleanValue.startsWith('"') && cleanValue.endsWith('"')) ||
        (cleanValue.startsWith("'") && cleanValue.endsWith("'"))
      ) {
        cleanValue = cleanValue.slice(1, -1)
      }
      return convertByType(cleanValue, type)
    }

    return inner
  }

  // Handle ao-type encoded items without outer quotes
  const aoTypeMatch = item.match(/^\(ao-type-(\w+)\)\s+(.+)$/)
  if (aoTypeMatch) {
    const [, type, value] = aoTypeMatch
    let cleanValue = value
    // Handle escaped quotes
    cleanValue = cleanValue.replace(/\\"/g, '"')
    // If wrapped in quotes, remove them
    if (
      (cleanValue.startsWith('"') && cleanValue.endsWith('"')) ||
      (cleanValue.startsWith("'") && cleanValue.endsWith("'"))
    ) {
      cleanValue = cleanValue.slice(1, -1)
    }
    return convertByType(cleanValue, type)
  }

  // Boolean
  if (item === "?1") return true
  if (item === "?0") return false

  // Number
  if (/^-?\d+$/.test(item)) {
    return parseInt(item, 10)
  }
  if (/^-?\d+\.\d+$/.test(item)) {
    return parseFloat(item)
  }

  // Nested list
  if (item.startsWith("(") && item.endsWith(")")) {
    return parseStructuredList(item)
  }

  return item
}

/**
 * Parse structured field dictionary
 */
const parseStructuredDict = value => {
  const decoded = decodeSigInput(value)
  const result = {}

  for (const [key, info] of Object.entries(decoded)) {
    if (info && info.components && info.components.length > 0) {
      result[key] = parseStructuredItem(info.components[0])
    } else {
      result[key] = true
    }
  }

  return result
}

/**
 * Parse body-keys list
 */
const parseBodyKeysList = value => {
  const matches = [...value.matchAll(/"([^"]+)"/g)]
  return matches.map(m => m[1])
}

/**
 * Parse multipart body
 */
const parseMultipartBody = (body, boundary) => {
  const result = {}

  // Split by boundary lines
  const parts = body.split(`--${boundary}`)

  for (let i = 0; i < parts.length; i++) {
    const part = parts[i]

    // Skip empty parts and the terminating part
    if (!part || part === "--" || part === "--\r\n" || part.trim() === "")
      continue

    // Remove leading \r\n if present
    let content = part
    if (content.startsWith("\r\n")) {
      content = content.substring(2)
    }

    // Remove trailing \r\n or -- if present
    if (content.endsWith("\r\n")) {
      content = content.substring(0, content.length - 2)
    }

    if (!content) continue

    // Parse all lines
    const lines = content.split("\r\n")
    const partData = {}
    let partName = null

    for (const line of lines) {
      if (!line) continue

      const colonIndex = line.indexOf(": ")
      if (colonIndex > -1) {
        const name = line.substring(0, colonIndex)
        const value = line.substring(colonIndex + 2)

        // Check if this is content-disposition to extract part name
        if (name.toLowerCase() === "content-disposition") {
          const nameMatch = value.match(/name="([^"]+)"/)
          if (nameMatch) {
            partName = nameMatch[1]
          }
        } else {
          // Store all other headers/fields
          partData[name] = value
        }
      }
    }

    // Store the part data under its name
    if (partName && Object.keys(partData).length > 0) {
      result[partName] = partData
    }
  }

  return result
}

/**
 * Unflatten paths with '/'
 */
const unflattenPaths = (obj, typeMap = {}) => {
  const result = {}

  // Check if there are any paths to unflatten
  const hasPathsToUnflatten = Object.keys(obj).some(key => key.includes("/"))

  // If no paths to unflatten, return the object as-is
  if (!hasPathsToUnflatten) {
    return obj
  }

  // First pass: collect all keys and sort them to process parents before children
  const sortedKeys = Object.keys(obj).sort()

  for (const key of sortedKeys) {
    const value = obj[key]

    if (key.includes("/")) {
      const parts = key.split("/")
      let current = result
      let i = 0

      while (i < parts.length - 1) {
        let part = parts[i]

        // Check if we have consecutive empty parts (multiple slashes)
        if (part === "" && i + 1 < parts.length && parts[i + 1] === "") {
          // Skip empty parts until we find a non-empty one or reach the end
          let j = i
          while (j < parts.length && parts[j] === "") {
            j++
          }
          // Use "/" as the key for multiple slashes
          part = "/"
          i = j - 1 // Will be incremented at the end of loop
        } else if (part === "") {
          // Single empty part at the beginning or middle, skip it
          i++
          continue
        }

        if (!current[part]) {
          current[part] = {}
        }
        current = current[part]
        i++
      }

      // Handle the final part
      const finalPart = parts[parts.length - 1]
      current[finalPart] = value
    } else {
      result[key] = value
    }
  }

  // Second pass: convert objects with numeric keys to arrays only if they have type="list"
  const convertToArraysRecursive = (obj, parentKey = "") => {
    if (Array.isArray(obj)) {
      return obj.map((item, index) =>
        convertToArraysRecursive(item, `${parentKey}/${index + 1}`)
      )
    } else if (obj && typeof obj === "object") {
      // Check if this object has type="list" in typeMap
      const hasListType = typeMap[parentKey] === "list"

      // Only convert to array if it has numeric keys AND type="list"
      if (hasListType) {
        const converted = maybeConvertToArray(obj)
        if (Array.isArray(converted)) {
          return converted.map((item, index) =>
            convertToArraysRecursive(item, `${parentKey}/${index + 1}`)
          )
        }
      }

      // Otherwise, keep as object and recurse
      const result = {}
      for (const [key, value] of Object.entries(obj)) {
        const childKey = parentKey ? `${parentKey}/${key}` : key
        result[key] = convertToArraysRecursive(value, childKey)
      }
      return result
    }
    return obj
  }

  return convertToArraysRecursive(result)
}

/**
 * Helper to check if a key pattern suggests an array structure
 */
const isArrayKey = (obj, currentKey, partIndex) => {
  const parts = currentKey.split("/")
  const prefix = parts.slice(0, partIndex + 1).join("/")

  // Check if there are other keys with the same prefix but numeric suffixes
  for (const key of Object.keys(obj)) {
    if (key.startsWith(prefix + "/") && key !== currentKey) {
      const otherParts = key.split("/")
      if (
        otherParts.length > partIndex + 1 &&
        /^\d+$/.test(otherParts[partIndex + 1])
      ) {
        return true
      }
    }
  }

  return false
}

/**
 * Check if a string contains binary data (non-printable characters)
 * @param {string} str - The string to check
 * @returns {boolean} True if binary data detected
 */
const isBinaryString = str => {
  if (!str || typeof str !== "string") return false

  // Check for non-printable characters (excluding common whitespace)
  for (let i = 0; i < str.length; i++) {
    const code = str.charCodeAt(i)
    // Allow tab (9), newline (10), carriage return (13), and printable ASCII (32-126)
    if (code < 9 || (code > 13 && code < 32) || code > 126) {
      return true
    }
  }
  return false
}

/**
 * Convert binary string to Buffer
 * @param {string} str - Binary string to convert
 * @returns {Buffer} Buffer representation of the string
 */
const stringToBuffer = str => {
  const buffer = Buffer.alloc(str.length)
  for (let i = 0; i < str.length; i++) {
    buffer[i] = str.charCodeAt(i)
  }
  return buffer
}

/**
 * Original from function - extracts and converts only signed components
 * @param {Object} http - HTTP message object with headers, body, and status
 * @returns {Object|null} Converted signed components or null if not signed
 */
export const from = http => {
  const input =
    http.headers["signature-input"] || http.headers["Signature-Input"]
  if (!input) {
    return null
  }

  // Decode signature inputs
  const inputs = decodeSigInput(input)

  // Process the first signature (following the original logic)
  for (const k in inputs) {
    const sigData = inputs[k]

    // Extract only the signed components
    const extractedComponents = extract(http, sigData.components)

    // Check if ao-result header is present and points to body
    const aoResult = http.headers["ao-result"] || http.headers["Ao-Result"]
    if (aoResult === "body" && extractedComponents.body) {
      // Check if body is binary data
      if (isBinaryString(extractedComponents.body)) {
        return stringToBuffer(extractedComponents.body)
      }
    }

    // Convert the extracted components to JSON format
    const result = toJSON(extractedComponents)

    // Handle ao-result if present
    if (aoResult && result[aoResult] !== undefined) {
      // Return the value of the key specified by ao-result
      return result[aoResult]
    }

    return result
  }

  return null
}

/**
 * Extract all keys and signature information from HTTP signature message
 * @param {Object} http - HTTP message object with headers and body
 * @returns {Object} Object containing all extracted signature data
 */
export const extractKeys = http => {
  const result = {
    signatures: {},
    keys: {},
    boundary: null,
    requiresBody: false,
    body: http.body ? toBuffer(http.body) : null,
    bodyText: typeof http.body === "string" ? http.body : null,
  }

  // Get multipart boundary if present
  result.boundary = getBoundary(http)

  // Get signature header
  const signatureHeader = http.headers.signature || http.headers.Signature
  if (!signatureHeader) {
    return result
  }

  // Get signature-input header
  const signatureInput =
    http.headers["signature-input"] || http.headers["Signature-Input"]
  if (!signatureInput) {
    return result
  }

  // Decode all signature inputs
  const inputs = decodeSigInput(signatureInput)

  // Parse signature header to extract actual signatures
  // Format: sig1=:base64signature:, sig2=:base64signature:
  const signatures = {}
  const sigPattern = /([a-zA-Z0-9-]+)=:([^:]+):/g
  let match
  while ((match = sigPattern.exec(signatureHeader)) !== null) {
    signatures[match[1]] = match[2]
  }

  // Process each signature
  for (const [sigName, sigData] of Object.entries(inputs)) {
    const extractedValues = extract(http, sigData.components)

    // Check if body is required
    if (extractedValues.__bodyRequired__) {
      result.requiresBody = true
    }

    const signatureInfo = {
      name: sigName,
      signature: signatures[sigName] || null,
      components: sigData.components,
      params: sigData.params,
      extractedValues: extractedValues,
      hasContentDigest: sigData.components.some(
        c => c.replace(/"/g, "").toLowerCase() === "content-digest"
      ),
    }

    // If has content-digest, verify it
    if (signatureInfo.hasContentDigest && result.body) {
      const contentDigest =
        http.headers["content-digest"] || http.headers["Content-Digest"]
      if (contentDigest) {
        signatureInfo.contentDigestVerification = verifyContentDigest(
          contentDigest,
          result.body
        )
      }
    }

    // Extract key information from params
    if (sigData.params.keyid) {
      try {
        const keyBuffer = base64url.toBuffer(sigData.params.keyid)
        result.keys[sigName] = {
          keyid: sigData.params.keyid,
          keyBuffer: keyBuffer,
          algorithm: sigData.params.alg || "unknown",
        }
      } catch (e) {
        // If keyid is not base64url encoded, store as-is
        result.keys[sigName] = {
          keyid: sigData.params.keyid,
          algorithm: sigData.params.alg || "unknown",
        }
      }
    }

    result.signatures[sigName] = signatureInfo
  }

  return result
}

/**
 * Verify if a message has valid HTTP signature structure
 * @param {Object} http - HTTP message object
 * @returns {boolean} True if message has valid signature structure
 */
export const hasValidSignature = http => {
  const hasSignature =
    (http.headers.signature || http.headers.Signature) !== undefined
  const hasSignatureInput =
    (http.headers["signature-input"] || http.headers["Signature-Input"]) !==
    undefined
  return hasSignature && hasSignatureInput
}

/**
 * Verify content-digest header against body
 * @param {string} contentDigest - Content-Digest header value
 * @param {string|Buffer} body - Request/response body
 * @returns {Object} Verification result with digest info
 */
export const verifyContentDigest = (contentDigest, body) => {
  // Parse content-digest header format: algorithm=:base64digest:
  const match = contentDigest.match(/([^=]+)=:([^:]+):/)
  if (!match) {
    return { valid: false, error: "Invalid content-digest format" }
  }

  const [, algorithm, expectedDigest] = match

  try {
    // Convert body to Buffer if needed
    const bodyBuffer =
      typeof body === "string" ? Buffer.from(body, "utf-8") : body

    // Calculate digest based on algorithm
    let actualDigest
    const crypto = require("crypto")

    if (algorithm === "sha-256") {
      const hash = crypto.createHash("sha256")
      hash.update(bodyBuffer)
      actualDigest = hash.digest("base64")
    } else if (algorithm === "sha-512") {
      const hash = crypto.createHash("sha512")
      hash.update(bodyBuffer)
      actualDigest = hash.digest("base64")
    } else {
      return { valid: false, error: `Unsupported algorithm: ${algorithm}` }
    }

    return {
      valid: actualDigest === expectedDigest,
      algorithm,
      expectedDigest,
      actualDigest,
      matches: actualDigest === expectedDigest,
    }
  } catch (error) {
    return { valid: false, error: error.message }
  }
}

/**
 * Get all signature names from a message
 * @param {Object} http - HTTP message object
 * @returns {Array} Array of signature names
 */
export const getSignatureNames = http => {
  const signatureInput =
    http.headers["signature-input"] || http.headers["Signature-Input"]
  if (!signatureInput) return []

  const inputs = decodeSigInput(signatureInput)
  return Object.keys(inputs)
}
