import { toBuffer, formatFloat, isBytes, isPojo } from "./encode-utils.js"

// Helper to generate the correct number of backslashes for a given nesting level
function getBackslashes(level) {
  return "\\".repeat(Math.pow(2, level) - 1)
}

// Helper to encode primitive values at a specific nesting level
function encodePrimitiveAtLevel(value, level) {
  const bs = getBackslashes(level)

  if (typeof value === "number") {
    if (Number.isInteger(value)) {
      return `${bs}"(ao-type-integer) ${value}${bs}"`
    } else {
      return `${bs}"(ao-type-float) ${formatFloat(value)}${bs}"`
    }
  } else if (typeof value === "string") {
    return `${bs}"${value}${bs}"`
  } else if (value === null) {
    const innerBs = getBackslashes(level + 1)
    return `${bs}"(ao-type-atom) ${innerBs}"null${innerBs}"${bs}"`
  } else if (value === undefined) {
    const innerBs = getBackslashes(level + 1)
    return `${bs}"(ao-type-atom) ${innerBs}"undefined${innerBs}"${bs}"`
  } else if (typeof value === "symbol") {
    const desc = value.description || "Symbol.for()"
    const innerBs = getBackslashes(level + 1)
    return `${bs}"(ao-type-atom) ${innerBs}"${desc}${innerBs}"${bs}"`
  } else if (typeof value === "boolean") {
    const innerBs = getBackslashes(level + 1)
    return `${bs}"(ao-type-atom) ${innerBs}"${value}${innerBs}"${bs}"`
  } else {
    return `${bs}"${String(value)}${bs}"`
  }
}

// Recursive function to handle arrays at any nesting level
function encodeArrayAtLevel(items, level) {
  // The original code only goes 3 levels deep for arrays
  if (level >= 3) {
    const bs = getBackslashes(level)
    const stringItems = items
      .map(item => {
        if (typeof item === "number") {
          if (Number.isInteger(item)) {
            return `${bs}"(ao-type-integer) ${item}${bs}"`
          } else {
            return `${bs}"(ao-type-float) ${formatFloat(item)}${bs}"`
          }
        } else if (typeof item === "string") {
          return `${bs}"${item}${bs}"`
        } else {
          return `${bs}"${String(item)}${bs}"`
        }
      })
      .join(", ")
    return `${getBackslashes(level - 1)}"(ao-type-list) ${stringItems}${getBackslashes(level - 1)}"`
  }

  const encodedItems = items
    .map(item => {
      if (Array.isArray(item)) {
        return encodeArrayAtLevel(item, level + 1)
      } else {
        return encodePrimitiveAtLevel(item, level)
      }
    })
    .join(", ")

  if (level === 0) {
    return `"(ao-type-list) ${encodedItems}"`
  } else {
    const bs = getBackslashes(level - 1)
    return `${bs}"(ao-type-list) ${encodedItems}${bs}"`
  }
}

export default function encodeArrayItem(item) {
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
    return encodeArrayAtLevel(item, 1)
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
