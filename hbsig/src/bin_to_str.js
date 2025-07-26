/**
 * Convert a Buffer to string if it contains valid UTF-8 text
 * @param {Buffer} buffer - Buffer to check and potentially convert
 * @returns {string|Buffer} - String if valid UTF-8, otherwise original Buffer
 */
export default function bin_to_str(buffer) {
  if (!Buffer.isBuffer(buffer)) {
    return buffer
  }

  // Empty buffer stays as buffer
  if (buffer.length === 0) {
    return buffer
  }

  try {
    const str = buffer.toString("utf8")
    // Check if it's valid UTF-8 by seeing if it round-trips correctly
    if (Buffer.from(str, "utf8").equals(buffer)) {
      // Additional check: ensure all characters are printable or common whitespace
      // This prevents converting binary data that happens to be valid UTF-8
      let isPrintable = true
      for (let i = 0; i < str.length; i++) {
        const code = str.charCodeAt(i)
        // Allow printable ASCII (32-126) and common whitespace (tab, newline, carriage return)
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
        return str
      }
    }
  } catch (e) {
    // Not valid UTF-8, return original buffer
  }

  return buffer
}
