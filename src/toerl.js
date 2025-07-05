export default function toErl(obj, indent = 0) {
  const spaces = " ".repeat(indent)

  // Handle null (as atom)
  if (obj === null) {
    return "null" // Erlang atom
  }

  // Handle undefined (as atom)
  if (obj === undefined) {
    return "undefined" // Erlang atom
  }

  // Handle boolean (as atom)
  if (typeof obj === "boolean") {
    return obj.toString() // true/false are atoms in Erlang
  }

  // Handle numbers
  if (typeof obj === "number") {
    if (Number.isInteger(obj)) {
      return obj.toString()
    } else {
      // Format float properly
      return obj.toString()
    }
  }

  // Handle strings
  if (typeof obj === "string") {
    // Escape special characters
    const escaped = obj
      .replace(/\\/g, "\\\\")
      .replace(/"/g, '\\"')
      .replace(/\n/g, "\\n")
      .replace(/\r/g, "\\r")
      .replace(/\t/g, "\\t")
    return `<<"${escaped}">>`
  }

  // Handle symbols (as atom)
  if (typeof obj === "symbol") {
    const desc = obj.description || "Symbol.for()"
    // Check if it's a simple atom or needs quoting
    if (/^[a-z][a-zA-Z0-9_]*$/.test(desc)) {
      return desc // Simple atom like: ok, error, undefined
    } else {
      return `'${desc}'` // Quoted atom for special cases
    }
  }

  // Handle Buffer/Binary
  if (
    Buffer.isBuffer(obj) ||
    (obj &&
      typeof obj === "object" &&
      obj.type === "Buffer" &&
      Array.isArray(obj.data))
  ) {
    const data = Buffer.isBuffer(obj) ? obj : Buffer.from(obj.data)
    if (data.length === 0) {
      return "<<>>"
    }
    const bytes = Array.from(data).join(",")
    return `<<${bytes}>>`
  }

  // Handle Arrays
  if (Array.isArray(obj)) {
    if (obj.length === 0) {
      return "[]"
    }

    const items = obj.map(item => toErl(item, indent + 2))

    // Check if it fits on one line
    const oneLine = `[${items.join(", ")}]`
    if (oneLine.length <= 80 && !items.some(item => item.includes("\n"))) {
      return oneLine
    }

    // Multi-line format
    const inner = items.map(item => `${spaces}  ${item}`).join(",\n")
    return `[\n${inner}\n${spaces}]`
  }

  // Handle Objects/Maps
  if (typeof obj === "object" && obj !== null) {
    const entries = Object.entries(obj)

    if (entries.length === 0) {
      return "#{}"
    }

    const pairs = entries.map(([key, value]) => {
      const erlangKey = `<<"${key}">>`
      const erlangValue = toErl(value, indent + 2)
      return `${erlangKey} => ${erlangValue}`
    })

    // Check if it fits on one line
    const oneLine = `#{ ${pairs.join(", ")} }`
    if (oneLine.length <= 80 && !pairs.some(pair => pair.includes("\n"))) {
      return oneLine
    }

    // Multi-line format
    const inner = pairs.map(pair => `${spaces}  ${pair}`).join(",\n")
    return `#{\n${inner}\n${spaces}}`
  }

  // Fallback
  return "unknown"
}

// Helper function to convert JSON string or object
function convert(input) {
  let obj

  // If input is a string, try to parse it as JSON
  if (typeof input === "string") {
    try {
      // First try to parse as JSON
      obj = JSON.parse(input)
    } catch (e) {
      // If it fails, try to evaluate it (for cases with Symbol, Buffer, etc.)
      try {
        obj = eval(`(${input})`)
      } catch (e2) {
        throw new Error("Invalid input: not valid JSON or JavaScript object")
      }
    }
  } else {
    obj = input
  }

  return toErl(obj)
}

// Function to format test case for Erlang
function formatTestCase(obj, testName = "test") {
  const erlangData = toErl(obj, 2)

  // If it's a simple one-liner, put it inline
  if (!erlangData.includes("\n") && erlangData.length < 60) {
    return `${testName}_test() -> show(${erlangData}).`
  }

  // Multi-line format
  return `${testName}_test() -> 
  show(${erlangData}).`
}

// Function to convert multiple test cases
function convertMultiple(testCases, baseTestName = "generated") {
  return testCases
    .map((testCase, index) => {
      const testName = `${baseTestName}_${index + 1}`
      return formatTestCase(testCase, testName)
    })
    .join("\n\n")
}
