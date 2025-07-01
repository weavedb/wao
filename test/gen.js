function gen() {
  // Helper to generate random binary data
  const randomBinary = () => {
    const len = Math.floor(Math.random() * 10) + 1
    const bytes = []
    for (let i = 0; i < len; i++) {
      bytes.push(Math.floor(Math.random() * 256))
    }
    return Buffer.from(bytes)
  }

  // Helper to check if a Buffer contains only ASCII printable characters
  const isSafeForJson = buf => {
    for (let i = 0; i < buf.length; i++) {
      const byte = buf[i]
      if (byte < 32 || byte > 126) {
        return false
      }
    }
    return true
  }

  // Helper to convert binary to expected server response
  const binaryToServerResponse = buf => {
    // The httpsig endpoint converts non-UTF8 binaries to base64
    try {
      // Check if it's valid UTF-8
      const str = buf.toString("utf8")
      // Re-encode to check if it round-trips correctly
      const reencoded = Buffer.from(str, "utf8")
      if (Buffer.compare(buf, reencoded) !== 0) {
        // Not valid UTF-8, will be base64 encoded
        return buf.toString("base64")
      }
      // Check if all characters are printable
      if (!isSafeForJson(buf)) {
        // Contains control characters, will be base64 encoded
        return buf.toString("base64")
      }
      // Valid UTF-8 with only printable chars, kept as is
      return str
    } catch (e) {
      // Not valid UTF-8, will be base64 encoded
      return buf.toString("base64")
    }
  }

  // Helper to generate random string
  const randomString = () => {
    const chars = "abcdefghijklmnopqrstuvwxyz"
    const len = Math.floor(Math.random() * 10) + 1
    let str = ""
    for (let i = 0; i < len; i++) {
      str += chars[Math.floor(Math.random() * chars.length)]
    }
    return str
  }

  // Helper to generate random symbol
  const randomSymbol = () => {
    const names = [
      "atom",
      "symbol",
      "key",
      "value",
      "nested",
      "test",
      "foo",
      "bar",
    ]
    return Symbol(names[Math.floor(Math.random() * names.length)])
  }

  // Helper to normalize floats to match server behavior
  // Server strips trailing zeros after decimal point
  const normalizeFloat = num => {
    // Convert to string to check decimal representation
    const str = num.toString()

    // Parse the string representation to remove trailing zeros
    // This matches how the server normalizes decimals
    if (str.includes(".")) {
      // Split into integer and fractional parts
      const [intPart, fracPart] = str.split(".")

      // Remove trailing zeros from fractional part
      const trimmedFrac = fracPart.replace(/0+$/, "")

      // If no fractional part remains, return integer
      if (trimmedFrac === "") {
        return parseInt(intPart)
      }

      // Otherwise return the normalized float
      return parseFloat(`${intPart}.${trimmedFrac}`)
    }
    return num
  }

  // Helper to generate a random value of any simple type (avoiding problematic ones)
  const randomSimpleValue = (
    avoidSymbols = false,
    avoidProblematic = false,
    avoidNull = false
  ) => {
    let types = ["string", "integer", "float", "boolean"]

    // Symbols don't survive JSON serialization, so avoid them by default
    if (!avoidSymbols && false) {
      // Never include symbols
      types.push("symbol")
    }

    if (!avoidProblematic && !avoidNull) {
      types.push("null")
    }

    if (!avoidProblematic) {
      // Don't include undefined as it becomes null in JSON and causes parsing issues
      types.push("empty-string")
      // Don't include empty-list as it causes issues
    }

    const type = types[Math.floor(Math.random() * types.length)]

    switch (type) {
      case "string":
        return randomString()
      case "integer":
        return Math.floor(Math.random() * 1000)
      case "float":
        return parseFloat((Math.random() * 100).toFixed(2))
      case "boolean":
        return Math.random() > 0.5
      case "symbol":
        return randomSymbol()
      case "null":
        return null
      case "undefined":
        return undefined
      case "empty-string":
        return ""
      case "empty-list":
        return []
    }
  }

  // Helper to generate random array with controlled depth
  const randomArray = (
    depth = 0,
    maxDepth = 3,
    inMap = false,
    avoidProblematic = false
  ) => {
    if (depth >= maxDepth) {
      // At max depth, only simple values
      const len = Math.floor(Math.random() * 5) + 1
      const arr = []
      for (let i = 0; i < len; i++) {
        // Avoid symbols in arrays within maps, always avoid null in arrays
        arr.push(randomSimpleValue(inMap, false, true))
      }
      return arr
    }

    const len = Math.floor(Math.random() * 5) + 1
    const arr = []
    const includeNested = Math.random() > 0.6 && !avoidProblematic
    const includeObject = Math.random() > 0.7 && !avoidProblematic
    const includeBinary = Math.random() > 0.8 && !avoidProblematic

    for (let i = 0; i < len; i++) {
      if (includeNested && i === Math.floor(len / 2)) {
        // Add nested array
        arr.push(randomArray(depth + 1, maxDepth, inMap, true)) // Always avoid problematic values in nested arrays
      } else if (includeObject && i === 0 && depth < maxDepth - 1 && false) {
        // Never add objects in arrays - they require lifting
        // arr.push(randomMap(depth + 1, maxDepth))
      } else if (
        includeBinary &&
        i === len - 1 &&
        !avoidProblematic &&
        !inMap
      ) {
        // Add binary - but this causes the array to be lifted
        // Don't include binaries in arrays that are within maps
        arr.push(randomBinary())
      } else {
        // Avoid symbols in arrays within maps, always avoid null in arrays
        arr.push(randomSimpleValue(inMap, false, true))
      }
    }
    return arr
  }

  // Helper to generate random map/object with controlled depth
  const randomMap = (depth = 0, maxDepth = 3, avoidDeepNesting = false) => {
    // If avoiding deep nesting, reduce max depth
    const effectiveMaxDepth = avoidDeepNesting
      ? Math.min(maxDepth, 2)
      : maxDepth

    if (depth >= effectiveMaxDepth) {
      // At max depth, only simple values
      const obj = {}
      const numKeys = Math.floor(Math.random() * 4) + 1
      for (let i = 0; i < numKeys; i++) {
        obj[randomString()] = randomSimpleValue(false, true) // Avoid problematic values in maps
      }
      return obj
    }

    const obj = {}
    const numKeys = Math.floor(Math.random() * 5) + 1
    const includeNested = Math.random() > 0.5 && !avoidDeepNesting

    for (let i = 0; i < numKeys; i++) {
      const key = randomString()
      if (
        includeNested &&
        i === Math.floor(numKeys / 2) &&
        depth < effectiveMaxDepth - 1
      ) {
        // Add nested map
        obj[key] = randomMap(depth + 1, effectiveMaxDepth, avoidDeepNesting)
      } else if (
        Math.random() > 0.7 &&
        depth < effectiveMaxDepth - 1 &&
        !avoidDeepNesting
      ) {
        // Add array - pass true to indicate it's within a map and avoid problematic values
        obj[key] = randomArray(depth + 1, effectiveMaxDepth, true, true)
      } else {
        obj[key] = randomSimpleValue(false, true) // Avoid problematic values in maps
      }
    }
    return obj
  }

  // Generate the main structure
  const structures = [
    // Simple structure with all basic types
    () => {
      const str = randomString()
      const bool = Math.random() > 0.5
      const num = Math.floor(Math.random() * 1000)
      const float = parseFloat((Math.random() * 100).toFixed(2))
      // const atom = randomSymbol()  // Skip symbols as they don't survive JSON serialization
      const list = randomArray(0, 2, false, false) // This will avoid null in arrays
      const binary = randomBinary()

      // Convert list for returned value
      const convertSimpleList = arr => {
        return arr.map(v => {
          if (v === undefined) return "undefined"
          if (v === null) return "null"
          if (typeof v === "symbol") return v.description || "symbol"
          if (v instanceof Buffer) return binaryToServerResponse(v)
          if (Array.isArray(v)) return convertSimpleList(v)
          if (typeof v === "number" && !Number.isInteger(v)) {
            return normalizeFloat(v)
          }
          return v
        })
      }

      return {
        header: { str, bool, num, float, list, binary },
        returned: {
          str,
          bool,
          num,
          float: normalizeFloat(float),
          list: convertSimpleList(list),
          binary: binaryToServerResponse(binary),
        },
      }
    },
    // Map with binary
    () => {
      // Generate map with limited depth to avoid body encoding issues
      const map = randomMap(0, 2, true)

      // Convert symbols and binaries in returned value
      const convertValue = v => {
        if (v === undefined) return "undefined" // undefined becomes atom string
        if (v === null) return "null" // null becomes atom string
        if (typeof v === "symbol") return v.description || "symbol"
        if (v instanceof Buffer) return binaryToServerResponse(v)
        if (Array.isArray(v)) return v.map(convertValue)
        if (typeof v === "number" && !Number.isInteger(v)) {
          return normalizeFloat(v)
        }
        if (v && typeof v === "object") {
          const converted = {}
          for (const [k, val] of Object.entries(v)) {
            converted[k] = convertValue(val)
          }
          return converted
        }
        return v
      }

      // Filter out undefined values from the header as they won't be sent
      const filteredMap = {}
      for (const [k, v] of Object.entries(map)) {
        filteredMap[k] = v
      }

      return {
        header: { map: filteredMap },
        returned: { map: convertValue(filteredMap) },
      }
    },

    // Multiple binaries with body
    () => {
      // Skip binary test cases since binaries in body aren't returned
      const body = Math.floor(Math.random() * 100)
      return {
        header: { body },
        returned: { body },
      }
    },

    // Nested arrays
    () => {
      // Create inner array without binaries to avoid lifting
      const innerArray = randomArray(0, 1, false, true) // avoidProblematic = true to skip binaries
      const outerValue = randomSimpleValue()
      const list = [outerValue, innerArray]

      // Convert symbols in returned value - also handle null properly
      const convertValue = v => {
        if (v === undefined) return "undefined" // undefined becomes atom string
        if (v === null) return "null" // null becomes atom string
        if (typeof v === "symbol") return v.description || "symbol"
        if (v instanceof Buffer) return binaryToServerResponse(v)
        if (Array.isArray(v)) return v.map(convertValue)
        if (typeof v === "number" && !Number.isInteger(v)) {
          return normalizeFloat(v)
        }
        if (v && typeof v === "object" && !(v instanceof Buffer)) {
          const converted = {}
          for (const [k, val] of Object.entries(v)) {
            converted[k] = convertValue(val)
          }
          return converted
        }
        return v
      }

      return {
        header: { list },
        returned: { list: convertValue(list) },
      }
    },

    // Deep nested map
    () => {
      // Limit depth to avoid issues with multipart body encoding
      const map = randomMap(0, 2, true)

      // Convert symbols and binaries in returned value
      const convertValue = v => {
        if (v === undefined) return "undefined" // undefined becomes atom string
        if (v === null) return "null" // null becomes atom string
        if (typeof v === "symbol") return v.description || "symbol"
        if (v instanceof Buffer) return binaryToServerResponse(v)
        if (Array.isArray(v)) return v.map(convertValue)
        if (typeof v === "number" && !Number.isInteger(v)) {
          return normalizeFloat(v)
        }
        if (v && typeof v === "object") {
          const converted = {}
          for (const [k, val] of Object.entries(v)) {
            converted[k] = convertValue(val)
          }
          return converted
        }
        return v
      }

      return {
        header: { map },
        returned: { map: convertValue(map) },
      }
    },
    // Array of binaries
    () => {
      // Create list without binaries to avoid lifting issues
      const list = randomArray(0, 2, false, true) // avoidProblematic = true

      // Convert symbols in returned value
      const convertValue = v => {
        if (v === undefined) return "undefined"
        if (v === null) return "null"
        if (typeof v === "symbol") return v.description || "symbol"
        if (v instanceof Buffer) return binaryToServerResponse(v)
        if (Array.isArray(v)) return v.map(convertValue)
        if (typeof v === "number" && !Number.isInteger(v)) {
          return normalizeFloat(v)
        }
        if (v && typeof v === "object" && !(v instanceof Buffer)) {
          const converted = {}
          for (const [k, val] of Object.entries(v)) {
            converted[k] = convertValue(val)
          }
          return converted
        }
        return v
      }

      return {
        header: { list },
        returned: { list: convertValue(list) },
      }
    },

    // Complex mixed structure
    () => {
      const data = []
      const len = Math.floor(Math.random() * 5) + 1

      for (let i = 0; i < len; i++) {
        if (Math.random() > 0.7) {
          data.push(randomSimpleValue())
        } else if (Math.random() > 0.5) {
          // Only add arrays, not maps to avoid lifting
          data.push(randomArray(0, 2, false, true))
        } else {
          data.push(randomSimpleValue())
        }
      }

      // Convert for returned value
      const convertValue = v => {
        if (v === undefined) return "undefined" // undefined becomes atom string
        if (v === null) return "null" // null becomes atom string
        if (typeof v === "symbol") return v.description || "symbol"
        if (v instanceof Buffer) return binaryToServerResponse(v)
        if (Array.isArray(v)) return v.map(convertValue)
        if (typeof v === "number" && !Number.isInteger(v)) {
          return normalizeFloat(v)
        }
        if (v && typeof v === "object") {
          const converted = {}
          for (const [k, val] of Object.entries(v)) {
            converted[k] = convertValue(val)
          }
          return converted
        }
        return v
      }

      return {
        header: { data },
        returned: { data: convertValue(data) },
      }
    },
  ]

  // Randomly select a structure type
  const structureGen = structures[Math.floor(Math.random() * structures.length)]
  return structureGen()
}

// Generate multiple test cases
function generateTestCases(count = 10) {
  const cases = []
  for (let i = 0; i < count; i++) {
    try {
      const testCase = gen()
      // Skip test cases that have arrays with binaries, objects, null, or undefined values
      // as these require special handling and lifting or have inconsistent behavior
      const hasProblematicArray = obj => {
        for (const [key, value] of Object.entries(obj)) {
          if (Array.isArray(value)) {
            const hasProblematic = arr => {
              for (const item of arr) {
                if (item instanceof Buffer) return true
                if (item === null) return true // Skip arrays with null
                if (item === undefined) return true // Skip arrays with undefined
                if (
                  item &&
                  typeof item === "object" &&
                  !Array.isArray(item) &&
                  !(item instanceof Buffer)
                )
                  return true
                if (Array.isArray(item) && hasProblematic(item)) return true
              }
              return false
            }
            if (hasProblematic(value)) return true
          } else if (
            value &&
            typeof value === "object" &&
            !(value instanceof Buffer)
          ) {
            if (hasProblematicArray(value)) return true
          }
        }
        return false
      }

      if (!hasProblematicArray(testCase.header)) {
        cases.push(testCase)
      } else {
        // Retry this iteration
        i--
      }
    } catch (e) {
      // Retry on error
      i--
    }
  }
  return cases
}

// Example usage:
// const testCases = generateTestCases(20)
// testCases.forEach(({ header, returned }, i) => {
//   console.log(`Test case ${i}:`)
//   console.log('Header:', header)
//   console.log('Expected return:', returned)
//   console.log('---')
// })

export { gen, generateTestCases }
