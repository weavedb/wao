import {
  hasNonAscii,
  sha256,
  hasNewline,
  isBytes,
  isPojo,
} from "./encode-utils.js"

// Helper functions
const isEmpty = value => {
  if (typeof value === "string") return value === ""
  if (Array.isArray(value)) return value.length === 0
  if (isPojo(value)) return Object.keys(value).length === 0
  if (isBytes(value)) return value.length === 0 || value.byteLength === 0
  return false
}

const hasOnlyEmptyValues = obj => {
  if (!isPojo(obj)) return false
  return Object.values(obj).every(
    v =>
      (typeof v === "string" && v === "") ||
      (Array.isArray(v) && v.length === 0) ||
      (isPojo(v) && Object.keys(v).length === 0)
  )
}

const isSimpleValue = value => {
  return (
    typeof value === "string" ||
    typeof value === "number" ||
    typeof value === "boolean" ||
    value === null ||
    value === undefined ||
    typeof value === "symbol"
  )
}

const getValueByPath = (obj, path) => {
  const parts = path.split("/")
  let value = obj
  for (const part of parts) {
    if (/^\d+$/.test(part)) {
      value = value[parseInt(part) - 1]
    } else {
      value = value[part]
    }
  }
  return value
}

// Array analysis helper
const analyzeArray = array => {
  const analysis = {
    hasObjects: false,
    hasArrays: false,
    hasNonObjects: false,
    hasArraysOfObjects: false,
    hasEmptyStrings: false,
    hasEmptyObjects: false,
    hasNonEmptyObjects: false,
    hasObjectsWithOnlyEmptyValues: false,
    hasOnlyEmptyElements: true,
  }

  for (const item of array) {
    if (isPojo(item)) {
      analysis.hasObjects = true
      if (Object.keys(item).length === 0) {
        analysis.hasEmptyObjects = true
      } else {
        analysis.hasNonEmptyObjects = true
        if (hasOnlyEmptyValues(item)) {
          analysis.hasObjectsWithOnlyEmptyValues = true
        }
      }
    } else if (Array.isArray(item)) {
      analysis.hasArrays = true
      if (item.some(subItem => isPojo(subItem))) {
        analysis.hasArraysOfObjects = true
      }
    } else {
      analysis.hasNonObjects = true
    }

    if (typeof item === "string" && item === "") {
      analysis.hasEmptyStrings = true
    }

    if (!isEmpty(item)) {
      analysis.hasOnlyEmptyElements = false
    }
  }

  return analysis
}

// Body key collector class
class BodyKeyCollector {
  constructor(obj) {
    this.obj = obj
    this.keys = []
  }

  collect() {
    this.processRootObject()
    return this.deduplicateAndFilter()
  }

  // Process top-level object
  processRootObject() {
    const objKeys = Object.keys(this.obj)

    for (const [key, value] of Object.entries(this.obj)) {
      if (this.isSpecialDataBodyField(key, value, objKeys)) {
        this.keys.push(key)
      } else if (Array.isArray(value) && value.length > 0) {
        this.processRootArray(key, value)
      } else if (isPojo(value)) {
        this.processRootNestedObject(key, value)
      } else if (this.needsBodyKey(key, value)) {
        this.keys.push(key)
      }
    }
  }

  // Check if field is special data/body field
  isSpecialDataBodyField(key, value, objKeys) {
    if (
      (key === "data" || key === "body") &&
      isSimpleValue(value) &&
      objKeys.length > 1
    ) {
      const otherKey = key === "data" ? "body" : "data"
      const otherValue = this.obj[otherKey]

      if (
        otherValue &&
        isPojo(otherValue) &&
        Object.keys(otherValue).length > 0
      ) {
        return false
      }
      return true
    }
    return false
  }

  // Check if value needs a body key
  needsBodyKey(key, value) {
    return (
      (isBytes(value) && value.length > 0) ||
      (typeof value === "string" && value.includes("\n")) ||
      (typeof value === "string" && hasNonAscii(value))
    )
  }

  // Process root-level arrays
  processRootArray(key, array) {
    const analysis = analyzeArray(array)
    let bodyPartCounter = 1

    if (analysis.hasArraysOfObjects) {
      // Handle arrays of arrays containing objects
      array.forEach((item, index) => {
        if (Array.isArray(item)) {
          item.forEach((subItem, subIndex) => {
            if (isPojo(subItem)) {
              this.keys.push(`${key}/${index + 1}/${subIndex + 1}`)
            }
          })
        }
      })
      this.keys.push(key)
    } else if (
      analysis.hasObjects &&
      (analysis.hasEmptyStrings || analysis.hasEmptyObjects) &&
      !analysis.hasObjectsWithOnlyEmptyValues
    ) {
      // Mixed array: only non-empty objects get parts
      array.forEach(item => {
        if (isPojo(item) && Object.keys(item).length > 0) {
          const path = `${key}/${bodyPartCounter}`
          this.keys.push(path)
          this.addNestedObjectPaths(item, path)
        }
        bodyPartCounter++
      })
      this.keys.push(key)
    } else if (analysis.hasObjects) {
      // Regular array with objects
      const skipEmptyObjects = analysis.hasOnlyEmptyElements

      array.forEach(item => {
        if (isPojo(item)) {
          if (!(skipEmptyObjects && Object.keys(item).length === 0)) {
            const path = `${key}/${bodyPartCounter}`
            this.keys.push(path)
            if (Object.keys(item).length > 0) {
              this.addNestedObjectPaths(item, path)
            }
          }
        } else if (typeof item === "string" && item === "") {
          this.keys.push(`${key}/${bodyPartCounter}`)
        }
        bodyPartCounter++
      })

      // Add main array key conditionally
      if (!analysis.hasObjectsWithOnlyEmptyValues || analysis.hasNonObjects) {
        if (!analysis.hasOnlyEmptyElements) {
          this.keys.push(key)
        }
      }
    } else {
      // Simple array without objects
      const hasOnlyEmptyArraysOrObjects = array.every(
        item =>
          isEmpty(item) &&
          (Array.isArray(item) || isPojo(item) || typeof item === "string")
      )

      if (hasOnlyEmptyArraysOrObjects || !array.every(isEmpty)) {
        this.keys.push(key)
      }
    }
  }

  // Add paths for nested objects within an object
  addNestedObjectPaths(obj, basePath) {
    for (const [nestedKey, nestedValue] of Object.entries(obj)) {
      if (isPojo(nestedValue) && Object.keys(nestedValue).length > 0) {
        this.keys.push(`${basePath}/${nestedKey}`)
      }
    }
  }

  // Process root-level nested object
  processRootNestedObject(key, obj) {
    // Check for arrays with only empty elements
    for (const [k, v] of Object.entries(obj)) {
      if (Array.isArray(v) && v.length > 0 && v.every(isEmpty)) {
        this.keys.push(`${key}/${k}`)
      }
    }

    // Traverse the object
    this.traverse(obj, key)
  }

  // Traverse nested structures
  traverse(current, path) {
    let hasSimpleFields = false
    const nestedPaths = []
    let hasArraysWithObjects = false

    // First pass: analyze structure
    for (const [key, value] of Object.entries(current)) {
      const fullPath = path ? `${path}/${key}` : key
      const result = this.analyzeFieldInTraversal(value, fullPath, nestedPaths)

      hasSimpleFields = hasSimpleFields || result.hasSimpleFields
      hasArraysWithObjects = hasArraysWithObjects || result.hasArraysWithObjects
    }

    // Add current path if needed
    if (hasSimpleFields || (hasArraysWithObjects && path)) {
      this.keys.push(path)
    }

    // Second pass: check for arrays with only empty elements
    for (const [key, value] of Object.entries(current)) {
      const fullPath = path ? `${path}/${key}` : key
      if (Array.isArray(value) && value.length > 0 && value.every(isEmpty)) {
        this.keys.push(fullPath)
      }
    }

    // Traverse nested objects
    for (const nestedPath of nestedPaths) {
      const nestedObj = getValueByPath(this.obj, nestedPath)
      if (isPojo(nestedObj)) {
        this.traverse(nestedObj, nestedPath)
      }
    }
  }

  // Analyze a field during traversal
  analyzeFieldInTraversal(value, fullPath, nestedPaths) {
    let hasSimpleFields = false
    let hasArraysWithObjects = false

    if (Array.isArray(value)) {
      if (value.length === 0) {
        hasSimpleFields = true
      } else {
        const analysis = analyzeArray(value)

        if (analysis.hasObjects) {
          hasArraysWithObjects = true
          this.processArrayInTraversal(value, fullPath, nestedPaths, analysis)

          if (analysis.hasNonObjects) {
            hasSimpleFields = true
            this.keys.push(fullPath)
          }
        } else {
          hasSimpleFields = true
        }
      }
    } else if (isPojo(value)) {
      if (Object.keys(value).length === 0) {
        hasSimpleFields = true
      } else if (hasOnlyEmptyValues(value)) {
        this.keys.push(fullPath)
      } else {
        const hasArraysWithOnlyEmptyElements = Object.entries(value).some(
          ([k, v]) => Array.isArray(v) && v.length > 0 && v.every(isEmpty)
        )

        if (hasArraysWithOnlyEmptyElements) {
          this.keys.push(fullPath)
        }
        nestedPaths.push(fullPath)
      }
    } else if ((isBytes(value) && value.length > 0) || isSimpleValue(value)) {
      hasSimpleFields = true
    }

    return { hasSimpleFields, hasArraysWithObjects }
  }

  // Process arrays found during traversal
  processArrayInTraversal(array, fullPath, nestedPaths, analysis) {
    if (
      (analysis.hasEmptyStrings || analysis.hasEmptyObjects) &&
      analysis.hasNonEmptyObjects
    ) {
      // Special case: only non-empty objects get parts
      array.forEach((item, index) => {
        if (isPojo(item) && Object.keys(item).length > 0) {
          const itemPath = `${fullPath}/${index + 1}`
          this.keys.push(itemPath)
          nestedPaths.push(itemPath)
        }
      })
    } else if (
      analysis.hasObjectsWithOnlyEmptyValues &&
      !analysis.hasNonObjects
    ) {
      // Objects with only empty values
      array.forEach((item, index) => {
        if (isPojo(item)) {
          const itemPath = `${fullPath}/${index + 1}`
          this.keys.push(itemPath)
          if (Object.keys(item).length > 0) {
            nestedPaths.push(itemPath)
          }
        }
      })
    } else {
      // Normal case
      array.forEach((item, index) => {
        if (isPojo(item)) {
          const itemPath = `${fullPath}/${index + 1}`
          this.keys.push(itemPath)
          if (Object.keys(item).length > 0) {
            nestedPaths.push(itemPath)
          }
        }
      })
    }
  }

  // Deduplicate and filter results
  deduplicateAndFilter() {
    const uniqueKeys = [...new Set(this.keys)]

    return uniqueKeys.filter(k => {
      if (!k) return false

      // Check if this is a path to an element inside an array with only empty elements
      const parts = k.split("/")
      if (parts.length >= 2 && /^\d+$/.test(parts[parts.length - 1])) {
        const arrayPath = parts.slice(0, -1).join("/")
        const arrayValue = getValueByPath(this.obj, arrayPath)

        if (Array.isArray(arrayValue) && arrayValue.every(isEmpty)) {
          return false
        }
      }

      return true
    })
  }
}

export default function collectBodyKeys(obj, prefix = "") {
  const collector = new BodyKeyCollector(obj)
  return collector.collect()
}
