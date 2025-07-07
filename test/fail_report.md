# Comprehensive Failing Cases Report

## Overview
This report documents all patterns that cause encoding failures in the system, along with a JavaScript detector to identify failing cases before encoding.

## Failing Patterns Analysis

### 1. Multiple Binary Fields
**Pattern**: Having more than one field with binary/buffer data (only `body` and `data` together are allowed)

**Examples**:
```javascript
{ binary: Buffer.from([1, 2, 3]), binary2: Buffer.from([1, 2, 3]) }  // FAILS
{ bin: Buffer.from([1]), file: Buffer.from([2]) }  // FAILS
{ body: Buffer.from([1]), data: Buffer.from([2]) }  // OK - special case
{ body: Buffer.from([1]), bin: Buffer.from([2]) }  // FAILS - bin is not special
{ data: Buffer.from([1]), file: Buffer.from([2]) }  // FAILS
```

**Why it fails**: The encoder expects at most one binary field, with the only exception being when BOTH `body` and `data` fields are present (special case). Multiple binary fields would require complex multipart encoding that isn't supported.

### 2. Arrays Containing Buffers
**Pattern**: Arrays that have Buffer objects as elements

**Examples**:
```javascript
{ bin: [Buffer.from([1]), Buffer.from([2])] }
{ list: [Symbol("ok"), [Buffer.from([1, 2, 3])]] }
{ files: [Buffer.from([1]), "text", Buffer.from([2])] }
```

**Why it fails**: Buffers inside arrays need special multipart handling that isn't implemented. The array encoding logic expects serializable values.

### 3. Empty Buffer in data Field
**Pattern**: Using an empty buffer as the value for the `data` field

**Examples**:
```javascript
{ data: Buffer.from([]) }
{ data: Buffer.from([]), body: Buffer.from([]) }
{ bin: Buffer.from([]), data: Buffer.from([]) }
{ bin: Buffer.from([]), data: Buffer.from([]), body: Buffer.from([]) }
```

**Why it fails**: Empty `data` field with buffer type has special semantic meaning in the protocol and conflicts with the encoding logic. The `data` field expects non-empty content when it's a buffer.

### 4. Large Numbers (Beyond 32-bit)
**Pattern**: Numbers that exceed 32-bit integer range

**Examples**:
```javascript
{ max_int64: 9223372036854775807 }  // 2^63 - 1
{ min_int64: -9223372036854775808 } // -2^63
{ large_positive: 1000000000000000 } // 10^15
{ large_negative: -1000000000000000 }
{ int_array: [9223372036854775807, -9223372036854775808, 0] }
{ boundary_ints: { max: 9223372036854775807, min: -9223372036854775808 } }
```

**Valid range**: -2147483648 to 2147483647 (32-bit signed integers)

**Why it fails**: The system uses 32-bit integer encoding. JavaScript's Number type can represent larger values, but the protocol cannot.

### 5. Field Name Case Conflicts
**Pattern**: Multiple fields with the same name but different case

**Examples**:
```javascript
{ data: 1, Data: 2, DATA: 3 }
{ body: "a", Body: "b", BODY: "c" }
{ test: 1, Test: 2, TEST: 3, TeSt: 4 }
{ a1: 1, A1: 2 }
```

**Why it fails**: The system treats field names case-insensitively for certain operations, causing conflicts when multiple case variations exist.

### 6. Special Characters in Field Names
**Pattern**: Field names containing special characters

**Examples**:
```javascript
{ "field!": 1, "field?": 2, "field#": 3 }
{ "field@": 1, "field$": 2, "field%": 3 }
{ "field&": 1, "field*": 2, "field(": 3 }
```

**Why it fails**: Special characters may have reserved meanings in the encoding format or cause parsing issues.

### 7. Numeric Field Names
**Pattern**: Pure numeric strings as field names

**Examples**:
```javascript
{ 123: "numeric", 456: "another" }
{ "123": "numeric", "456": "another" }
{ 0: "zero", 1: "one", 2: "two" }
```

**Why it fails**: Numeric field names conflict with array index notation in path resolution (e.g., "data/1" could mean array index or field name).

### 8. Empty or Whitespace Field Names
**Pattern**: Empty string or whitespace-only field names

**Examples**:
```javascript
{ "": "empty_key" }
{ " ": "space_key" }
{ "  ": "two_spaces" }
{ "\t": "tab_key" }
{ "\n": "newline_key" }
```

**Why it fails**: Empty or whitespace-only keys cannot be properly encoded in headers or form field names.

### 9. Complex Structures with Failing Elements
**Pattern**: Any nested structure containing the above failing patterns

**Examples**:
```javascript
{
  users: [
    { id: 9223372036854775807, name: "user1" }, // large number
    { id: -9223372036854775808, name: "user2" }
  ]
}
{
  data: [1, 9223372036854775807, "text", { val: 2 }], // large number in array
  Data: [3, -9223372036854775808, [], {}], // case conflict
  DATA: ["", null, Symbol("undefined"), Symbol("ok")]
}
{
  field: 1,
  Field: [2], // case conflict
  FIELD: { data: 3 },
  FiElD: [{ val: 4 }]
}
```

## JavaScript Failing Case Detector

```javascript
/**
 * Comprehensive detector for cases that will fail encoding
 * @param {object} obj - The object to check
 * @returns {object} - { isValid: boolean, errors: string[] }
 */
function detectFailingCase(obj) {
  const errors = [];
  const visited = new WeakSet();
  
  // Helper to check if value is a Buffer
  function isBuffer(value) {
    return Buffer.isBuffer(value) || 
           (value && typeof value === 'object' && value.type === 'Buffer' && Array.isArray(value.data));
  }
  
  // Helper to check if number is within 32-bit range
  function is32BitSafe(num) {
    return Number.isInteger(num) && num >= -2147483648 && num <= 2147483647;
  }
  
  // 1. Check for multiple binary fields
  function checkMultipleBinaryFields(obj, path = '') {
    const binaryFields = [];
    for (const [key, value] of Object.entries(obj)) {
      if (isBuffer(value)) {
        binaryFields.push(key);
      }
    }
    
    // Only body/data combination is allowed for multiple binary fields
    if (binaryFields.length > 1) {
      // Check if it's exactly body and data (the only allowed combination)
      const isAllowed = binaryFields.length === 2 && 
                        binaryFields.includes('body') && 
                        binaryFields.includes('data');
      
      if (!isAllowed) {
        errors.push(`Multiple binary fields at ${path || 'root'}: ${binaryFields.join(', ')}`);
      }
    }
  }
  
  // 2. Check for case conflicts
  function checkCaseConflicts(obj, path = '') {
    const keys = Object.keys(obj);
    const lowerKeys = keys.map(k => k.toLowerCase());
    const conflicts = {};
    
    lowerKeys.forEach((lowerKey, idx) => {
      if (!conflicts[lowerKey]) {
        conflicts[lowerKey] = [];
      }
      conflicts[lowerKey].push(keys[idx]);
    });
    
    for (const [lowerKey, originalKeys] of Object.entries(conflicts)) {
      if (originalKeys.length > 1) {
        errors.push(`Case conflict at ${path || 'root'}: ${originalKeys.join(', ')}`);
      }
    }
  }
  
  // 3. Check field names
  function checkFieldNames(obj, path = '') {
    for (const key of Object.keys(obj)) {
      // Check for empty or whitespace keys
      if (key === '' || /^\s+$/.test(key)) {
        errors.push(`Empty or whitespace-only field name at ${path || 'root'}: "${key}"`);
      }
      
      // Check for numeric keys
      if (/^\d+$/.test(key)) {
        errors.push(`Numeric field name at ${path || 'root'}: "${key}"`);
      }
      
      // Check for special characters
      if (/[!?#@$%^&*()]/.test(key)) {
        errors.push(`Special characters in field name at ${path || 'root'}: "${key}"`);
      }
    }
  }
  
  // 4. Check for empty buffer as data field
  function checkEmptyDataBuffer(obj, path = '') {
    if (obj.data && isBuffer(obj.data)) {
      const buffer = Buffer.isBuffer(obj.data) ? obj.data : Buffer.from(obj.data.data);
      if (buffer.length === 0) {
        errors.push(`Empty buffer as data field at ${path || 'root'}`);
      }
    }
  }
  
  // 5. Deep inspection for all patterns
  function inspect(value, path = '') {
    // Avoid circular references
    if (typeof value === 'object' && value !== null) {
      if (visited.has(value)) return;
      visited.add(value);
    }
    
    // Check numbers
    if (typeof value === 'number') {
      if (!is32BitSafe(value)) {
        errors.push(`Large number at ${path}: ${value}`);
      }
    }
    
    // Check arrays
    if (Array.isArray(value)) {
      value.forEach((item, idx) => {
        const itemPath = `${path}[${idx}]`;
        
        // Check for buffers in arrays
        if (isBuffer(item)) {
          errors.push(`Buffer inside array at ${itemPath}`);
        }
        
        // Recursive check
        inspect(item, itemPath);
      });
    }
    
    // Check objects
    if (value && typeof value === 'object' && !Array.isArray(value) && !isBuffer(value)) {
      // Run object-level checks
      checkMultipleBinaryFields(value, path);
      checkCaseConflicts(value, path);
      checkFieldNames(value, path);
      checkEmptyDataBuffer(value, path);
      
      // Recursive check for nested values
      for (const [key, val] of Object.entries(value)) {
        const keyPath = path ? `${path}.${key}` : key;
        inspect(val, keyPath);
      }
    }
  }
  
  // Start inspection
  inspect(obj);
  
  return {
    isValid: errors.length === 0,
    errors: errors
  };
}

// Export for use
if (typeof module !== 'undefined' && module.exports) {
  module.exports = { detectFailingCase };
}
```

## Usage Example

```javascript
const { detectFailingCase } = require('./detector');

// Test a potentially failing case
const testCase = {
  max_int64: 9223372036854775807,
  data: Buffer.from([]),
  Data: "conflict",
  binary: Buffer.from([1, 2, 3]),
  binary2: Buffer.from([4, 5, 6]),
  "field!": "special",
  "": "empty",
  items: [Buffer.from([1]), "text"],
  nested: {
    large: 1000000000000000,
    123: "numeric key"
  }
};

const result = detectFailingCase(testCase);
console.log('Is valid:', result.isValid);
console.log('Errors:', result.errors);
```

## Summary of Constraints

1. **Numbers**: Must be within 32-bit signed integer range (-2,147,483,648 to 2,147,483,647)
2. **Binary fields**: Maximum one binary field UNLESS it's specifically `body` and `data` fields together (the only allowed combination of multiple binary fields)
3. **Arrays**: Cannot contain Buffer objects
4. **Field names**: 
   - No case variations of the same name
   - No special characters (!?#@$%^&*())
   - No pure numeric names
   - No empty or whitespace-only names
5. **Special fields**: `data` field cannot be an empty buffer
6. **Nesting**: All constraints apply recursively to nested structures

Note: Only `body` and `data` are special field names with unique handling in the encoder. Other field names like `bin`, `binary`, `file` etc. are treated as regular fields.

## Recommendations

1. Always validate objects with the detector before attempting to encode
2. Use descriptive, lowercase field names without special characters
3. Keep numbers within 32-bit range or use string representation for larger values
4. Avoid putting binary data in arrays - use separate fields instead
5. Be consistent with field name casing throughout the object structure
