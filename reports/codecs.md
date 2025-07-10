# HyperBEAM Codec System API Documentation

## Overview

HyperBEAM uses a three-layer codec system to transform JavaScript objects into HTTP-compatible messages with cryptographic signatures. The system consists of:

1. **Structured Codec**: Type annotation and serialization (RFC-9651)
2. **Flat Codec**: Path-based decomposition of nested structures
3. **HTTPSig Codec**: HTTP multipart encoding with signatures (RFC-9421)

## Processing Pipeline

### Encoding (JS Object → HTTP Message)

```
JavaScript Object
    ↓
[Structured Codec]
- Add type annotations to ao-types
- Convert special types (boolean, null, arrays)
- Group case-sensitive IDs
    ↓
[Flat Codec]  
- Decompose nested objects into paths
- Convert arrays to numbered paths
    ↓
[HTTPSig Codec]
- Determine header vs body encoding
- Create multipart structure
- Generate content-digest
- Add signatures
    ↓
HTTP Message
```

### Decoding (HTTP Message → JS Object)

```
HTTP Message
    ↓
[HTTPSig Codec]
- Verify signatures
- Parse multipart body
- Extract headers
    ↓
[Flat Codec]
- Reconstruct paths to nested structure
- Convert numbered paths to arrays
    ↓
[Structured Codec]
- Apply type conversions from ao-types
- Restore empty values
- Ungroup IDs
    ↓
JavaScript Object
```

## Key Concepts

### 1. ao-types Header

The `ao-types` header is a structured field dictionary mapping field names to their types:

```
ao-types: count="integer", active="atom", data="list", config="empty-message"
```

**Purpose**: Preserves type information for non-string values during HTTP transport.

**Types**:
- `integer`: Whole numbers
- `float`: Decimal numbers (scientific notation)
- `atom`: Booleans and null
- `list`: Arrays
- `empty-binary`: Empty string
- `empty-list`: Empty array
- `empty-message`: Empty object

### 2. body-keys Header

Lists the names of multipart sections in order:

```
body-keys: "user", "user/profile", "items/1", "items/2"
```

**Purpose**: Maintains the order and structure of multipart body sections.

### 3. content-disposition Header

Identifies each multipart section:

```
content-disposition: form-data;name="user/profile"
content-disposition: inline  // For main body content
```

**Purpose**: Names each part and identifies the inline (main) content.

### 4. inline-body-key Header

Specifies which field represents the main body:

```
inline-body-key: "data"  // Makes 'data' field the inline content
```

**Default behavior**:
- If `body` field exists → `body` is inline
- Else if `data` field exists → `data` is inline  
- Otherwise → no inline content

### 5. content-digest Header

SHA-256 hash of the body content:

```
content-digest: sha-256=:base64-encoded-hash:
```

**Purpose**: Ensures body integrity for signature verification.

## Common Patterns

### Pattern 1: Simple Object

```javascript
// Input
{
  name: "John",
  age: 30,
  active: true
}

// Output Headers
ao-types: age="integer", active="atom"
name: John
age: 30
active: "true"
```

### Pattern 2: Nested Object

```javascript
// Input
{
  user: {
    name: "John",
    profile: {
      age: 30
    }
  }
}

// Output
content-type: multipart/form-data; boundary="..."
body-keys: "user/profile"

--...
content-disposition: form-data;name="user/profile"
ao-types: age="integer"
age: 30
name: John
--...--
```

### Pattern 3: Array of Objects

```javascript
// Input
{
  items: [
    { id: 1, name: "A" },
    { id: 2, name: "B" }
  ]
}

// Output
body-keys: "items/1", "items/2"

--...
content-disposition: form-data;name="items/1"
ao-types: id="integer"
id: 1
name: A
--...
content-disposition: form-data;name="items/2"
ao-types: id="integer"
id: 2
name: B
--...--
```

### Pattern 4: Mixed Types in Array

```javascript
// Input
{
  values: [1, "text", true, null]
}

// Output Headers
ao-types: values="list"
values: "(ao-type-integer) 1", "text", "(ao-type-atom) \"true\"", "(ao-type-atom) \"null\""
```

### Pattern 5: Empty Values

```javascript
// Input
{
  empty_string: "",
  empty_array: [],
  empty_object: {}
}

// Output Headers
ao-types: empty_string="empty-binary", empty_array="empty-list", empty_object="empty-message"
// Note: No actual values in output, only type declarations
```

### Pattern 6: Deeply Nested Structure

```javascript
// Input
{
  level1: {
    level2: {
      level3: {
        value: "deep"
      }
    }
  }
}

// Output
body-keys: "level1/level2/level3"

--...
content-disposition: form-data;name="level1/level2/level3"
value: deep
--...--
```

## Size Thresholds

- **Header Limit**: 4096 bytes per field
- **Decision Logic**:
  - Size ≤ 4096 bytes AND not an object → Header
  - Size > 4096 bytes OR is an object → Multipart body

## Special Cases

### 1. Numeric String Keys

Keys that look like numbers but are strings:

```javascript
{ "routes": { "1": "home", "2": "about" } }
// Treated as object keys, not array indices
```

### 2. Filesystem Paths

Multiple consecutive slashes:

```javascript
{ "fs": { "/": { "home": {} } } }
// Becomes: fs///home
```

### 3. Case-Sensitive IDs

43-character ID strings are grouped:

```javascript
{ "JFPzG7dx0L0daajL72KKfqZPInLcsVqwtO6U9pTQBK4": "value" }
// Becomes: ao-ids: JFPzG7dx0L0daajL72KKfqZPInLcsVqwtO6U9pTQBK4="value"
```

## Error Handling

1. **Path Collisions**: Attempting to set both a value and nested object at same path
2. **Type Mismatches**: Type in ao-types doesn't match actual value
3. **Invalid Types**: Unknown type specified in ao-types
4. **Boundary Conflicts**: Generated boundary appears in content

## Implementation Checklist

- [ ] Parse structured fields (RFC-9651)
- [ ] Handle scientific notation floats (20 decimal places)
- [ ] Escape quotes in nested strings
- [ ] Generate deterministic multipart boundaries
- [ ] Maintain 1-based array indexing
- [ ] Preserve key ordering
- [ ] Handle empty values correctly
- [ ] Support path-based flattening/unflattening
- [ ] Implement SHA-256 content digests
- [ ] Group case-sensitive ID fields
