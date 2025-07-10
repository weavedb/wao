# HTTP Signature Codec Specification

## Overview

The HTTP Signature codec (`dev_codec_httpsig`) implements HTTP Message Signatures (RFC-9421) for HyperBEAM messages. It handles the conversion between HyperBEAM's internal format and HTTP multipart messages with cryptographic signatures.

## Core Concepts

### Message Structure

Every HTTP message consists of:
1. **Headers**: Key-value pairs with size limitations
2. **Body**: Optional multipart content for complex data
3. **Signature**: Cryptographic attestation of message integrity

### Key Components

- **`content-type`**: Indicates multipart encoding with boundary
- **`content-digest`**: SHA-256 hash of body content
- **`body-keys`**: Ordered list of multipart section names
- **`signature-input`**: Structured field describing what was signed
- **`signature`**: The actual cryptographic signature
- **`inline-body-key`**: Specifies which field represents the main body

## Encoding Rules

### 1. Header vs Body Decision

Fields are encoded based on size and structure:

```
Size <= 4096 bytes AND not a map → Header
Size > 4096 bytes OR is a map → Body (multipart)
```

### 2. Simple Header Encoding

Small, simple values become headers:

```
Input:  { "name": "John", "age": 30 }
Output: name: John
        age: 30
        ao-types: age="integer"
```

### 3. Multipart Body Encoding

Complex or large values use multipart encoding:

```
Input:  { "data": { "value": 123 } }
Output: content-type: multipart/form-data; boundary="BOUNDARY"
        body-keys: "data"
        
        --BOUNDARY
        content-disposition: form-data;name="data"
        ao-types: value="integer"
        value: 123
        --BOUNDARY--
```

### 4. Boundary Generation

Boundary is SHA-256 hash of concatenated body parts (base64url encoded):

```
boundary = base64url(sha256(part1 + CRLF + part2 + ...))
```

### 5. Body Keys Encoding

Multipart section names as structured list:

```
Input:  Multiple body parts named "user", "profile", "settings"
Output: body-keys: "user", "profile", "settings"
```

### 6. Content Digest

SHA-256 hash of body content:

```
Input:  Body content "Hello"
Output: content-digest: sha-256=:MnbYoDj6ZU2DdCDnkzLI6LLwkPBQzPkF2IXXT6dV5MM=:
```

### 7. Nested Path Encoding

Nested objects create path-based multipart sections:

```
Input:  { "user": { "profile": { "name": "John" } } }
Output: body-keys: "user/profile"
        
        --BOUNDARY
        content-disposition: form-data;name="user/profile"
        name: John
        --BOUNDARY--
```

### 8. Array Encoding in Multipart

Arrays become numbered paths (1-based):

```
Input:  { "items": [{ "id": 1 }, { "id": 2 }] }
Output: body-keys: "items/1", "items/2"
        
        --BOUNDARY
        content-disposition: form-data;name="items/1"
        ao-types: id="integer"
        id: 1
        --BOUNDARY
        content-disposition: form-data;name="items/2"
        ao-types: id="integer"
        id: 2
        --BOUNDARY--
```

### 9. Inline Body Key

The main body content uses inline disposition:

```
Default inline key: "body" (if exists) or "data" (if exists)
Can be overridden with inline-body-key header

Example with "data" as inline:
--BOUNDARY
content-disposition: inline
This is the main content
--BOUNDARY--
```

### 10. Empty Values in Multipart

Empty values are represented only in ao-types:

```
Input:  { "config": { "values": [], "data": "" } }
Output: --BOUNDARY
        content-disposition: form-data;name="config"
        ao-types: values="empty-list", data="empty-binary"
        --BOUNDARY--
```

## Decoding Rules

### 1. Header Collection

1. Extract all headers except body
2. Parse structured fields (ao-types, body-keys)
3. Identify multipart boundary from content-type

### 2. Multipart Parsing

```
1. Split body by boundary
2. For each part:
   - Extract content-disposition
   - Parse headers into fields
   - Reconstruct nested structure from path
```

### 3. Path Reconstruction

```
"user/profile/name" → { "user": { "profile": { "name": value } } }
"items/1" → { "items": [value] }
```

### 4. Type Application

Apply types from ao-types to parsed values (see structured codec).

## Special Cases

### 1. Multiple Slashes in Paths

Filesystem paths with multiple slashes:

```
Input:  { "filesystem": { "/": { "home": { "user": data } } } }
Output: body-keys: "filesystem///home/user"
```

### 2. Numeric String Keys

Object keys that look like numbers:

```
Input:  { "routes": { "1": { "path": "/home" } } }
Output: body-keys: "routes/1"
        
        --BOUNDARY
        content-disposition: form-data;name="routes/1"
        path: /home
        --BOUNDARY--
```

### 3. Large Arrays

Arrays with many items each get their own multipart section:

```
Input:  { "data": [obj1, obj2, ..., obj100] }
Output: body-keys: "data/1", "data/2", ..., "data/100"
        (100 multipart sections)
```

## Complete Examples

### Example 1: Simple Message
```
Input:
{
  "message": "Hello",
  "count": 42
}

Output:
ao-types: count="integer"
message: Hello
count: 42
```

### Example 2: Nested Object
```
Input:
{
  "user": {
    "name": "John",
    "age": 30
  }
}

Output:
content-type: multipart/form-data; boundary="abc123"
body-keys: "user"

--abc123
content-disposition: form-data;name="user"
ao-types: age="integer"
name: John
age: 30
--abc123--
```

### Example 3: Complex Structure
```
Input:
{
  "company": {
    "employees": [
      { "id": 1, "name": "Alice" },
      { "id": 2, "name": "Bob" }
    ]
  }
}

Output:
content-type: multipart/form-data; boundary="xyz789"
body-keys: "company", "company/employees/1", "company/employees/2"

--xyz789
content-disposition: form-data;name="company"
ao-types: employees="list"
--xyz789
content-disposition: form-data;name="company/employees/1"
ao-types: id="integer"
id: 1
name: Alice
--xyz789
content-disposition: form-data;name="company/employees/2"
ao-types: id="integer"
id: 2
name: Bob
--xyz789--
```

## Implementation Notes

1. **CRLF**: Use `\r\n` for line endings in multipart
2. **Boundary**: Must not appear in content; use SHA-256 for uniqueness
3. **Header Order**: Headers are alphabetically sorted in each part
4. **Path Separator**: Always use `/` for nested paths
5. **Array Indexing**: Arrays use 1-based indexing in paths
6. **Size Threshold**: 4096 bytes determines header vs body encoding
