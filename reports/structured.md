# Structured Fields Codec Specification

## Overview

The Structured Fields codec (`dev_codec_structured`) implements HTTP Structured Fields (RFC-9651) for HyperBEAM's internal message format. It provides type-safe serialization and deserialization of complex data structures while maintaining compatibility with HTTP headers.

## Core Concepts

### Type System

The codec supports the following base types:

1. **Integer**: Whole numbers within -999,999,999,999,999 to 999,999,999,999,999
2. **Decimal**: Floating-point numbers with up to 3 decimal places
3. **String**: UTF-8 text wrapped in double quotes
4. **Token**: Unquoted identifiers (alphanumeric + `*_-./:%`)
5. **Binary**: Base64-encoded byte sequences wrapped in colons
6. **Boolean**: `?1` (true) or `?0` (false)
7. **List**: Ordered collections of items
8. **Dictionary**: Key-value pairs
9. **Inner List**: Nested lists with parameters

### Key Fields

- **`ao-types`**: A structured field dictionary that maps field names to their types
- **`ao-ids`**: Special dictionary for preserving case-sensitive ID fields
- **Empty values**: Represented by type indicators without values:
  - `empty-binary`: Empty string `""`
  - `empty-list`: Empty array `[]`
  - `empty-message`: Empty object `{}`

## Encoding Rules

### 1. Type Annotation

Every non-string value requires type annotation in the `ao-types` header:

```
Input:  { "count": 42, "name": "test" }
Output: ao-types: count="integer"
        count: 42
        name: test
```

### 2. Integer Encoding

Integers are encoded as bare numbers with validation:

```
Input:  { "value": 123 }
Output: ao-types: value="integer"
        value: 123

Input:  { "negative": -999 }
Output: ao-types: negative="integer"
        negative: -999
```

### 3. Float/Decimal Encoding

Floats are encoded in scientific notation with exactly 20 decimal places:

```
Input:  { "pi": 3.14159 }
Output: ao-types: pi="float"
        pi: 3.14158999999999988262e+00

Input:  { "small": 0.0000001 }
Output: ao-types: small="float"
        small: 1.00000000000000006228e-07
```

### 4. Boolean/Atom Encoding

Booleans and atoms are encoded as quoted strings:

```
Input:  { "active": true, "status": null }
Output: ao-types: active="atom", status="atom"
        active: "true"
        status: "null"
```

### 5. List Encoding

Lists use structured field list syntax with type prefixes for non-strings:

```
Input:  { "numbers": [1, 2, 3] }
Output: ao-types: numbers="list"
        numbers: "(ao-type-integer) 1", "(ao-type-integer) 2", "(ao-type-integer) 3"

Input:  { "mixed": [1, "text", true] }
Output: ao-types: mixed="list"
        mixed: "(ao-type-integer) 1", "text", "(ao-type-atom) \"true\""
```

### 6. Empty Value Encoding

Empty values are only represented in ao-types:

```
Input:  { "empty": "", "arr": [], "obj": {} }
Output: ao-types: empty="empty-binary", arr="empty-list", obj="empty-message"
```

### 7. Nested Structure Encoding

Complex nested structures are flattened with multipart encoding (see httpsig codec).

## Decoding Rules

### 1. Type Resolution

1. Check `ao-types` for field type
2. If no type specified, treat as string
3. Apply type-specific parsing

### 2. List Parsing

Parse structured field lists and extract type annotations:

```
Input:  "(ao-type-integer) 42", "text", "(ao-type-atom) \"true\""
Output: [42, "text", true]
```

### 3. Empty Value Reconstruction

```
Input:  ao-types: data="empty-list"
Output: { "data": [] }
```

## Special Handling

### Case-Sensitive IDs

ID fields (43-character strings matching specific pattern) are grouped into `ao-ids`:

```
Input:  { "JFPzG7dx0L0daajL72KKfqZPInLcsVqwtO6U9pTQBK4": "value" }
Output: ao-ids: JFPzG7dx0L0daajL72KKfqZPInLcsVqwtO6U9pTQBK4="value"
```

### Escape Sequences

Special characters in strings are escaped:
- `\` → `\\`
- `"` → `\"`

## Complete Examples

### Example 1: Simple Types
```
Input:
{
  "count": 42,
  "rate": 3.14,
  "active": true,
  "name": "test"
}

Output:
ao-types: count="integer", rate="float", active="atom"
count: 42
rate: 3.14000000000000012434e+00
active: "true"
name: test
```

### Example 2: Arrays
```
Input:
{
  "data": [1, 2, 3],
  "tags": ["a", "b", "c"]
}

Output:
ao-types: data="list", tags="list"
data: "(ao-type-integer) 1", "(ao-type-integer) 2", "(ao-type-integer) 3"
tags: "a", "b", "c"
```

### Example 3: Mixed Array
```
Input:
{
  "mixed": [1, "two", 3.0, true, null]
}

Output:
ao-types: mixed="list"
mixed: "(ao-type-integer) 1", "two", "(ao-type-integer) 3", "(ao-type-atom) \"true\"", "(ao-type-atom) \"null\""
```

## Implementation Notes

1. **Precision**: Float encoding must use exactly 20 decimal places in scientific notation
2. **Validation**: Integers must be within ±999,999,999,999,999
3. **String Handling**: All strings in lists must be quoted
4. **Type Prefixes**: Non-string list items use `(ao-type-X)` prefix where X is the type
5. **Escape Processing**: Double quotes in atoms within lists require triple escaping: `\\\"`
