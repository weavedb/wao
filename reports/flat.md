# Flat Codec Specification

## Overview

The Flat codec (`dev_codec_flat`) transforms nested data structures into flat key-value pairs using path-based keys. It serves as an intermediate representation that preserves hierarchy while enabling efficient processing.

## Core Concepts

### Path Representation

Nested structures are flattened using `/` as a path separator:

```
Nested: { "user": { "profile": { "name": "John" } } }
Flat:   { "user/profile/name": "John" }
```

### Value Types

Only leaf values (non-objects) are preserved in the flat representation:
- Strings, numbers, booleans, null → Preserved as-is
- Objects and arrays → Decomposed into paths

## Encoding Rules

### 1. Simple Object Flattening

Each nested level adds a path segment:

```
Input:
{
  "user": {
    "name": "John",
    "age": 30
  }
}

Output:
{
  "user/name": "John",
  "user/age": 30
}
```

### 2. Deep Nesting

Arbitrary nesting depth is supported:

```
Input:
{
  "a": {
    "b": {
      "c": {
        "d": "value"
      }
    }
  }
}

Output:
{
  "a/b/c/d": "value"
}
```

### 3. Array Handling

Arrays create numbered paths (1-based):

```
Input:
{
  "items": ["first", "second", "third"]
}

Output:
{
  "items/1": "first",
  "items/2": "second",
  "items/3": "third"
}
```

### 4. Mixed Structures

Objects containing arrays and nested objects:

```
Input:
{
  "data": {
    "users": [
      { "name": "Alice" },
      { "name": "Bob" }
    ],
    "count": 2
  }
}

Output:
{
  "data/users/1/name": "Alice",
  "data/users/2/name": "Bob",
  "data/count": 2
}
```

### 5. Empty Values

Empty objects and arrays are preserved:

```
Input:
{
  "config": {
    "values": [],
    "settings": {}
  }
}

Output:
{
  "config/values": [],
  "config/settings": {}
}
```

### 6. Special Path Characters

Paths with special characters (including `/`):

```
Input:
{
  "filesystem": {
    "/": {
      "home": "data"
    }
  }
}

Output:
{
  "filesystem///home": "data"
}
```

## Decoding Rules

### 1. Path Parsing

Split paths by `/` and reconstruct hierarchy:

```
Input:  { "a/b/c": "value" }
Output: { "a": { "b": { "c": "value" } } }
```

### 2. Array Reconstruction

Numeric path segments indicate array indices:

```
Input:
{
  "items/1": "first",
  "items/2": "second",
  "items/3": "third"
}

Output:
{
  "items": ["first", "second", "third"]
}
```

### 3. Sparse Arrays

Missing indices create sparse arrays:

```
Input:
{
  "data/1": "first",
  "data/3": "third"
}

Output:
{
  "data": {
    "1": "first",
    "3": "third"
  }
}
```

### 4. Path Collision Handling

Later values override earlier ones:

```
Input:
{
  "a/b": "value1",
  "a/b/c": "value2"  // This would cause a collision
}

Error: Path collision detected
```

## Special Cases

### 1. Root-Level Arrays

Arrays at root level use numeric keys:

```
Input:  ["a", "b", "c"]
Output: { "1": "a", "2": "b", "3": "c" }
```

### 2. Mixed Numeric/String Keys

Objects with both numeric and non-numeric keys:

```
Input:
{
  "data": {
    "1": "numeric",
    "a": "alpha"
  }
}

Output:
{
  "data/1": "numeric",
  "data/a": "alpha"
}
```

### 3. Consecutive Slashes

Multiple slashes are preserved:

```
Input:  { "a//b": { "c": "value" } }
Output: { "a//b/c": "value" }
```

## Integration with Other Codecs

The flat codec works as an intermediate step:

1. **Structured → Flat**: Complex types are first encoded with type information
2. **Flat → HTTPSig**: Flat paths determine multipart structure
3. **HTTPSig → Flat**: Multipart paths are reconstructed
4. **Flat → Structured**: Types are reapplied to values

## Complete Examples

### Example 1: User Profile
```
Input:
{
  "user": {
    "id": 123,
    "profile": {
      "name": "John Doe",
      "email": "john@example.com"
    },
    "settings": {
      "theme": "dark",
      "notifications": true
    }
  }
}

Output:
{
  "user/id": 123,
  "user/profile/name": "John Doe",
  "user/profile/email": "john@example.com",
  "user/settings/theme": "dark",
  "user/settings/notifications": true
}
```

### Example 2: E-commerce Order
```
Input:
{
  "order": {
    "id": "ORD-001",
    "items": [
      {
        "product": "Widget",
        "quantity": 2,
        "price": 9.99
      },
      {
        "product": "Gadget",
        "quantity": 1,
        "price": 19.99
      }
    ],
    "total": 39.97
  }
}

Output:
{
  "order/id": "ORD-001",
  "order/items/1/product": "Widget",
  "order/items/1/quantity": 2,
  "order/items/1/price": 9.99,
  "order/items/2/product": "Gadget",
  "order/items/2/quantity": 1,
  "order/items/2/price": 19.99,
  "order/total": 39.97
}
```

### Example 3: Configuration
```
Input:
{
  "config": {
    "database": {
      "host": "localhost",
      "port": 5432,
      "credentials": {
        "user": "admin",
        "pass": "secret"
      }
    },
    "cache": {
      "enabled": true,
      "ttl": 3600
    }
  }
}

Output:
{
  "config/database/host": "localhost",
  "config/database/port": 5432,
  "config/database/credentials/user": "admin",
  "config/database/credentials/pass": "secret",
  "config/cache/enabled": true,
  "config/cache/ttl": 3600
}
```

## Implementation Notes

1. **Path Building**: Use `/` as separator, no leading or trailing slashes
2. **Array Indices**: Always 1-based for compatibility with Erlang
3. **Type Preservation**: Values maintain their types through flattening
4. **Order Preservation**: Original key order should be maintained
5. **Collision Detection**: Detect and error on path conflicts
