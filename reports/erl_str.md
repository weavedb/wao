# Erlang String Format API Documentation

This document describes the Erlang string format used for JavaScript-Erlang interoperability, including the structured field encoding for special types.

## Overview

The Erlang string format allows bidirectional conversion between JavaScript values and Erlang terms. It uses structured field encoding to preserve type information for special values like binaries and atoms.

## Structured Field Encoding

### Binary/Buffer Format: `:base64:`
- JavaScript `Buffer` or `Uint8Array` values are encoded as `:base64_content:`
- Empty buffers are encoded as `::`
- Example: `Buffer.from("hello")` → `":aGVsbG8=:"`

### Atom/Symbol Format: `%atom%`
- JavaScript `Symbol` values are encoded as `%atom_name%`
- Special symbols that convert to primitives:
  - `Symbol('null')` → `null`
  - `Symbol('true')` → `true`
  - `Symbol('false')` → `false`
- Example: `Symbol.for("ok")` → `"%ok%"`

## JavaScript API

### `erl_str_from(str: string): any`

Parses an Erlang term string into JavaScript values.

**Supported Erlang Terms:**
- **Atoms**: `atom`, `'quoted atom'` → `Symbol.for("atom")`
- **Numbers**: `42`, `3.14`, `1.23e10` → `42`, `3.14`, `1.23e10`
- **Binaries**: `<<"hello">>`, `<<1,2,3>>` → `Buffer`
- **Lists**: `[1, 2, 3]` → `[1, 2, 3]`
- **Maps**: `#{key => value}` → `{key: value}`
- **Special atoms**: `null`, `true`, `false` → `null`, `true`, `false`

**Examples:**
```javascript
import { erl_str_from } from './erl_str.js'

// Parse atoms
erl_str_from('ok')                    // Symbol.for('ok')
erl_str_from("'with spaces'")         // Symbol.for('with spaces')
erl_str_from('null')                  // null

// Parse numbers  
erl_str_from('42')                    // 42
erl_str_from('3.14')                  // 3.14

// Parse binaries
erl_str_from('<<"hello">>')           // Buffer.from('hello')
erl_str_from('<<1,2,3>>')            // Buffer.from([1,2,3])
erl_str_from('<<":aGVsbG8=:">>')     // Buffer.from('hello') via base64

// Parse collections
erl_str_from('[1, 2, 3]')            // [1, 2, 3]
erl_str_from('#{a => 1, b => 2}')    // {a: 1, b: 2}

// Parse complex structures
erl_str_from('#{<<"user">> => #{<<"name">> => <<"Alice">>, <<"age">> => 30}}')
// { user: { name: Buffer.from('Alice'), age: 30 } }
```

### `erl_str_to(value: any): string`

Converts JavaScript values to Erlang term string representation.

**Type Mappings:**
- `null` → `null`
- `undefined` → `undefined` 
- `boolean` → `true` / `false`
- `number` → number literal
- `string` → `'quoted atom'`
- `Symbol` → atom (quoted if needed)
- `Buffer` → `<<"binary">>` or `<<byte,list>>`
- `Array` → `[...]`
- `Object` → `#{...}`

**Examples:**
```javascript
import { erl_str_to } from './erl_str.js'

// Convert primitives
erl_str_to(null)                      // 'null'
erl_str_to(true)                      // 'true'
erl_str_to(42)                        // '42'
erl_str_to(3.14)                      // '3.14'

// Convert strings and symbols
erl_str_to('hello')                   // "'hello'"
erl_str_to(Symbol.for('ok'))          // 'ok'
erl_str_to(Symbol.for('with spaces')) // "'with spaces'"

// Convert buffers
erl_str_to(Buffer.from('hello'))      // '<<"hello">>'
erl_str_to(Buffer.from([1,2,3]))      // '<<1,2,3>>'

// Convert collections
erl_str_to([1, 2, 3])                 // '[1,2,3]'
erl_str_to({a: 1, b: 2})              // '#{<<"a">> => 1,<<"b">> => 2}'
```

### `isValidForRoundTrip(value: any): { valid: boolean, reason?: string }`

Validates if a JavaScript value can successfully round-trip through the Erlang string format.

**Returns:**
- `{ valid: true }` if the value can round-trip
- `{ valid: false, reason: string }` if the value will fail

**Examples:**
```javascript
import { isValidForRoundTrip } from './erl_str.js'

// Valid values
isValidForRoundTrip(42)                        // { valid: true }
isValidForRoundTrip(Symbol.for('ok'))          // { valid: true }
isValidForRoundTrip([1, 2, 3])                // { valid: true }

// Invalid values
isValidForRoundTrip(Infinity)                  // { valid: false, reason: 'Infinity is not supported' }
isValidForRoundTrip(Symbol('local'))           // { valid: false, reason: 'Non-global symbols cannot round-trip' }
isValidForRoundTrip([undefined])               // { valid: false, reason: 'undefined in arrays becomes null' }
isValidForRoundTrip('%%')                      // { valid: false, reason: 'String pattern conflicts with structured field' }
```

## Erlang API

### `dev_encode:to_erl(Msg) -> Term`

Converts JSON data with structured fields to Erlang terms.

**Parameters:**
- `Msg`: Map containing `<<"body">>` key with JSON binary

**Returns:**
- Erlang term with decoded structured fields

**Process:**
1. Extracts JSON from message body
2. Parses JSON to Erlang terms
3. Decodes structured fields:
   - `:base64:` → binary
   - `%atom%` → atom
   - Handles `$empty` annotations

### `dev_encode:to_str(Term) -> Binary`

Converts Erlang terms to UTF-8 safe string representation.

**Parameters:**
- `Term`: Any Erlang term

**Returns:**
- Binary string representation

**Features:**
- Binaries always formatted as `<<byte,list>>`
- UTF-8 safe (avoids encoding issues)
- Preserves all Erlang types

### `dev_encode:json_to_erl(Msg1, Msg2, Opts) -> {ok, Binary}`

Convenience function that combines `to_erl/1` and `to_str/1`.

**Examples:**
```erlang
% Convert JSON with structured fields
Msg = #{<<"body">> => <<"{\"status\": \"%ok%\", \"data\": \":aGVsbG8=:\"}">>},
Term = dev_encode:to_erl(Msg),
% Returns: #{<<"status">> => ok, <<"data">> => <<"hello">>}

% Convert term to string
Str = dev_encode:to_str(#{status => ok, count => 42}),
% Returns: <<"#{status => ok,count => 42}">>

% Combined operation
{ok, Result} = dev_encode:json_to_erl(Msg, undefined, []),
% Returns: {ok, <<"#{<<\"status\">> => ok,<<\"data\">> => <<104,101,108,108,111>>}">>}
```

## Special Cases and Edge Cases

### Symbol Handling
- Non-global symbols cannot round-trip through JSON
- All symbols (except special ones) become global symbols after round-trip
- Use `Symbol.for()` for symbols that need to round-trip

### Undefined Handling
- `undefined` is omitted from objects during JSON serialization
- `undefined` in arrays becomes `null` in JSON
- `Symbol('undefined')` is preserved as a symbol

### Number Limitations
- `Infinity`, `-Infinity`, and `NaN` are not supported (not valid JSON)
- Large numbers may lose precision due to JavaScript number limitations

### String Handling
- Empty strings become empty binaries
- All strings are converted to UTF-8 buffers in Erlang
- Strings in objects become binary keys

### Problematic Patterns
- Avoid `%%` in strings (conflicts with empty atom marker)
- Atoms with special characters may need quoting
- Multi-line atoms are supported but require careful parsing

## Values That Cannot Round-Trip

The following values will fail or change during round-trip:

### 1. Special Number Values
```javascript
// These are not valid JSON
Infinity      // → null in JSON
-Infinity     // → null in JSON  
NaN           // → null in JSON
```

### 2. Non-Global Symbols
```javascript
Symbol('local')           // → Symbol.for('local') after round-trip
Symbol('')                // Empty description symbols fail
Symbol(' ')               // Space-only description fails
// Use Symbol.for() instead for symbols that need to round-trip
```

### 3. Undefined in Arrays
```javascript
[1, undefined, 3]         // → [1, null, 3] in JSON
// undefined is preserved in objects (omitted during serialization)
```

### 4. Conflicting String Patterns
```javascript
// These patterns conflict with structured field encoding:
"%%"                      // Interpreted as empty atom marker
"%token%"                 // Interpreted as atom 'token'
":base64:"                // Would be interpreted as base64 binary
"::"                      // Interpreted as empty binary

// Any string matching these patterns:
/^%[^%]*%$/               // Atom pattern: %...%
/^:[^:]*:$/               // Binary pattern: :...:
```

### 5. Functions and Special Objects
```javascript
function() {}             // Cannot be serialized to JSON
new Date()                // Becomes string in JSON
/regex/                   // Becomes empty object {} in JSON
new Map()                 // Becomes empty object {} in JSON
new Set()                 // Becomes empty object {} in JSON
```

### 6. Circular References
```javascript
const obj = { a: 1 };
obj.self = obj;           // Throws during JSON serialization
```

### 7. Very Large Numbers
```javascript
// Numbers beyond safe integer range may lose precision
Number.MAX_SAFE_INTEGER + 1  // May lose precision
9007199254740993             // May round incorrectly
```

## Round-Trip Guarantees

The following values are guaranteed to round-trip correctly:
- All finite numbers within safe integer range
- Booleans and null
- Global symbols (`Symbol.for(...)`)
- Special symbols that convert to primitives
- Buffers and typed arrays
- Arrays (without undefined values)
- Objects (without undefined values)
- Nested structures of the above
- Strings that don't match structured field patterns

## Error Handling

Both parsers throw descriptive errors for invalid input:
- Unexpected characters with position and context
- Invalid structured field format
- Malformed Erlang terms

Example error:
```
Error: Unexpected character at position 42: 'X' (char code: 88)
Context: "=> value,[X]"
```

## Best Practices

1. **Use Global Symbols**: Always use `Symbol.for()` instead of `Symbol()` for values that need to round-trip
2. **Avoid Special Patterns**: Don't use strings that match `%...%` or `:...:` patterns
3. **Handle Undefined**: Remove undefined from arrays before serialization
4. **Validate Before Sending**: Use `isValidForRoundTrip()` to check values before processing
5. **Escape Special Strings**: In production, implement escaping for structured field patterns