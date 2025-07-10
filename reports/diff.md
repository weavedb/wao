# Erlang HyperBEAM Codec: Internal vs External Encoding Differences

## Executive Summary

The HyperBEAM codec has different behavior when encoding data internally (Erlang to Erlang) versus externally (JavaScript/JSON to Erlang). The key issue is that the external encoding path cannot handle complex empty values (empty maps `{}` and empty lists `[]`) when they appear as direct items within a list. This document explains the patterns that work, those that don't, and provides solutions.

## The Problem

When encoding from JavaScript to Erlang through the HTTP multipart format, the `encode_value/1` function in `dev_codec_structured.erl` fails with a `badmatch` error when it encounters an empty map `{}` or empty list `[]` as a direct item in a list.

### Example that fails externally:
```javascript
{ empty: ["", [], {}, null] }
```

### Error:
```erlang
error:{badmatch,#{}}:[{dev_codec_structured,'-encode_value/1-fun-0-',1,
  [{file,"/home/basque/dev/wao/HyperBEAM/src/dev_codec_structured.erl"},
   {line,209}]}
```

## Root Cause Analysis

The `encode_value/1` function handles list encoding by prefixing each non-binary item with its type:

```erlang
encode_value(Values) when is_list(Values) ->
    EncodedValues = lists:map(
        fun(Bin) when is_binary(Bin) -> {item, {string, Bin}, []};
           (Item) ->
            {RawType, Encoded} = encode_value(Item),  % RECURSIVE CALL
            Type = hb_ao:normalize_key(RawType),
            {item, {string, <<"(ao-type-", Type/binary, ") ", Encoded/binary>>}, []}
        end,
        Values
    ),
    {<<"list">>, iolist_to_binary(hb_structured_fields:list(EncodedValues))};
```

The problem: When `Item` is an empty map `#{}` or empty list `[]`, the recursive call to `encode_value(Item)` has no matching clause, causing a `badmatch` error.

## Patterns That Work vs Don't Work

### ❌ Patterns that ONLY work internally (fail externally)

#### 1. Mixed empty and non-empty values in list body
```erlang
% This puts empty map {} in the list encoding - FAILS externally
"4: null\r\n"
"ao-types: 1=\"empty-binary\", 2=\"empty-list\", 3=\"empty-message\", 4=\"atom\"\r\n"
```

#### 2. Direct encoding of complex empty types
```erlang
% Trying to encode ["", [], {}, null] directly
"1: \"\"\r\n"
"2: []  % <-- This will fail\r\n"
"3: {}  % <-- This will fail\r\n"
"4: null\r\n"
```

### ✅ Patterns that work BOTH internally and externally

#### 1. All empty values declared in ao-types with empty body
```erlang
% Declare ALL positions in ao-types, leave body empty
"ao-types: 1=\"empty-binary\", 2=\"empty-list\", 3=\"empty-message\", 4=\"atom\"\r\n"
"content-disposition: form-data;name=\"nested/empty\"\r\n"
"\r\n"  % <-- EMPTY BODY
```

This works because:
- Empty values are created during TABM processing phase
- No `encode_value/1` is called since body is empty
- The structure `[<<>>, [], #{}, null]` is created from type declarations

#### 2. Separate multipart sections for empty containers
```erlang
% Parent declares structure
"ao-types: lists=\"list\", maps=\"list\"\r\n"
"content-disposition: form-data;name=\"complex-empty\"\r\n"
"\r\n"
% Child parts create empty values
"--abc\r\n"
"ao-types: 1=\"empty-list\"\r\n"
"content-disposition: form-data;name=\"complex-empty/lists\"\r\n"
"\r\n"
"--abc\r\n"
"ao-types: 1=\"empty-message\"\r\n"
"content-disposition: form-data;name=\"complex-empty/maps\"\r\n"
```

## Processing Flow Comparison

### Internal Path (Erlang → HTTP → Erlang)
```
Erlang Data Structure
    ↓
from/1 (handles empty values with type annotations)
    ↓
HTTP Multipart (with ao-types)
    ↓
to/1 (recreates from types)
    ↓
Original Erlang Structure
```

### External Path (JavaScript → HTTP → Erlang)
```
JavaScript/JSON
    ↓
HTTP Multipart
    ↓
to/1 (parses structure)
    ↓
encode_value/1 (FAILS on empty map/list in list)
    ❌
```

## Why Internal Works When External Doesn't

### Internal Path
1. Empty values already exist as Erlang terms (`[]`, `#{}`)
2. `from/1` function handles them with special type annotations
3. No encoding through `encode_value/1` needed

### External Path
1. JSON is parsed into intermediate format
2. `encode_value/1` is called to convert values
3. **FAILS**: No case to handle empty maps/lists in list context

## Solution Strategy

To make data structures work externally:

1. **Never put empty maps/lists as direct list values**
2. **Use ao-types declarations for ALL empty positions**
3. **Keep bodies empty when all values are empty**
4. **Use separate multipart sections for complex nested empties**

### Before (fails externally):
```erlang
show2(#{
    <<"body">> =>
        <<"--abc\r\n"
          "ao-types: 1=\"empty-binary\", 2=\"empty-list\", 3=\"empty-message\", 4=\"atom\"\r\n"
          "content-disposition: form-data;name=\"nested/empty\"\r\n"
          "4: null\r\n"  % <-- Other positions ({}, []) encoded in body
          "--abc--">>
})
```

### After (works everywhere):
```erlang
show2(#{
    <<"body">> =>
        <<"--abc\r\n"
          "ao-types: 1=\"empty-binary\", 2=\"empty-list\", 3=\"empty-message\", 4=\"atom\"\r\n"
          "content-disposition: form-data;name=\"nested/empty\"\r\n"
          "\r\n"  % <-- Empty body, all created from types
          "--abc--">>
})
```

## Real-World Example

### JavaScript Input
```javascript
{
  nested: {
    empty: ["", [], {}, null],
    nums: [1, 2, 3]
  }
}
```

### Working HTTP Encoding
```erlang
<<"--abc\r\n"
  "ao-types: empty=\"list\", nums=\"list\"\r\n"
  "content-disposition: form-data;name=\"nested\"\r\n"
  "\r\n"
  "--abc\r\n"
  "ao-types: 1=\"empty-binary\", 2=\"empty-list\", 3=\"empty-message\", 4=\"atom\"\r\n"
  "content-disposition: form-data;name=\"nested/empty\"\r\n"
  "\r\n"  % <-- No body content!
  "--abc\r\n"
  "ao-types: 1=\"integer\", 2=\"integer\", 3=\"integer\"\r\n"
  "content-disposition: form-data;name=\"nested/nums\"\r\n"
  "1: 1\r\n"
  "2: 2\r\n"
  "3: 3\r\n"
  "--abc--">>
```

## Key Insights

1. **Type declarations are processed before encoding**: The `ao-types` processing happens during the TABM phase, before `encode_value/1` is called.

2. **Empty body trick**: By leaving the body empty and declaring all values in `ao-types`, we bypass the problematic encoding step entirely.

3. **Numbered positions matter**: Using numbered positions (1, 2, 3) in `ao-types` ensures proper list reconstruction.

4. **Separation of concerns**: Complex structures should be broken into separate multipart sections to avoid encoding conflicts.

## Conclusion

The HyperBEAM codec's external encoding limitation requires careful handling of empty complex types. By using type declarations instead of direct encoding, we can create data structures that work consistently across both internal and external paths. The key is understanding that `ao-types` processing provides a backdoor to create structures that would otherwise fail during the encoding phase.
