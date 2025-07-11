# Flat Codec Specification

## Overview

The Flat codec (`dev_codec_flat`) transforms nested map structures into flat key-value pairs using path-based keys. It serves as a simple intermediate representation that preserves hierarchy while enabling efficient processing.

## Core Concepts

### Path Representation

Nested structures are flattened using `/` as a path separator:

```
Nested: #{ <<"user">> => #{ <<"name">> => <<"John">> } }
Flat:   #{ <<"user/name">> => <<"John">> }
```

### Supported Value Types

The flat codec **only** supports:
- **Binary strings** as leaf values
- **Maps** as containers (which get flattened)

**Not supported:**
- Numbers
- Booleans
- Lists/Arrays (as values)
- Atoms (except as map keys)
- Other Erlang terms

### Map Key Types

Erlang maps allow any term as a key, including complex types that cannot be represented in JSON:
- **Binary strings** (most common): `<<"key">>`
- **Lists of binaries**: `[<<"part1">>, <<"part2">>]`
- **Other Erlang terms** that `hb_path:to_binary/1` can convert to a path

Note: This flexibility is unique to Erlang and cannot be represented in JSON or most other data formats.

## Encoding Rules (`to/1`)

### 1. Binary Passthrough

Binary values at the top level pass through unchanged:

```erlang
Input:  <<"raw binary">>
Output: <<"raw binary">>

% In code:
dev_codec_flat:to(<<"raw binary">>) % Returns: <<"raw binary">>
```

### 2. Simple Map Flattening

Each nested level adds a path segment:

```erlang
Input:
#{ <<"user">> => #{ <<"name">> => <<"John">> } }

Output:
#{ <<"user/name">> => <<"John">> }
```

### 3. Multiple Keys at Same Level

Maps with multiple keys at the same level:

```erlang
Input:
#{
  <<"x">> => #{
    <<"y">> => <<"1">>,
    <<"z">> => <<"2">>
  },
  <<"a">> => <<"3">>
}

Output:
#{
  <<"x/y">> => <<"1">>,
  <<"x/z">> => <<"2">>,
  <<"a">> => <<"3">>
}
```

### 4. Deep Nesting

Arbitrary nesting depth is supported:

```erlang
Input:
#{ <<"a">> => #{ <<"b">> => #{ <<"c">> => #{ <<"d">> => <<"deep">> } } } }

Output:
#{ <<"a/b/c/d">> => <<"deep">> }
```

## Decoding Rules (`from/1`)

### 1. Binary Input Deserialization

When input is binary, it's deserialized first (expecting key: value format with newlines):

```erlang
Input:  <<"key: value\n">>
Output: #{ <<"key">> => <<"value">> }

% Example with multiple entries:
Input:  <<"user/name: John\nuser/age: 30\n">>
Output: #{ <<"user">> => #{ <<"name">> => <<"John">>, <<"age">> => <<"30">> } }
```

### 2. Path Parsing

Split paths by `/` and reconstruct hierarchy:

```erlang
Input:  #{ <<"a/b/c">> => <<"value">> }
Output: #{ <<"a">> => #{ <<"b">> => #{ <<"c">> => <<"value">> } } }
```

### 3. Path Collision Handling

If a path collision occurs (trying to set a value where a map already exists), an error is thrown:

```erlang
% This will throw {path_collision, {key, Key}, {existing, OldValue}, {value, NewValue}}
% Example: Trying to flatten this would cause an error
#{
  <<"a">> => <<"value1">>,        % "a" is a value
  <<"a">> => #{                   % "a" is also a map - collision!
    <<"b">> => <<"value2">>
  }
}
```

### 4. Map Merging

When two paths lead to the same map key and both values are maps, they are merged:

```erlang
Input:
#{
  <<"a/b">> => <<"1">>,
  <<"a/c">> => <<"2">>
}

Output:
#{ <<"a">> => #{ <<"b">> => <<"1">>, <<"c">> => <<"2">> } }
```

## Serialization Format

The codec includes `serialize/1` and `deserialize/1` functions for text representation:

### Serialize Format
```
path/to/key: value
another/path: another value
```

- Each line contains one key-value pair
- Format: `<path>: <value>`
- Paths use `/` as separator
- Values are binary strings

### Example Serialization
```erlang
Input Map:
#{ <<"user/name">> => <<"John">>, <<"user/age">> => <<"30">> }

Serialized Output:
user/name: John
user/age: 30
```

## Integration with Other Codecs

The flat codec delegates signature-related functions to `dev_codec_httpsig`:
- `commit/3`
- `verify/3`
- `committed/3`

## Limitations

1. **Values must be binaries**: No support for numbers, atoms, or other Erlang terms as values
2. **No array support**: Lists cannot be flattened or reconstructed
3. **Simple format**: This is a basic flattening mechanism, not a full data transformation codec
4. **Path character restrictions**: The implementation doesn't handle special cases like paths containing the separator character

## Complete Examples

### Example 1: User Profile
```erlang
Input:
#{
  <<"user">> => #{
    <<"profile">> => #{
      <<"name">> => <<"John Doe">>,
      <<"email">> => <<"john@example.com">>
    },
    <<"settings">> => #{
      <<"theme">> => <<"dark">>,
      <<"notifications">> => <<"true">>  % Note: must be string, not boolean
    }
  }
}

Output:
#{
  <<"user/profile/name">> => <<"John Doe">>,
  <<"user/profile/email">> => <<"john@example.com">>,
  <<"user/settings/theme">> => <<"dark">>,
  <<"user/settings/notifications">> => <<"true">>
}
```

### Example 2: Configuration
```erlang
Input:
#{
  <<"config">> => #{
    <<"database">> => #{
      <<"host">> => <<"localhost">>,
      <<"port">> => <<"5432">>,  % Note: must be string
      <<"credentials">> => #{
        <<"user">> => <<"admin">>,
        <<"pass">> => <<"secret">>
      }
    }
  }
}

Output:
#{
  <<"config/database/host">> => <<"localhost">>,
  <<"config/database/port">> => <<"5432">>,
  <<"config/database/credentials/user">> => <<"admin">>,
  <<"config/database/credentials/pass">> => <<"secret">>
}
```

### Example 3: Complex Keys (Erlang-specific)
```erlang
% This example uses list keys, which is valid in Erlang but not representable in JSON
Input:
#{
  <<"x">> => #{
    [<<"y">>, <<"z">>] => #{
      <<"a">> => <<"2">>
    },
    <<"a">> => <<"2">>
  }
}

Output:
#{
  <<"x/y/z/a">> => <<"2">>,  % The list key [<<"y">>, <<"z">>] becomes part of the path
  <<"x/a">> => <<"2">>
}
```

## Implementation Notes

1. **Binary strings only**: All values must be binaries (Erlang binaries using `<<>>` syntax)
2. **Map keys**: Keys in maps are typically binaries but can be other Erlang terms (lists, atoms, etc.)
3. **Path building**: Uses `hb_path:to_binary/1` for path construction and `hb_path:term_to_path_parts/1` for parsing
4. **Error handling**: Throws exceptions on path collisions rather than overwriting
5. **No type conversion**: The codec does not convert between types - binaries in, binaries out
6. **Message conversion**: Uses `hb_message:convert/3` or `hb_message:convert/4` for format transformations
