// Test cases for httpsig_from (HTTP to TABM)
export const cases_from = [
  // 1. Simple message with body
  {
    "content-type": "text/plain",
    body: "Hello World",
  },

  // 2. Message with headers only
  {
    "x-custom-header": "value",
    "content-length": "0",
  },

  // 3. Message with inline body key
  {
    "inline-body-key": "data",
    body: "Some data content",
  },

  // 4. Multipart message with single part
  {
    "content-type": 'multipart/form-data; boundary="simple-boundary"',
    body: `--simple-boundary\r\ncontent-disposition: form-data;name="field1"\r\n\r\nvalue1\r\n--simple-boundary--`,
  },

  // 5. Multipart with inline part
  {
    "content-type": 'multipart/form-data; boundary="inline-boundary"',
    body: `--inline-boundary\r\ncontent-disposition: inline\r\n\r\nInline content\r\n--inline-boundary--`,
  },

  // 6. Multipart with nested structure
  {
    "content-type": 'multipart/form-data; boundary="nested-boundary"',
    body: `--nested-boundary\r\ncontent-disposition: form-data;name="user/name"\r\n\r\nJohn\r\n--nested-boundary\r\ncontent-disposition: form-data;name="user/email"\r\n\r\njohn@example.com\r\n--nested-boundary--`,
  },

  // 7. Message with grouped IDs
  {
    "ao-ids":
      'id1XYZ01234567890123456789012345678901234567="value1", id2ABC01234567890123456789012345678901234567="value2"',
    "regular-header": "value",
  },

  // 8. Multipart with headers in parts
  {
    "content-type": 'multipart/form-data; boundary="header-boundary"',
    body: `--header-boundary\r\ncontent-disposition: form-data;name="data"\r\ncontent-type: application/json\r\n\r\n{"key": "value"}\r\n--header-boundary--`,
  },

  // 9. Empty body message
  {
    "content-type": "text/plain",
    "x-empty": "true",
  },

  // 10. Multipart with body keys
  {
    "content-type": 'multipart/form-data; boundary="keys-boundary"',
    "body-keys": "user, config",
    body: `--keys-boundary\r\ncontent-disposition: form-data;name="user"\r\n\r\nJohn Doe\r\n--keys-boundary\r\ncontent-disposition: form-data;name="config"\r\n\r\nsettings\r\n--keys-boundary--`,
  },

  // 11. Message with special characters
  {
    "x-special": 'Hello "World"',
    body: "Line1\nLine2\r\nLine3",
  },

  // 12. Deep nested multipart
  {
    "content-type": 'multipart/form-data; boundary="deep-boundary"',
    body: `--deep-boundary\r\ncontent-disposition: form-data;name="a/b/c"\r\n\r\ndeep value\r\n--deep-boundary--`,
  },

  // 13. Mixed case headers (should normalize)
  {
    "Content-Type": "text/html",
    "X-Custom-Header": "value",
    body: "<html></html>",
  },

  // 14. Multipart with empty parts
  {
    "content-type": 'multipart/form-data; boundary="empty-boundary"',
    body: `--empty-boundary\r\ncontent-disposition: form-data;name="empty"\r\n\r\n\r\n--empty-boundary\r\ncontent-disposition: form-data;name="not-empty"\r\n\r\nvalue\r\n--empty-boundary--`,
  },

  // 15. Unicode content
  {
    "content-type": "text/plain; charset=utf-8",
    body: "Hello 世界 🎉",
  },

  // 16. Multipart with sub-messages
  {
    "content-type": 'multipart/form-data; boundary="sub-boundary"',
    body: `--sub-boundary\r\ncontent-disposition: form-data;name="message"\r\ncontent-type: text/plain\r\nx-meta: data\r\n\r\nMessage body\r\n--sub-boundary--`,
  },

  // 17. Complex headers structure
  {
    "x-array": "[1,2,3]",
    "x-object": '{"key":"value"}',
    "x-boolean": "true",
    body: "content",
  },

  // 18. Multipart with various dispositions
  {
    "content-type": 'multipart/form-data; boundary="disp-boundary"',
    body: `--disp-boundary\r\ncontent-disposition: inline\r\n\r\nInline data\r\n--disp-boundary\r\ncontent-disposition: form-data;name="attachment"\r\n\r\nAttachment data\r\n--disp-boundary--`,
  },

  // 19. Long header value (near threshold)
  {
    "x-long": "a".repeat(4000),
    "x-short": "short value",
  },

  // 20. Complete multipart form
  {
    "content-type": 'multipart/form-data; boundary="form-boundary"',
    body: `--form-boundary\r\ncontent-disposition: form-data;name="user/profile/name"\r\n\r\nAlice\r\n--form-boundary\r\ncontent-disposition: form-data;name="user/profile/age"\r\n\r\n30\r\n--form-boundary\r\ncontent-disposition: form-data;name="user/settings/theme"\r\n\r\ndark\r\n--form-boundary\r\ncontent-disposition: form-data;name="metadata"\r\ncontent-type: application/json\r\n\r\n{"created": "2024-01-01"}\r\n--form-boundary--`,
  },
]

// Test cases for httpsig_to (TABM to HTTP)
export const cases_to = [
  // 1. Simple message with body
  {
    message: "Hello World",
    body: "Main content",
  },

  // 2. Headers only message
  {
    "x-custom-header": "value",
    "content-length": "0",
  },

  // 3. Message with data as inline key
  {
    data: "Some data content",
    "x-header": "value",
  },

  // 4. Nested structure (should become multipart)
  {
    user: {
      name: "John",
      email: "john@example.com",
    },
    config: "settings",
  },

  // 5. Message with IDs to group
  {
    id1XYZ01234567890123456789012345678901234567: "value1",
    id2ABC01234567890123456789012345678901234567: "value2",
    "regular-key": "value",
  },

  // 6. Deep nested structure
  {
    a: {
      b: {
        c: "deep value",
      },
    },
  },

  // 7. Mixed content types
  {
    text: "plain text",
    data: {
      json: '{"key": "value"}',
      number: "123",
    },
  },

  // 8. Long values (exceeding header limit)
  {
    short: "short value",
    long: "x".repeat(5000),
  },

  // 9. Empty values
  {
    empty: "",
    nonempty: "value",
    nested: {
      empty: "",
    },
  },

  // 10. Special characters
  {
    special: 'Hello "World"',
    multiline: "Line1\nLine2",
    unicode: "Hello 世界",
  },

  // 11. Array-like structure
  {
    items: {
      0: "first",
      1: "second",
      2: "third",
    },
  },

  // 12. Complex nested with body
  {
    metadata: {
      version: "1.0",
      created: "2024-01-01",
    },
    body: "Main document content",
    config: {
      enabled: "true",
    },
  },

  // 13. Keys requiring normalization
  {
    Content_Type: "text/html",
    X_Custom_Header: "value",
    normal_key: "value",
  },

  // 14. Structure with inline-body-key
  {
    "inline-body-key": "content",
    content: "This is the main content",
    meta: "data",
  },

  // 15. Unicode in keys and values
  {
    greeting: "Hello 世界",
    "emoji_🎉": "celebration",
  },

  // 16. Deeply nested with mixed types
  {
    app: {
      name: "MyApp",
      config: {
        port: "3000",
        features: {
          auth: "enabled",
          cache: "disabled",
        },
      },
    },
  },

  // 17. Boolean and numeric strings
  {
    enabled: "true",
    count: "42",
    rate: "3.14",
  },

  // 18. Single body value (should not create multipart)
  {
    body: "Just a simple body content",
  },

  // 19. Complex form-like structure
  {
    form: {
      user: {
        name: "Alice",
        email: "alice@example.com",
      },
      preferences: {
        theme: "dark",
        notifications: "on",
      },
    },
  },

  // 20. Large nested structure
  {
    users: {
      alice: {
        profile: {
          name: "Alice Smith",
          age: "30",
        },
        settings: {
          theme: "dark",
        },
      },
      bob: {
        profile: {
          name: "Bob Jones",
          age: "25",
        },
        settings: {
          theme: "light",
        },
      },
    },
    system: {
      version: "2.0",
      status: "active",
    },
  },
]
