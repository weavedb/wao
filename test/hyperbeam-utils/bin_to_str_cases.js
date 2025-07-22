export const cases = [
  // Basic string cases
  {
    input: Buffer.from("hello world"),
    expected: "hello world",
  },
  {
    input: Buffer.alloc(0),
    expected: Buffer.alloc(0),
  },
  {
    input: Buffer.from("a"),
    expected: "a",
  },
  {
    input: Buffer.from("12345"),
    expected: "12345",
  },

  // Special characters that should convert
  {
    input: Buffer.from("line1\nline2"),
    expected: "line1\nline2",
  },
  {
    input: Buffer.from("col1\tcol2"),
    expected: "col1\tcol2",
  },
  {
    input: Buffer.from("text\r\n"),
    expected: "text\r\n",
  },
  {
    input: Buffer.from('{"key": "value"}'),
    expected: '{"key": "value"}',
  },
  {
    input: Buffer.from('recursive="list"'),
    expected: 'recursive="list"',
  },
  {
    input: Buffer.from("ao-types"),
    expected: "ao-types",
  },

  // Binary data that should NOT convert
  {
    input: Buffer.from([255, 254, 253]),
    expected: Buffer.from([255, 254, 253]),
  },
  {
    input: Buffer.from([0, 0, 0, 0]),
    expected: Buffer.from([0, 0, 0, 0]),
  },
  {
    input: Buffer.from([1, 2, 3, 255, 0]),
    expected: Buffer.from([1, 2, 3, 255, 0]),
  },
  {
    input: Buffer.from([1, 2, 3, 4, 5]),
    expected: Buffer.from([1, 2, 3, 4, 5]),
  },

  // Edge cases
  {
    input: Buffer.from([0x01, 0x02, 0x03]), // ASCII control chars
    expected: Buffer.from([0x01, 0x02, 0x03]),
  },
  {
    input: Buffer.from(
      '"(ao-type-integer) 1", "(ao-type-list) \\"(ao-type-integer) 2\\""'
    ),
    expected:
      '"(ao-type-integer) 1", "(ao-type-list) \\"(ao-type-integer) 2\\""',
  },
  {
    input: Buffer.from("SGVsbG8gV29ybGQ="),
    expected: "SGVsbG8gV29ybGQ=",
  },
  {
    input: Buffer.from([192, 168, 1, 1]),
    expected: Buffer.from([192, 168, 1, 1]),
  },
  {
    input: Buffer.from([0]),
    expected: Buffer.from([0]),
  },
  {
    input: Buffer.from(
      "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"
    ),
    expected:
      "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~",
  },
]
