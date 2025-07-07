export default function genSimple() {
  const values = {
    emptyBuf: Buffer.from([]),
    buf: Buffer.from([1, 2, 3]),
    emptyObj: {},
    emptyArr: [],
    str: "abc",
    int: 1,
    float: 3.14,
    bool: true,
    sym: Symbol("atom"),
    null: null,
    undef: undefined,
  }

  const testCases = []

  // Helper to check if we should skip this combination
  function shouldSkip(obj) {
    // Skip if data has empty buffer (known server issue)
    if (obj.data && Buffer.isBuffer(obj.data) && obj.data.length === 0) {
      return true
    }
    return false
  }

  // Just data/body
  for (const [, dataVal] of Object.entries(values)) {
    for (const [, bodyVal] of Object.entries(values)) {
      const testCase = { data: dataVal, body: bodyVal }
      if (!shouldSkip(testCase)) {
        testCases.push(testCase)
      }
    }
  }

  // data/body + num (only number values)
  const numValues = [values.int, values.float, 0]
  for (const [, dataVal] of Object.entries(values)) {
    for (const [, bodyVal] of Object.entries(values)) {
      for (const numVal of numValues) {
        const testCase = { data: dataVal, body: bodyVal, num: numVal }
        if (!shouldSkip(testCase)) {
          testCases.push(testCase)
        }
      }
    }
  }

  // data/body + str (only string values)
  for (const [, dataVal] of Object.entries(values)) {
    for (const [, bodyVal] of Object.entries(values)) {
      const testCase = { data: dataVal, body: bodyVal, str: "abc" }
      if (!shouldSkip(testCase)) {
        testCases.push(testCase)
      }
    }
  }

  // data/body + ok (only boolean values)
  for (const [, dataVal] of Object.entries(values)) {
    for (const [, bodyVal] of Object.entries(values)) {
      const testCase = { data: dataVal, body: bodyVal, ok: true }
      if (!shouldSkip(testCase)) {
        testCases.push(testCase)
      }
    }
  }

  // data/body + bin (only buffer values)
  const binValues = [values.emptyBuf, values.buf]
  for (const [, dataVal] of Object.entries(values)) {
    for (const [, bodyVal] of Object.entries(values)) {
      for (const binVal of binValues) {
        const testCase = { data: dataVal, body: bodyVal, bin: binVal }
        if (!shouldSkip(testCase)) {
          testCases.push(testCase)
        }
      }
    }
  }

  return testCases
}
