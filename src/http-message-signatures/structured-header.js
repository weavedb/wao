import {
  isInnerList,
  parseDictionary,
  parseItem,
  parseList,
  serializeDictionary,
  serializeInnerList,
  serializeItem,
  serializeList,
} from "structured-headers"

export class Dictionary {
  constructor(input) {
    this.raw = input
    this.parsed = parseDictionary(input)
  }

  toString() {
    return this.serialize()
  }

  serialize() {
    return serializeDictionary(this.parsed)
  }

  has(key) {
    return this.parsed.has(key)
  }

  get(key) {
    const value = this.parsed.get(key)
    if (!value) {
      return value
    }
    if (isInnerList(value)) {
      return serializeInnerList(value)
    }
    return serializeItem(value)
  }
}

export class List {
  constructor(input) {
    this.raw = input
    this.parsed = parseList(input)
  }

  toString() {
    return this.serialize()
  }

  serialize() {
    return serializeList(this.parsed)
  }
}

export class Item {
  constructor(input) {
    this.raw = input
    this.parsed = parseItem(input)
  }

  toString() {
    return this.serialize()
  }

  serialize() {
    return serializeItem(this.parsed)
  }
}

export function parseHeader(header) {
  const classes = [List, Dictionary, Item]
  for (let i = 0; i < classes.length; i++) {
    try {
      return new classes[i](header)
    } catch (e) {
      // noop
    }
  }
  throw new Error("Unable to parse header as structured field")
}

/**
 * This allows consumers of the library to supply field specifications that aren't
 * strictly "structured fields". Really a string must start with a `"` but that won't
 * tend to happen in our configs.
 *
 * @param {string} input
 * @returns {string}
 */
export function quoteString(input) {
  // if it's not quoted, attempt to quote
  if (!input.startsWith('"')) {
    // try to split the structured field
    const [name, ...rest] = input.split(";")
    // no params, just quote the whole thing
    if (!rest.length) {
      return `"${name}"`
    }
    // quote the first part and put the rest back as it was
    return `"${name}";${rest.join(";")}`
  }
  return input
}
