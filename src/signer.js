import base64url from "base64url"
import { hash } from "fast-sha256"
import { httpbis } from "http-message-signatures"
import { parseItem, serializeList } from "structured-headers"
const { verifyMessage } = httpbis
const {
  augmentHeaders,
  createSignatureBase,
  createSigningParameters,
  formatSignatureBase,
} = httpbis

export function hbEncodeValue(value) {
  if (isBytes(value)) {
    if (value.byteLength === 0) return hbEncodeValue("")
    return [undefined, value]
  }

  if (typeof value === "string") {
    if (value.length === 0) return ["empty-binary", undefined]
    return [undefined, value]
  }

  if (Array.isArray(value)) {
    if (value.length === 0) return ["empty-list", undefined]
    if (value.some(isPojo)) {
      throw new Error(
        `Array with objects should have been lifted: ${JSON.stringify(value)}`
      )
    }

    const encoded = value
      .map(v => {
        if (typeof v === "string") {
          if (v === "") return `"(ao-type-empty-binary) "`
          const escaped = v.replace(/\\/g, "\\\\").replace(/"/g, '\\"')
          return `"${escaped}"`
        } else if (typeof v === "number") return String(v)
        else if (typeof v === "boolean") return v ? "?1" : "?0"
        else if (typeof v === "symbol") {
          const desc = v.description || "symbol"
          const escaped = desc.replace(/\\/g, "\\\\").replace(/"/g, '\\"')
          return `"(ao-type-atom) ${escaped}"`
        } else if (v === null) return `"(ao-type-atom) null"`
        else if (v === undefined) return `"(ao-type-atom) undefined"`
        else if (Array.isArray(v) && v.length === 0) {
          return `"(ao-type-empty-list) "`
        }
        return `"${String(v)}"`
      })
      .join(", ")
    return ["list", encoded]
  }

  if (typeof value === "number") {
    if (!Number.isInteger(value)) return ["float", `${value}`]
    return ["integer", String(value)]
  }

  if (typeof value === "boolean") {
    return ["atom", `"${value ? "true" : "false"}"`]
  }

  if (typeof value === "symbol") {
    const desc = value.description || "symbol"
    return ["atom", `"${desc}"`]
  }

  if (value === null) return ["atom", `"null"`]

  if (value === undefined) return ["atom", `"undefined"`]

  throw new Error(`Cannot encode value: ${String(value)}`)
}

const toView = value => {
  if (ArrayBuffer.isView(value)) {
    return Buffer.from(value.buffer, value.byteOffset, value.byteLength)
  } else if (typeof value === "string") return base64url.toBuffer(value)

  throw new Error(
    "Value must be Uint8Array, ArrayBuffer, or base64url-encoded string"
  )
}

const httpSigName = address => {
  const decoded = base64url.toBuffer(address)
  const hexString = [...decoded.subarray(1, 9)]
    .map(byte => byte.toString(16).padStart(2, "0"))
    .join("")
  return `http-sig-${hexString}`
}

const joinUrl = ({ url, path }) => {
  if (path.startsWith("http://") || path.startsWith("https://")) {
    return path
  }
  return url.endsWith("/") ? url.slice(0, -1) + path : url + path
}

const MAX_HEADER_LENGTH = 4096

function encode_body_keys(bodyKeys) {
  if (!bodyKeys || bodyKeys.length === 0) return ""
  const items = bodyKeys.map(key => `"${key}"`)
  const result = items.join(", ")
  return result
}

async function hasNewline(value) {
  if (typeof value === "string") return value.includes("\n")
  if (value instanceof Blob) {
    value = await value.text()
    return value.includes("\n")
  }
  if (isBytes(value)) return Buffer.from(value).includes("\n")
  return false
}

async function sha256(data) {
  // Convert data to Uint8Array if needed
  let uint8Array
  if (data instanceof ArrayBuffer) {
    uint8Array = new Uint8Array(data)
  } else if (data instanceof Uint8Array) {
    uint8Array = data
  } else if (ArrayBuffer.isView(data)) {
    uint8Array = new Uint8Array(data.buffer, data.byteOffset, data.byteLength)
  } else {
    throw new Error("sha256 expects ArrayBuffer or ArrayBufferView")
  }

  // fast-sha256 returns Uint8Array, convert to ArrayBuffer
  const hashResult = hash(uint8Array)
  return hashResult.buffer.slice(
    hashResult.byteOffset,
    hashResult.byteOffset + hashResult.byteLength
  )
}

function isBytes(value) {
  return value instanceof ArrayBuffer || ArrayBuffer.isView(value)
}

function isPojo(value) {
  return (
    !isBytes(value) &&
    !Array.isArray(value) &&
    !(value instanceof Blob) &&
    typeof value === "object" &&
    value !== null
  )
}

function hbEncodeLift(obj, parent = "", top = {}) {
  const [flattened, types] = Object.entries({ ...obj }).reduce(
    (acc, [key, value]) => {
      const storageKey = parent ? `${parent}/${key}` : key
      if (value == null) {
        const [type, encoded] = hbEncodeValue(value)
        if (encoded !== undefined) acc[0][key] = encoded
        if (type) acc[1][key.toLowerCase()] = type
        return acc
      }
      if (Array.isArray(value)) {
        const hasObjects = value.some(isPojo)
        const hasBinary = value.some(isBytes)
        if (hasObjects || hasBinary) {
          const indexedObj = value.reduce(
            (obj, v, idx) => Object.assign(obj, { [idx]: v }),
            {}
          )
          acc[1][key.toLowerCase()] = "list"
          hbEncodeLift(indexedObj, storageKey, top)
          return acc
        } else {
          const [type, encoded] = hbEncodeValue(value)
          if (type) acc[1][key.toLowerCase()] = type
          if (encoded !== undefined) acc[0][key] = encoded
          return acc
        }
      }

      const originalValue = value

      if (isPojo(value)) {
        if (Object.keys(value).length === 0) {
          acc[1][key.toLowerCase()] = "empty-message"
          return acc
        }

        const hasComplexValues = Object.values(value).some(
          v => isPojo(v) || (Array.isArray(v) && v.some(item => isPojo(item)))
        )

        if (!hasComplexValues) {
          const items = []
          const hasAnyNonEmptyValues = Object.values(value).some(v => {
            return !(
              v === null ||
              v === undefined ||
              v === "" ||
              (Array.isArray(v) && v.length === 0) ||
              (isPojo(v) && Object.keys(v).length === 0)
            )
          })

          Object.entries(value).forEach(([k, v]) => {
            const subKey = k.toLowerCase()

            if (v === null) {
              items.push(`${subKey}="null"`)
              acc[1][`${key.toLowerCase()}%2f${subKey}`] = "atom"
            } else if (v === undefined) {
              items.push(`${subKey}="undefined"`)
              acc[1][`${key.toLowerCase()}%2f${subKey}`] = "atom"
            } else if (typeof v === "string") {
              if (v === "") {
                acc[1][`${key.toLowerCase()}%2f${subKey}`] = "empty-binary"
              } else {
                const escaped = v.replace(/\\/g, "\\\\").replace(/"/g, '\\"')
                items.push(`${subKey}="${escaped}"`)
              }
            } else if (typeof v === "number") {
              items.push(`${subKey}=${v}`)
              if (Number.isInteger(v)) {
                acc[1][`${key.toLowerCase()}%2f${subKey}`] = "integer"
              } else {
                acc[1][`${key.toLowerCase()}%2f${subKey}`] = "float"
              }
            } else if (typeof v === "boolean") {
              items.push(`${subKey}=${v ? "?1" : "?0"}`)
              acc[1][`${key.toLowerCase()}%2f${subKey}`] = "boolean"
            } else if (typeof v === "symbol") {
              const desc = v.description || "symbol"
              const escaped = desc.replace(/\\/g, "\\\\").replace(/"/g, '\\"')
              items.push(`${subKey}="${escaped}"`)
              acc[1][`${key.toLowerCase()}%2f${subKey}`] = "atom"
            } else if (Array.isArray(v) && !v.some(item => isPojo(item))) {
              if (v.length === 0) {
                items.push(`${subKey}=()`)
                acc[1][`${key.toLowerCase()}%2f${subKey}`] = "empty-list"
              } else {
                const listItems = v.map(item => {
                  if (typeof item === "string") {
                    return `"${item.replace(/\\/g, "\\\\").replace(/"/g, '\\"')}"`
                  } else if (typeof item === "number") {
                    return String(item)
                  } else if (typeof item === "boolean") {
                    return item ? "?1" : "?0"
                  } else if (typeof item === "symbol") {
                    const desc = item.description || "symbol"
                    return `"${desc.replace(/\\/g, "\\\\").replace(/"/g, '\\"')}"`
                  } else if (item === null) {
                    return `"null"`
                  } else if (item === undefined) {
                    return `"undefined"`
                  } else {
                    return `"${String(item)}"`
                  }
                })
                items.push(`${subKey}=(${listItems.join(" ")})`)
              }
            } else if (isPojo(v) && Object.keys(v).length === 0) {
              items.push(`${subKey}`)
              acc[1][`${key.toLowerCase()}%2f${subKey}`] = "empty-message"
            }
          })

          const encodedValue = items.join(", ")

          const hasOnlyEmptyValues = Object.entries(value).every(([k, v]) => {
            return (
              v === null ||
              v === undefined ||
              v === "" ||
              (Array.isArray(v) && v.length === 0) ||
              (isPojo(v) && Object.keys(v).length === 0)
            )
          })

          if (!hasAnyNonEmptyValues) {
            acc[1][key.toLowerCase()] = "map"
          } else if (encodedValue === "") {
            acc[1][key.toLowerCase()] = "empty-message"
          } else {
            acc[0][key] = encodedValue
            acc[1][key.toLowerCase()] = "map"
          }
        } else hbEncodeLift(value, storageKey, top)
        return acc
      }

      const [type, encoded] = hbEncodeValue(value)

      if (encoded !== undefined) {
        if (isBytes(encoded)) top[storageKey] = encoded
        else {
          acc[0][key] = encoded
          if (type) acc[1][key.toLowerCase()] = type
        }
      } else if (type) acc[1][key.toLowerCase()] = type
      return acc
    },
    [{}, {}]
  )

  if (Object.keys(flattened).length === 0 && Object.keys(types).length === 0)
    return top

  if (Object.keys(types).length > 0) {
    const aoTypeItems = Object.entries(types).map(([key, value]) => {
      const safeKey = key
        .toLowerCase()
        .replace(
          /[^a-z0-9_\-.*\/]/g,
          c => "%" + c.charCodeAt(0).toString(16).padStart(2, "0")
        )
        .replace(/\//g, "%2f")
      return `${safeKey}="${value}"`
    })
    aoTypeItems.sort()
    const aoTypes = aoTypeItems.join(", ")

    if (Buffer.from(aoTypes).byteLength > MAX_HEADER_LENGTH) {
      const flatK = parent ? `${parent}/ao-types` : "ao-types"
      top[flatK] = aoTypes
    } else flattened["ao-types"] = aoTypes
  }

  if (parent) top[parent] = flattened
  else Object.assign(top, flattened)
  return top
}

function encodePart(name, { headers = {}, body }) {
  const headerEntries =
    headers instanceof Headers
      ? Array.from(headers.entries())
      : Object.entries(headers || {})

  const parts = headerEntries.reduce(
    (acc, [name, value]) => {
      acc.push(`${name}: `, value, "\r\n")
      return acc
    },
    [`content-disposition: form-data;name="${name}"\r\n`]
  )

  if (body) parts.push("\r\n", body)

  return new Blob(parts)
}

async function encode(obj = {}) {
  if (Object.keys(obj).length === 0) return { headers: {}, body: undefined }
  const originalObj = obj
  const flattened = hbEncodeLift(obj)

  const bodyKeys = []
  const headerKeys = []

  await Promise.all(
    Object.keys(flattened).map(async key => {
      const value = flattened[key]

      if (isPojo(value)) {
        const subPart = await encode(value)
        if (!subPart) return

        bodyKeys.push(key)
        flattened[key] = encodePart(key, subPart)
        return
      }

      if (isBytes(value)) {
        bodyKeys.push(key)
        const uint8Array =
          value instanceof Uint8Array
            ? value
            : value instanceof ArrayBuffer
              ? new Uint8Array(value)
              : Buffer.isBuffer(value)
                ? new Uint8Array(value.buffer, value.byteOffset, value.length)
                : new Uint8Array(
                    value.buffer,
                    value.byteOffset,
                    value.byteLength
                  )
        flattened[key] = new Blob([
          `content-disposition: form-data;name="${key}"\r\n\r\n`,
          uint8Array,
        ])
        return
      }

      const valueStr = String(value)
      if (
        (await hasNewline(valueStr)) ||
        key.includes("/") ||
        Buffer.from(valueStr).byteLength > MAX_HEADER_LENGTH ||
        (isPojo(value) && valueStr === "[object Object]")
      ) {
        bodyKeys.push(key)
        flattened[key] = new Blob([
          `content-disposition: form-data;name="${key}"\r\n\r\n`,
          value,
        ])
        return
      }

      headerKeys.push(key)
    })
  )

  const headers = {}
  headerKeys.forEach(key => {
    headers[key] = flattened[key]
  })

  if ("data" in originalObj && !bodyKeys.includes("data")) {
    bodyKeys.push("data")
    delete headers["data"]
  }

  if ("body" in originalObj && !bodyKeys.includes("body")) {
    bodyKeys.push("body")
    delete headers["body"]
  }

  if (bodyKeys.length > 0) {
    headers["body-keys"] = encode_body_keys(bodyKeys)
  }

  let body = undefined
  let promoteToBody = true
  if (bodyKeys.length > 0) {
    if (bodyKeys.length === 1) {
      const bodyKey = bodyKeys[0]
      const originalValue = originalObj[bodyKey]
      const flattenedValue = flattened[bodyKey]

      if (
        !isPojo(originalValue) ||
        (isPojo(originalValue) && typeof flattenedValue === "string")
      ) {
        if (
          (bodyKey === "body" || bodyKey === "data") &&
          isPojo(originalValue) &&
          typeof flattenedValue === "string"
        ) {
          body = new Blob([flattenedValue])
        } else if (Array.isArray(originalValue)) {
          const hasSymbols = originalValue.some(
            item => typeof item === "symbol"
          )
          if (hasSymbols) {
            const [type, encoded] = hbEncodeValue(originalValue)
            body = new Blob([encoded || originalValue.toString()])
          } else body = new Blob([originalValue.toString()])
        } else body = new Blob([originalValue || flattenedValue])
        headers["inline-body-key"] = bodyKey
      } else promoteToBody = false
    }

    if (!promoteToBody || bodyKeys.length > 1) {
      const bodyParts = await Promise.all(
        bodyKeys.map(async name => {
          if (flattened[name] instanceof Blob) return flattened[name]
          const value = originalObj[name] || flattened[name] || ""
          if (
            name === "body" &&
            isPojo(originalObj[name]) &&
            typeof flattened[name] === "string"
          ) {
            const partBlob = new Blob([
              `content-disposition: form-data;name="${name}"\r\n\r\n`,
              flattened[name],
            ])
            return partBlob
          }

          let valueToEncode = value
          if (Array.isArray(value)) {
            const hasSymbols = value.some(item => typeof item === "symbol")
            if (hasSymbols) {
              const [type, encoded] = hbEncodeValue(value)
              valueToEncode = encoded || value.toString()
            }
          } else if (isBytes(value)) valueToEncode = value
          let partBlob
          if (isBytes(valueToEncode)) {
            const uint8Array =
              valueToEncode instanceof Uint8Array
                ? valueToEncode
                : valueToEncode instanceof ArrayBuffer
                  ? new Uint8Array(valueToEncode)
                  : Buffer.isBuffer(valueToEncode)
                    ? new Uint8Array(
                        valueToEncode.buffer,
                        valueToEncode.byteOffset,
                        valueToEncode.length
                      )
                    : new Uint8Array(
                        valueToEncode.buffer,
                        valueToEncode.byteOffset,
                        valueToEncode.byteLength
                      )
            partBlob = new Blob([
              `content-disposition: form-data;name="${name}"\r\n\r\n`,
              uint8Array,
            ])
          } else {
            partBlob = new Blob([
              `content-disposition: form-data;name="${name}"\r\n\r\n`,
              valueToEncode,
            ])
          }
          return partBlob
        })
      )

      const allPartsBuffer = await new Blob(bodyParts).arrayBuffer()
      const hashResult = await sha256(allPartsBuffer)
      const boundary = base64url.encode(Buffer.from(hashResult))

      const finalParts = []
      for (const part of bodyParts) {
        finalParts.push(`--${boundary}\r\n`)
        finalParts.push(part)
        finalParts.push("\r\n")
      }
      finalParts.push(`--${boundary}--`)

      headers["content-type"] = `multipart/form-data; boundary="${boundary}"`
      body = new Blob(finalParts)
    }

    if (body) {
      const finalContent = await body.arrayBuffer()
      const contentDigest = await sha256(finalContent)
      const base64 = base64url.toBase64(base64url.encode(contentDigest))
      headers["content-digest"] = `sha-256=:${base64}:`
      headers["content-length"] = String(finalContent.byteLength)
    }
  }

  return { headers, body }
}

const toHttpSigner = signer => {
  const params = ["alg", "keyid"].sort()

  return async ({ request, fields }) => {
    let signatureBase
    let signatureInput
    let createCalled = false

    const create = injected => {
      createCalled = true

      const { publicKey, alg = "rsa-pss-sha512" } = injected

      const publicKeyBuffer = toView(publicKey)

      const signingParameters = createSigningParameters({
        params,
        paramValues: {
          keyid: base64url.encode(publicKeyBuffer),
          alg,
        },
      })

      const signatureBaseArray = createSignatureBase({ fields }, request)
      signatureInput = serializeList([
        [
          signatureBaseArray.map(([item]) => parseItem(item)),
          signingParameters,
        ],
      ])

      signatureBaseArray.push(['"@signature-params"', [signatureInput]])
      signatureBase = formatSignatureBase(signatureBaseArray)

      return new TextEncoder().encode(signatureBase)
    }

    const result = await signer(create, "httpsig")

    if (!createCalled) {
      throw new Error(
        "create() must be invoked in order to construct the data to sign"
      )
    }

    if (!result.signature || !result.address) {
      throw new Error("Signer must return signature and address")
    }

    const signatureBuffer = toView(result.signature)
    const signedHeaders = augmentHeaders(
      request.headers,
      signatureBuffer,
      signatureInput,
      httpSigName(result.address)
    )

    const finalHeaders = {}
    for (const [key, value] of Object.entries(signedHeaders)) {
      if (key === "Signature" || key === "Signature-Input") {
        finalHeaders[key.toLowerCase()] = value
      } else finalHeaders[key] = value
    }

    return { ...request, headers: finalHeaders }
  }
}

export function createRequest(config) {
  const { signer, url = "http://localhost:10001" } = config

  if (!signer) {
    throw new Error("Signer is required for mainnet mode")
  }

  return async function request(fields) {
    const { path = "/relay/process", method = "POST", ...restFields } = fields
    const aoFields = { ...restFields }
    const rootKeys = Object.keys(aoFields)
    const binaryKeys = rootKeys.filter(key => isBytes(aoFields[key]))

    if (binaryKeys.length > 1 && !aoFields.body && !aoFields.data) {
      aoFields.body = "1984"
    }

    const encoded = await encode(aoFields)
    const headersObj = encoded ? encoded.headers : {}
    const body = encoded ? encoded.body : undefined

    const _url = joinUrl({ url, path })

    if (body && !headersObj["content-length"]) {
      const bodySize = body.size || body.byteLength || 0
      if (bodySize > 0) {
        headersObj["content-length"] = String(bodySize)
      }
    }

    const lowercaseHeaders = {}
    for (const [key, value] of Object.entries(headersObj)) {
      lowercaseHeaders[key.toLowerCase()] = value
    }

    const signingFields = Object.keys(lowercaseHeaders).filter(
      key => key !== "body-keys"
    )

    if (signingFields.length === 0 && !body) {
      lowercaseHeaders["content-length"] = "0"
      signingFields.push("content-length")
    }

    const signedRequest = await toHttpSigner(signer)({
      request: { url: _url, method, headers: lowercaseHeaders },
      fields: signingFields,
    })

    const finalHeaders = {}

    for (const [key, value] of Object.entries(headersObj)) {
      finalHeaders[key] = value
    }

    finalHeaders["signature"] = signedRequest.headers["signature"]
    finalHeaders["signature-input"] = signedRequest.headers["signature-input"]

    if (headersObj["body-keys"]) {
      finalHeaders["body-keys"] = headersObj["body-keys"]
    }

    const result = { url: _url, method, headers: finalHeaders }

    if (body) result.body = body

    return result
  }
}
export async function send(signedMsg, fetchImpl = fetch) {
  const fetchOptions = {
    method: signedMsg.method,
    headers: signedMsg.headers,
    redirect: "follow",
  }
  if (
    signedMsg.body !== undefined &&
    signedMsg.method !== "GET" &&
    signedMsg.method !== "HEAD"
  ) {
    fetchOptions.body = signedMsg.body
  }
  const response = await fetchImpl(signedMsg.url, fetchOptions)

  if (response.status >= 400) {
    throw new Error(`${response.status}: ${await response.text()}`)
  }

  let headers = {}
  if (response.headers && typeof response.headers.forEach === "function") {
    response.headers.forEach((v, k) => (headers[k] = v))
  } else headers = response.headers

  return {
    response,
    headers,
    body: await response.text(),
    status: response.status,
  }
}
