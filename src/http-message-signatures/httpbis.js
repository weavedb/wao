import {
  parseDictionary,
  parseItem,
  serializeItem,
  serializeList,
  ByteSequence,
  serializeDictionary,
  parseList,
  isInnerList,
  isByteSequence,
  Token,
} from "structured-headers"
import { Dictionary, parseHeader, quoteString } from "./structured-header.js"

// Default params if not provided
const defaultParams = ["created", "keyid", "alg", "expires"]

// Helper to check if message is a request
const isRequest = message => {
  return message.method !== undefined
}

/**
 * Components can be derived from requests or responses (which can also be bound to their request).
 * The signature is essentially (component, params, signingSubject, supplementaryData)
 *
 * MODIFIED: This implementation matches Erlang behavior where @ components are treated as field lookups
 */
export function deriveComponent(component, params, message, req) {
  // MODIFIED: Match Erlang behavior - all @ components become field lookups
  // The Erlang strips @ and calls extract_field for ALL derived components

  // Remove @ prefix to match Erlang behavior
  const fieldName = component.startsWith("@") ? component.slice(1) : component

  // For all @ components, do a field lookup instead of deriving
  // This matches the Erlang identifier_to_component behavior
  if (component.startsWith("@")) {
    try {
      if (params.has("req") && req) {
        return extractHeader(fieldName, params, req, undefined)
      }
      return extractHeader(fieldName, params, message, req)
    } catch (e) {
      // If field not found, return empty or throw based on component type
      throw new Error(
        `No field "${fieldName}" found for component "${component}"`
      )
    }
  }

  // This code path should not be reached for @ components anymore
  // but keeping it for non-@ components
  throw new Error(`Unsupported component "${component}"`)
}

export function extractHeader(header, params, messageOrHeaders, req) {
  const headers = messageOrHeaders.headers || messageOrHeaders
  const context = params.has("req") ? req?.headers : headers
  if (!context) {
    throw new Error("Missing request in request-response bound component")
  }
  const headerTuple = Object.entries(context).find(
    ([name]) => name.toLowerCase() === header
  )
  if (!headerTuple) {
    throw new Error(`No header "${header}" found in headers`)
  }
  const values = Array.isArray(headerTuple[1])
    ? headerTuple[1]
    : [headerTuple[1]]
  if (params.has("bs") && (params.has("sf") || params.has("key"))) {
    throw new Error("Cannot have both `bs` and (implicit) `sf` parameters")
  }
  if (params.has("sf") || params.has("key")) {
    // strict encoding of field
    const value = values.join(", ")
    const parsed = parseHeader(value)
    if (params.has("key") && !(parsed instanceof Dictionary)) {
      throw new Error("Unable to parse header as dictionary")
    }
    if (params.has("key")) {
      const key = params.get("key").toString()
      if (!parsed.has(key)) {
        throw new Error(`Unable to find key "${key}" in structured field`)
      }
      return [parsed.get(key)]
    }
    return [parsed.toString()]
  }
  if (params.has("bs")) {
    return [
      values
        .map(val => {
          const encoded = Buffer.from(val.trim().replace(/\n\s*/gm, " "))
          return `:${encoded.toString("base64")}:`
        })
        .join(", "),
    ]
  }
  // raw encoding
  return [values.map(val => val.trim().replace(/\n\s*/gm, " ")).join(", ")]
}

function normaliseParams(params) {
  const map = new Map()
  params.forEach((value, key) => {
    if (value instanceof ByteSequence) {
      map.set(key, value.toBase64())
    } else if (value instanceof Token) {
      map.set(key, value.toString())
    } else {
      map.set(key, value)
    }
  })
  return map
}

export function createSignatureBase(config, res, req) {
  return config.fields.reduce((base, fieldName) => {
    const [field, params] = parseItem(quoteString(fieldName))
    const fieldParams = normaliseParams(params)
    const lcFieldName = field.toLowerCase()
    if (lcFieldName !== "@signature-params") {
      let value = null
      if (config.componentParser) {
        value =
          config.componentParser(lcFieldName, fieldParams, res, req) ?? null
      }
      if (value === null) {
        value = field.startsWith("@")
          ? deriveComponent(lcFieldName, fieldParams, res, req)
          : extractHeader(lcFieldName, fieldParams, res, req)
      }
      base.push([serializeItem([field, params]), value])
    }
    return base
  }, [])
}

export function formatSignatureBase(base) {
  return base
    .map(([key, value]) => {
      const quotedKey = serializeItem(parseItem(quoteString(key)))

      // MODIFIED: Special handling to match Erlang behavior
      // If the key is "@path", format it as "path" (without @) in the signature base
      let formattedKey = quotedKey
      if (quotedKey === '"@path"') {
        formattedKey = '"path"'
      }

      return value.map(val => `${formattedKey}: ${val}`).join("\n")
    })
    .join("\n")
}

export function createSigningParameters(config) {
  const now = new Date()
  return (config.params ?? defaultParams).reduce((params, paramName) => {
    let value = ""
    switch (paramName.toLowerCase()) {
      case "created":
        // created is optional but recommended. If created is supplied but is null, that's an explicit
        // instruction to *not* include the created parameter
        if (config.paramValues?.created !== null) {
          const created = config.paramValues?.created ?? now
          value = Math.floor(created.getTime() / 1000)
        }
        break
      case "expires":
        // attempt to obtain an explicit expires time, otherwise create one that is 300 seconds after
        // creation. Don't add an expires time if there is no created time
        if (
          config.paramValues?.expires ||
          config.paramValues?.created !== null
        ) {
          const expires =
            config.paramValues?.expires ??
            new Date((config.paramValues?.created ?? now).getTime() + 300000)
          value = Math.floor(expires.getTime() / 1000)
        }
        break
      case "keyid": {
        // attempt to obtain the keyid omit if missing
        const kid = config.paramValues?.keyid ?? config.key.id ?? null
        if (kid) {
          value = kid.toString()
        }
        break
      }
      case "alg": {
        // if there is no alg, but it's listed as a required parameter, we should probably
        // throw an error - the problem is that if it's in the default set of params, do we
        // really want to throw if there's no keyid?
        const alg = config.paramValues?.alg ?? config.key.alg ?? null
        if (alg) {
          value = alg.toString()
        }
        break
      }
      default:
        if (config.paramValues?.[paramName] instanceof Date) {
          value = Math.floor(config.paramValues[paramName].getTime() / 1000)
        } else if (config.paramValues?.[paramName]) {
          value = config.paramValues[paramName]
        }
    }
    if (value) {
      params.set(paramName, value)
    }
    return params
  }, new Map())
}

export function augmentHeaders(headers, signature, signatureInput, name) {
  let signatureHeaderName = "Signature"
  let signatureInputHeaderName = "Signature-Input"
  let signatureHeader = new Map()
  let inputHeader = new Map()
  // check to see if there are already signature/signature-input headers
  // if there are we want to store the current (case-sensitive) name of the header
  // and we want to parse out the current values so we can append our new signature
  for (const header in headers) {
    switch (header.toLowerCase()) {
      case "signature": {
        signatureHeaderName = header
        signatureHeader = parseDictionary(
          Array.isArray(headers[header])
            ? headers[header].join(", ")
            : headers[header]
        )
        break
      }
      case "signature-input":
        signatureInputHeaderName = header
        inputHeader = parseDictionary(
          Array.isArray(headers[header])
            ? headers[header].join(", ")
            : headers[header]
        )
        break
    }
  }
  // find a unique signature name for the header. Check if any existing headers already use
  // the name we intend to use, if there are, add incrementing numbers to the signature name
  // until we have a unique name to use
  let signatureName = name ?? "sig"
  if (signatureHeader.has(signatureName) || inputHeader.has(signatureName)) {
    let count = 0
    while (
      signatureHeader.has(`${signatureName}${count}`) ||
      inputHeader.has(`${signatureName}${count}`)
    ) {
      count++
    }
    signatureName += count.toString()
  }
  // append our signature and signature-inputs to the headers and return
  signatureHeader.set(signatureName, [
    new ByteSequence(signature.toString("base64")),
    new Map(),
  ])
  inputHeader.set(signatureName, parseList(signatureInput)[0])
  return {
    ...headers,
    [signatureHeaderName]: serializeDictionary(signatureHeader),
    [signatureInputHeaderName]: serializeDictionary(inputHeader),
  }
}

export async function signMessage(config, message, req) {
  const signingParameters = createSigningParameters(config)
  const signatureBase = createSignatureBase(
    {
      fields: config.fields ?? [],
      componentParser: config.componentParser,
    },
    message,
    req
  )
  const signatureInput = serializeList([
    [signatureBase.map(([item]) => parseItem(item)), signingParameters],
  ])
  signatureBase.push(['"@signature-params"', [signatureInput]])
  const base = formatSignatureBase(signatureBase)
  // call sign
  const signature = await config.key.sign(Buffer.from(base))
  return {
    ...message,
    headers: augmentHeaders(
      { ...message.headers },
      signature,
      signatureInput,
      config.name
    ),
  }
}

export async function verifyMessage(config, message, req) {
  const { signatures, signatureInputs } = Object.entries(
    message.headers
  ).reduce((accum, [name, value]) => {
    switch (name.toLowerCase()) {
      case "signature":
        return Object.assign(accum, {
          signatures: parseDictionary(
            Array.isArray(value) ? value.join(", ") : value
          ),
        })
      case "signature-input":
        return Object.assign(accum, {
          signatureInputs: parseDictionary(
            Array.isArray(value) ? value.join(", ") : value
          ),
        })
      default:
        return accum
    }
  }, {})
  // no signatures means an indeterminate result
  if (!signatures?.size && !signatureInputs?.size) {
    return null
  }
  // a missing header means we can't verify the signatures
  if (!signatures?.size || !signatureInputs?.size) {
    throw new Error("Incomplete signature headers")
  }
  const now = Math.floor(Date.now() / 1000)
  const tolerance = config.tolerance ?? 0
  const notAfter =
    config.notAfter instanceof Date
      ? Math.floor(config.notAfter.getTime() / 1000)
      : (config.notAfter ?? now)
  const maxAge = config.maxAge ?? null
  const requiredParams = config.requiredParams ?? []
  const requiredFields = config.requiredFields ?? []
  return Array.from(signatureInputs.entries()).reduce(
    async (prev, [name, input]) => {
      const signatureParams = Array.from(input[1].entries()).reduce(
        (params, [key, value]) => {
          if (value instanceof ByteSequence) {
            Object.assign(params, {
              [key]: value.toBase64(),
            })
          } else if (value instanceof Token) {
            Object.assign(params, {
              [key]: value.toString(),
            })
          } else if (key === "created" || key === "expired") {
            Object.assign(params, {
              [key]: new Date(value * 1000),
            })
          } else {
            Object.assign(params, {
              [key]: value,
            })
          }
          return params
        },
        {}
      )
      const [result, key] = await Promise.all([
        prev.catch(e => e),
        config.keyLookup(signatureParams),
      ])
      // @todo - confirm this is all working as expected
      if (config.all && !key) {
        throw new Error("Unknown key")
      }
      if (!key) {
        if (result instanceof Error) {
          throw result
        }
        return result
      }
      if (
        input[1].has("alg") &&
        key.algs?.includes(input[1].get("alg")) === false
      ) {
        throw new Error("Unsupported key algorithm")
      }
      if (!isInnerList(input)) {
        throw new Error("Malformed signature input")
      }
      const hasRequiredParams = requiredParams.every(param =>
        input[1].has(param)
      )
      if (!hasRequiredParams) {
        throw new Error("Missing required signature parameters")
      }
      // this could be tricky, what if we say "@method" but there is "@method;req"
      const hasRequiredFields = requiredFields.every(field =>
        input[0].some(([fieldName]) => fieldName === field)
      )
      if (!hasRequiredFields) {
        throw new Error("Missing required signed fields")
      }
      if (input[1].has("created")) {
        const created = input[1].get("created") - tolerance
        // maxAge overrides expires.
        // signature is older than maxAge
        if ((maxAge && now - created > maxAge) || created > notAfter) {
          throw new Error("Signature is too old")
        }
      }
      if (input[1].has("expires")) {
        const expires = input[1].get("expires") + tolerance
        // expired signature
        if (now > expires) {
          throw new Error("Signature has expired")
        }
      }
      // now look to verify the signature! Build the expected "signing base" and verify it!
      const fields = input[0].map(item => serializeItem(item))
      const signingBase = createSignatureBase(
        { fields, componentParser: config.componentParser },
        message,
        req
      )
      signingBase.push(['"@signature-params"', [serializeList([input])]])
      const base = formatSignatureBase(signingBase)
      const signature = signatures.get(name)
      if (!signature) {
        throw new Error("No corresponding signature for input")
      }
      if (!isByteSequence(signature[0])) {
        throw new Error("Malformed signature")
      }
      return key.verify(
        Buffer.from(base),
        Buffer.from(signature[0].toBase64(), "base64"),
        signatureParams
      )
    },
    Promise.resolve(null)
  )
}
