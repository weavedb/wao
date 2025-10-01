import base64url from "base64url"
import { httpbis } from "./http-message-signatures/index.js"
import { parseItem, serializeList } from "structured-headers"
import { httpsig_from } from "./httpsig.js"
import { structured_to } from "./structured.js"
import { result } from "./send-utils.js"

const {
  augmentHeaders,
  createSignatureBase,
  createSigningParameters,
  formatSignatureBase,
} = httpbis

const toMsg = async req => {
  let msg = {}
  req?.headers?.forEach((v, k) => {
    msg[k] = v
  })
  if (req.body) {
    const arrayBuffer = await req.arrayBuffer()
    msg.body =
      typeof Buffer !== "undefined"
        ? Buffer.from(arrayBuffer) // Node.js
        : new Uint8Array(arrayBuffer) // Browser
  }

  return msg
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
  return await result(response)
}

export const httpSigName = address => {
  const decoded = base64url.toBuffer(address)
  const hexString = [...decoded.subarray(1, 9)]
    .map(byte => byte.toString(16).padStart(2, "0"))
    .join("")
  return `http-sig-${hexString}`
}

const toView = value => {
  if (ArrayBuffer.isView(value)) {
    return Buffer.from(value.buffer, value.byteOffset, value.byteLength)
  } else if (typeof value === "string") return base64url.toBuffer(value)

  throw new Error(
    "Value must be Uint8Array, ArrayBuffer, or base64url-encoded string"
  )
}

export const toHttpSigner = signer => {
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

      // SORT THE FIELDS HERE to match Erlang's lists:sort(maps:keys(Enc))
      const sortedFields = [...fields].sort()

      const signatureBaseArray = createSignatureBase(
        { fields: sortedFields },
        request
      )
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
