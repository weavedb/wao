import assert from "assert"
import { resolve } from "path"
import { describe, it } from "node:test"
import { AO, acc } from "../src/test.js"
import { ok, fail, Src } from "../src/helpers.js"
import weavedb from "../src/weavedb.js"
import { encode, Encoder, Bundle } from "arjson"

let attestor = acc[0]
const { signer, jwk } = attestor

const handler = `
Handlers.add("Hello", "Hello", function (msg)
  msg.reply({ Data = "" })
end)
`

describe("WebRTC", function () {})
