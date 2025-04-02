const { keys, omit, isNil } = require("ramda")
function generateId() {
  return Math.random().toString(36).substring(2, 15)
}

function toANS104Request(fields) {
  const dataItem = {
    target: fields.target,
    anchor: fields.anchor ?? "",
    tags: keys(
      omit(
        [
          "Target",
          "target",
          "Anchor",
          "anchor",
          "Data",
          "data",
          "data-protocol",
          "Data-Protocol",
          "variant",
          "Variant",
          "dryrun",
          "Dryrun",
          "Type",
          "type",
          "path",
          "method",
        ],
        fields
      )
    )
      .map(function (key) {
        return { name: key, value: fields[key] }
      }, fields)
      .concat([
        { name: "Data-Protocol", value: "ao" },
        { name: "Type", value: fields.Type ?? "Message" },
        { name: "Variant", value: fields.Variant ?? "ao.N.1" },
      ]),
    data: fields?.data || "",
  }
  return {
    headers: {
      "Content-Type": "application/ans104",
      "codec-device": "ans104@1.0",
    },
    item: dataItem,
  }
}

function parseSignatureInput(input) {
  const match = input.match(
    /^([^=]+)=\(([^)]+)\);alg="([^"]+)";keyid="([^"]+)"$/
  )
  if (!match) throw new Error("Invalid signature-input format")

  const [, label, fieldsStr, alg, keyid] = match
  const fields = fieldsStr.split('" "').map(f => f.replace(/"/g, ""))
  return { label, fields, alg, keyid }
}

module.exports = { toANS104Request, parseSignatureInput, generateId }
