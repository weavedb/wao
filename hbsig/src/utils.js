import sha256 from "fast-sha256"

function base64urlDecode(str) {
  str = str.replace(/-/g, "+").replace(/_/g, "/")
  const pad = str.length % 4
  if (pad === 2) str += "=="
  else if (pad === 3) str += "="
  else if (pad !== 0) throw new Error("Invalid base64url string")
  const bin = atob(str)
  const bytes = new Uint8Array(bin.length)
  for (let i = 0; i < bin.length; i++) bytes[i] = bin.charCodeAt(i)
  return bytes
}

function base64urlEncode(bytes) {
  let bin = ""
  for (const b of bytes) bin += String.fromCharCode(b)
  let b64 = btoa(bin)
  return b64.replace(/\+/g, "-").replace(/\//g, "_").replace(/=+$/g, "")
}

function toAddr(n) {
  if (typeof n === "object" && typeof n.n === "string") n = n.n
  const pubBytes = base64urlDecode(n)
  const hash = sha256(pubBytes)
  return base64urlEncode(hash)
}

export { toAddr }
