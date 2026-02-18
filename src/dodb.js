// Strip functions and other non-serializable values before storing.
// LMDB silently drops these; DO storage would throw on them.
// The codebase handles missing `handle` by recreating it on read.
function sanitize(val) {
  if (val == null) return val
  if (typeof val === "function") return null
  if (typeof val !== "object") return val
  if (val instanceof Uint8Array || val instanceof ArrayBuffer) return val
  if (ArrayBuffer.isView(val)) return val
  if (Array.isArray(val)) return val.map(sanitize)
  const out = {}
  for (const k of Object.keys(val)) {
    const v = val[k]
    if (typeof v !== "function") out[k] = sanitize(v)
  }
  return out
}

export default (storage) => ({
  put: async (key, val) => await storage.put(key, sanitize(val)),
  get: async (key) => await storage.get(key) ?? null,
  getKeys: async ({ start, end }) => {
    const map = await storage.list({ start, end })
    return [...map.keys()]
  },
})
