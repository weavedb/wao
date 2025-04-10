import lf from "localforage"
import { is, keys } from "ramda"

export default (_this, prefix) => {
  const k = key => (prefix ? `${prefix}-${key}` : key)
  return {
    put: async (key, val) => {
      let _val = null
      if (typeof val === "object" && !is(Array, val)) {
        _val = {}
        for (let k in val) {
          if (typeof val[k] !== "function") {
            if (is(Array, val[k])) {
              let _arr = []
              for (let v of val[k]) {
                if (
                  typeof v === "object" &&
                  !is(Array, v) &&
                  !(v instanceof Uint8Array) &&
                  !(v instanceof ArrayBuffer)
                ) {
                  let __val = {}
                  for (let k2 in v) {
                    if (typeof v[k2] !== "function") __val[k2] = v[k2]
                  }
                  _arr.push(__val)
                } else {
                  _arr.push(v)
                }
              }
              _val[k] = _arr
            } else if (
              typeof val[k] === "object" &&
              !(val[k] instanceof Uint8Array) &&
              !(val[k] instanceof ArrayBuffer)
            ) {
              let __val = {}
              for (let k2 in val[k]) {
                if (typeof val[k][k2] !== "function") __val[k2] = val[k][k2]
              }
              _val[k] = __val
            } else {
              _val[k] = val[k]
            }
          }
        }
      } else _val = val
      await lf.setItem(k(key), _val)
      _this.keys[key] = true
      await lf.setItem(k("keys"), keys(_this.keys))
    },
    get: async key => await lf.getItem(k(key)),
    getKeys: async ({ start, end }) => {
      if (!_this.keyInit) {
        const _keys = (await lf.getItem(k("keys"))) ?? []
        _this.keys = {}
        for (const v of _keys) _this.keys[v] = true
      }
      let __keys = []
      for (const k in _this.keys) {
        const sp = k.split(".")
        if (sp[0] === start) __keys.push(k)
      }
      return __keys
    },
  }
}
