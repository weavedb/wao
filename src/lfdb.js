import lf from "localforage"
import { is, keys } from "ramda"

export default _this => {
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
      if (key.match(/^env/)) {
        _val.memory = Array.from(_this.compressor.compress(_val.memory))
      }
      await lf.setItem(key, _val)
      _this.keys[key] = true
      await lf.setItem("keys", keys(_this.keys))
    },
    get: async key => {
      const val = await lf.getItem(key)
      if (key.match(/^env/)) {
        val.memory = _this.decompressor.decompress(val.memory)
      }
      return val
    },
    getKeys: async ({ start, end }) => {
      if (!_this.keyInit) {
        const _keys = (await lf.getItem("keys")) ?? []
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
