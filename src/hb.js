class HB {
  constructor({ url = "http://localhost:10000" } = {}) {
    this.url = url
  }
  async metrics() {
    const txt = await fetch(`${this.url}/metrics`).then(r => r.text())
    const parts = txt.split(/\r?\n/)
    let index = 0
    let _metrics = {}
    let vals = []
    let desc = []
    for (const v of parts) {
      if (/^# /.test(v)) {
        const [_, type, name, ...rest] = v.split(" ")
        if (!_metrics[name]) _metrics[name] = { index: ++index, values: [] }
        _metrics[name][type] = rest.join(" ")
      } else if (v !== "") {
        if (/{/.test(v)) {
          const [name, rest] = v.split("{")
          if (!_metrics[name]) _metrics[name] = { index: ++index, values: [] }
          const [params, val] = rest.split("}")
          let _params = {}
          for (const v of params.split(",")) {
            const [key, val] = v.trim().split("=")
            _params[key] = val.replace(/"/g, "")
          }
          _metrics[name].values.push({ value: val.trim(), params: _params })
        } else {
          const [name, val] = v.split(" ")
          if (!_metrics[name]) _metrics[name] = { index: ++index, values: [] }
          _metrics[name].values.push({ value: val })
        }
      }
    }
    return _metrics
  }
  async info() {
    const txt = await fetch(`${this.url}/~meta@1.0/info`).then(r => r.text())
    const parts = txt.split(/\r?\n/)
    let _info = null
    let allinfo = []
    for (const v of parts) {
      if (/^--/.test(v)) {
        if (_info !== null) allinfo.push(_info)
        _info = {}
      } else {
        const [name, ...rest] = v.split(": ")
        _info[name] = rest.join(": ")
      }
    }
    return allinfo
  }
}

export default HB
