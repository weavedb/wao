import BAR from "./ar-base.js"
import ArMem from "./armem-web.js"

class AR extends BAR {
  constructor(opt = {}) {
    super({ ...opt, ArMem })
  }
}

export default AR
