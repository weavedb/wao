import BAR from "./bar.js"
import ArMem from "./armem-web.js"

class AR extends BAR {
  constructor(opt = {}) {
    super({ ...opt, ArMem })
  }
}

export default AR
