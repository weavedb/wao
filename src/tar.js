import BAR from "./ar-base.js"
import ArMem from "./armem.js"

class AR extends BAR {
  constructor(opt = {}) {
    super({ ...opt, ArMem })
  }
}

export default AR
