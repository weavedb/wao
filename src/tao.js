import BAO from "./bao.js"
import { connect } from "./aoconnect.js"
import AR from "./tar.js"
import HB from "./hb.js"

class AO extends BAO {
  constructor(opt = {}) {
    super({ ...opt, connect, AR })
  }
}

export default AO
