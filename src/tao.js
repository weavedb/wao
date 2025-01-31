import BAO from "./bao.js"
import { connect } from "./aoconnect.js"
import AR from "./tar.js"

class AO extends BAO {
  constructor(opt = {}) {
    super({ ...opt, connect, AR })
  }
}

export default AO
