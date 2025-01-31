import BAO from "./bao.js"
import { connect } from "./aoconnect-web.js"
import AR from "./war.js"

class AO extends BAO {
  constructor(opt = {}) {
    super({ ...opt, connect, AR })
  }
}

export default AO
