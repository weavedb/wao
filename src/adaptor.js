import { connect } from "./aoconnect.js"
import { GQL, cu, su, mu } from "./test.js"
import Base from "./adaptor-base.js"

class Adaptor extends Base {
  constructor(obj) {
    super({ ...obj, GQL, cu, su, mu, connect })
  }
}
export default Adaptor
