import { connect } from "./aoconnect-web.js"
import { cu, su, mu } from "./accounts-web.js"
import GQL from "./tgql.js"
import Base from "./adaptor-base.js"

class Adaptor extends Base {
  constructor(obj) {
    super({ ...obj, GQL, cu, su, mu, connect })
  }
}
export default Adaptor
