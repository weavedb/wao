import MAO from "./ao.js"
import { srcs } from "./utils.js"
import AR from "./ar.js"
import { connect } from "./aoconnect.js"

class AO extends MAO {
  constructor(opt = {}) {
    super({ ...opt, in_memory: true })
    this.in_memory = true
    const {
      accounts,
      modules,
      results,
      assign,
      result,
      message,
      spawn,
      dryrun,
      txs,
    } = connect()
    this.module = modules.aos_2_0_1
    this.assign = assign
    this.result = result
    this.results = results
    this.message = message
    this.spawn = spawn
    this.dryrun = dryrun
    this.ar.txs = txs
    this.accounts = accounts
  }

  async init(jwk) {
    if (!jwk && this.accounts?.[0]) jwk = this.accounts[0].jwk
    await this.ar.init(jwk)
    return this
  }
}

export default AO
