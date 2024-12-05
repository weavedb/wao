import MAO from "./ao.js"
import { srcs } from "./utils.js"
import AR from "./tar.js"
import { connect } from "./aoconnect.js"
import { acc } from "./test.js"
class AO extends MAO {
  constructor(opt = {}) {
    super({ ...opt, in_memory: true })
    this.in_memory = true
    const {
      modules,
      results,
      assign,
      result,
      message,
      spawn,
      dryrun,
      monitor,
      unmonitor,
      mem,
    } = connect(opt.mem)
    this.module = mem.modules.aos2_0_1
    this.assign = assign
    this.result = result
    this.results = results
    this.message = message
    this.spawn = spawn
    this.dryrun = dryrun
    this.monitor = monitor
    this.unmonitor = unmonitor
    this.ar = new AR({ mem })
  }

  async init(jwk) {
    if (!jwk && this.acc[0]) jwk = this.acc[0].jwk
    await this.ar.init(jwk)
    return this
  }
}

export default AO
