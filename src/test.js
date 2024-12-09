import { readFileSync } from "fs"
import { resolve } from "path"
import { acc, mu, su, cu } from "./accounts.js"
import { connect } from "./aoconnect.js"
import AO from "./tao.js"
import AR from "./tar.js"
import { dirname } from "./utils.js"
import { Src, setup, ok, fail } from "./helpers.js"
const blueprint = async pkg => {
  return readFileSync(resolve(await dirname(), `lua/${pkg}.lua`), "utf8")
}
const scheduler = "GQ33BkPtZrqxA84vM8Zk-N2aO0toNNu_C-l-rawrBA"

export {
  AO,
  AR,
  connect,
  acc,
  mu,
  su,
  cu,
  blueprint,
  scheduler,
  ok,
  fail,
  Src,
  setup,
}
