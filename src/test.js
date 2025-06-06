import { readFileSync } from "fs"
import { resolve } from "path"
import { acc, mu, su, cu } from "./accounts.js"
import { connect } from "./aoconnect.js"
import AO from "./tao.js"
import AR from "./tar.js"
import GQL from "./tgql.js"
import ArMem from "./armem.js"
import { dirname, wait } from "./utils.js"
import { Testnet, Src, setup, ok, fail } from "./helpers.js"
import Server from "./server.js"
import Adaptor from "./adaptor.js"

const blueprint = async pkg => {
  return readFileSync(resolve(await dirname(), `lua/${pkg}.lua`), "utf8")
}
const scheduler = "_GQ33BkPtZrqxA84vM8Zk-N2aO0toNNu_C-l-rawrBA"
const HyperBEAM = "./hyperbeam.js"

export {
  wait,
  HyperBEAM,
  Adaptor,
  Testnet,
  Server,
  GQL,
  ArMem,
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
