import base from "./aoconnect-base.js"
import AR from "./war.js"
import { mu, su, cu, acc } from "./web.js"
import AoLoader from "./ao-loader.js"
import ArMem from "./armem-web.js"
const scheduler = "_GQ33BkPtZrqxA84vM8Zk-N2aO0toNNu_C-l-rawrBA"

export const connect = base({ AR, scheduler, mu, su, cu, acc, AoLoader, ArMem })
