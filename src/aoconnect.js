import base from "./aoconnect-base.js"
import AR from "./tar.js"
import { mu, su, cu, acc } from "./test.js"
import AoLoader from "@permaweb/ao-loader"
import ArMem from "./armem.js"
const scheduler = "_GQ33BkPtZrqxA84vM8Zk-N2aO0toNNu_C-l-rawrBA"

export const connect = base({ AR, scheduler, mu, su, cu, acc, AoLoader, ArMem })
