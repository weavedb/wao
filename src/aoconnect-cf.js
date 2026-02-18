import "./cf-env.js"
import base from "./aoconnect-base.js"
import AR from "./car.js"
import { mu, su, cu, acc } from "./cf.js"
import AoLoader from "./ao-loader.js"
import ArMem from "./armem-cf.js"
const scheduler = "_GQ33BkPtZrqxA84vM8Zk-N2aO0toNNu_C-l-rawrBA"

export const connect = base({ AR, scheduler, mu, su, cu, acc, AoLoader, ArMem })
