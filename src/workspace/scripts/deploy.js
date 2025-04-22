import { AO } from "wao"
const lua = process.argv[2]

import { readFileSync } from "fs"
import { resolve } from "path"
import yargs from "yargs"

let { wallet = null } = yargs(process.argv.slice(2)).argv

let src_data = null
try {
  src_data = readFileSync(resolve(process.cwd(), lua), "utf-8")
} catch (e) {
  console.log("lua script not found")
  process.exit()
}

let jwk = null
try {
  jwk = JSON.parse(readFileSync(resolve(process.cwd(), wallet), "utf-8"))
} catch (e) {
  console.log("wallet not found")
  process.exit()
}

const deploy = async () => {
  const ao = await new AO().init(jwk)
  const { pid, p } = await ao.deploy({ src_data })
  console.log("process deployed!", pid)

  //console.log(await p.m("Inc", false))
  //console.log(await p.d("Get", false))
}

deploy()
