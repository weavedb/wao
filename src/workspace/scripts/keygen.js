#!/usr/bin/env node
import { readFileSync, writeFileSync, existsSync } from "fs"
import { resolve } from "path"
import Arweave from "arweave"

const dim = t => `\x1b[2m${t}\x1b[22m`
const bold = t => `\x1b[1m${t}\x1b[22m`
const green = t => `\x1b[32m${t}\x1b[39m`
const red = t => `\x1b[31m${t}\x1b[39m`
const cyan = t => `\x1b[36m${t}\x1b[39m`

const root = process.cwd()

const main = async () => {
  console.log()
  console.log(`  ${bold("keygen")} — generate HyperBEAM .wallet.json`)
  console.log()

  const envPath = resolve(root, ".env.hyperbeam")
  if (!existsSync(envPath)) {
    console.log(`  ${red("✘")} .env.hyperbeam not found`)
    console.log(`    ${dim("Run")} npx wao create ${dim("with HyperBEAM enabled")}`)
    console.log()
    process.exit(1)
  }

  const env = readFileSync(envPath, "utf8")
  const match = env.match(/^CWD=(.+)$/m)
  if (!match) {
    console.log(`  ${red("✘")} No CWD in .env.hyperbeam`)
    console.log()
    process.exit(1)
  }

  const hbDir = resolve(root, match[1].trim())
  if (!existsSync(hbDir)) {
    console.log(`  ${red("✘")} HyperBEAM directory not found: ${hbDir}`)
    console.log()
    process.exit(1)
  }

  const dest = resolve(hbDir, ".wallet.json")
  const arweave = Arweave.init()
  const jwk = await arweave.wallets.generate()
  const addr = await arweave.wallets.jwkToAddress(jwk)
  writeFileSync(dest, JSON.stringify(jwk))
  console.log(`  ${green("✔")} ${dest}`)
  console.log(`    ${dim("address:")} ${cyan(addr)}`)
  console.log()
}

main()
