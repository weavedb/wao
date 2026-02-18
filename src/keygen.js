import _Arweave from "arweave"
const Arweave = _Arweave.default ?? _Arweave
import { toAddr } from "./utils.js"
import { mkdirSync, writeFileSync, existsSync, readdirSync } from "fs"
import { fileURLToPath } from "url"
import { dirname, resolve } from "path"

const __filename = fileURLToPath(import.meta.url)
const __dirname = dirname(__filename)
const walletsDir = resolve(__dirname, "../devnet/.wallets")
const names = ["su", "cu", "mu", "acc0", "acc1", "acc2"]

async function main() {
  const force = process.argv.includes("--force")

  if (!force && existsSync(walletsDir)) {
    const files = readdirSync(walletsDir).filter(f => f.endsWith(".json"))
    if (files.length > 0) {
      console.log("Wallets already exist in", walletsDir)
      console.log("Use --force to regenerate")
      return
    }
  }

  mkdirSync(walletsDir, { recursive: true })

  const arweave = Arweave.init({})

  console.log("Generating wallets...\n")

  for (const name of names) {
    const jwk = await arweave.wallets.generate()
    const addr = toAddr(jwk.n)
    writeFileSync(
      resolve(walletsDir, `${name}.json`),
      JSON.stringify(jwk, null, 2),
    )
    console.log(`  ${name.padEnd(5)} ${addr}`)
  }

  console.log(`\nWallets written to ${walletsDir}`)
}

main().catch(err => {
  console.error(err)
  process.exit(1)
})
