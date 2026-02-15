import { AO } from "wao"
import { readFileSync, existsSync, readdirSync } from "fs"
import { resolve } from "path"

const args = process.argv.slice(2)
const mainnet = args.includes("--mainnet")
const localHb = args.includes("--local-hb")
const luaMode = args.includes("--lua")
const walletFlag = args.indexOf("--wallet")
let walletPath = walletFlag !== -1 ? args[walletFlag + 1] : null
const nodeFlag = args.indexOf("--node")
const nodeUrl = nodeFlag !== -1 ? args[nodeFlag + 1] : null
const luaFiles = args.filter(
  (a, i) =>
    !a.startsWith("--") &&
    args[i - 1] !== "--wallet" &&
    args[i - 1] !== "--node"
)

if (args.includes("--help") || args.includes("-h")) {
  console.log(`Usage: yarn deploy [options] [lua-files...]

Targets:
  (default)       AO testnet via aoconnect
  --mainnet       Remote HyperBEAM (push-1.forward.computer)
  --local-hb      Local HyperBEAM (localhost:10001)

Options:
  --lua           Use Lua mode (lua@5.3a) instead of genesis-wasm
  --node <url>    Custom HyperBEAM node URL
  --wallet <path> Path to Arweave JWK wallet
  --help          Show this help

Examples:
  yarn deploy                              # testnet, all src/*.lua
  yarn deploy src/counter.lua              # testnet, one script
  yarn deploy --mainnet                    # remote HB, all src/*.lua
  yarn deploy --mainnet --lua              # remote HB with Lua mode
  yarn deploy --local-hb                   # local HB (genesis-wasm)
  yarn deploy --local-hb --lua             # local HB (Lua mode)
  yarn deploy --node https://push-3.forward.computer  # custom node`)
  process.exit(0)
}

// Resolve wallet from .env.hyperbeam if not specified
if (!walletPath) {
  const envPath = resolve(process.cwd(), ".env.hyperbeam")
  if (existsSync(envPath)) {
    const env = readFileSync(envPath, "utf8")
    const match = env.match(/^CWD=(.+)$/m)
    if (match) {
      const hbWallet = resolve(process.cwd(), match[1].trim(), ".wallet.json")
      if (existsSync(hbWallet)) walletPath = hbWallet
    }
  }
}

// Fallback to local .wallet.json
if (!walletPath) {
  const localWallet = resolve(process.cwd(), ".wallet.json")
  if (existsSync(localWallet)) walletPath = localWallet
}

if (!walletPath) {
  console.log("Wallet not found. Run: yarn keygen")
  process.exit(1)
}

// If no files specified, deploy all src/*.lua
let targets = luaFiles
if (targets.length === 0) {
  const srcDir = resolve(process.cwd(), "src")
  if (existsSync(srcDir)) {
    targets = readdirSync(srcDir)
      .filter(f => f.endsWith(".lua"))
      .map(f => `src/${f}`)
  }
}

if (targets.length === 0) {
  console.log("No Lua scripts found in src/")
  process.exit(1)
}

const jwk = JSON.parse(readFileSync(resolve(process.cwd(), walletPath), "utf-8"))

// Determine AO config
let aoOpt = {}
let dest = "ao testnet"

if (localHb) {
  const url = nodeUrl || "http://localhost:10001"
  aoOpt = { hb: url }
  dest = `local HyperBEAM (${url})`
  if (luaMode) {
    aoOpt.mode = "lua"
    dest += " [lua]"
  }
} else if (mainnet) {
  const url = nodeUrl || "https://push-1.forward.computer"
  aoOpt = { hb: url }
  dest = `remote HyperBEAM (${url})`
  if (luaMode) {
    aoOpt.mode = "lua"
    dest += " [lua]"
  }
}

const ao = await new AO(aoOpt).init(jwk)

console.log(`Deploying ${targets.length} script${targets.length > 1 ? "s" : ""} to ${dest}...\n`)

for (const lua of targets) {
  const filePath = resolve(process.cwd(), lua)
  if (!existsSync(filePath)) {
    console.log(`  \u2718 ${lua} \u2014 file not found`)
    continue
  }
  const src_data = readFileSync(filePath, "utf-8")
  try {
    if (mainnet || localHb) {
      const { pid, err } = await ao.deploy({ src_data })
      if (err) throw typeof err === "string" ? new Error(err) : err
      console.log(`  \u2714 ${lua} \u2192 ${pid}`)
    } else {
      const { pid, err: spwnErr } = await ao.spwn()
      if (spwnErr) throw typeof spwnErr === "string" ? new Error(spwnErr) : spwnErr
      await ao.message({
        process: pid,
        signer: ao.toSigner(jwk),
        tags: [{ name: "Action", value: "Eval" }],
        data: src_data,
      })
      console.log(`  \u2714 ${lua} \u2192 ${pid}`)
      console.log(`    aolink: https://aolink.ar.io/#/entity/${pid}`)
    }
  } catch (e) {
    console.log(`  \u2718 ${lua} \u2014 ${e.message}`)
  }
}
