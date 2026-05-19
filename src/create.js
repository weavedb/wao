#!/usr/bin/env node
import util from "node:util"
import { exec as _exec } from "node:child_process"
const exec = util.promisify(_exec)
import { cpSync, existsSync, writeFileSync } from "fs"
import { resolve } from "path"
import { dirname } from "./utils.js"
import { createInterface } from "readline"
import Arweave from "arweave"

const dim = t => `\x1b[2m${t}\x1b[22m`
const bold = t => `\x1b[1m${t}\x1b[22m`
const cyan = t => `\x1b[36m${t}\x1b[39m`
const green = t => `\x1b[32m${t}\x1b[39m`
const red = t => `\x1b[31m${t}\x1b[39m`

const splash = () => {
  const logo = [
    "██╗    ██╗ █████╗  ██████╗ ",
    "██║    ██║██╔══██╗██╔═══██╗",
    "██║ █╗ ██║███████║██║   ██║",
    "██║███╗██║██╔══██║██║   ██║",
    "╚███╔███╔╝██║  ██║╚██████╔╝",
    " ╚══╝╚══╝ ╚═╝  ╚═╝ ╚═════╝ ",
  ]
  console.log()
  for (const line of logo) console.log(`  ${cyan(line)}`)
  console.log(`  ${dim("WizardAO SDK for Vibe Engineering")}`)
  console.log()
}

const tree = (title) => {
  const frames = ["◒", "◐", "◓", "◑"]
  const steps = []
  let active = null
  let intervalId = null
  let startTime = Date.now()

  const elapsed = (from) => {
    const ms = Date.now() - from
    return ms < 1000 ? `${ms}ms` : `${(ms / 1000).toFixed(1)}s`
  }

  const render = () => {
    const lines = []
    lines.push("")
    lines.push(`  ${bold(title)}`)
    lines.push("")
    for (let i = 0; i < steps.length; i++) {
      const s = steps[i]
      const isLast = i === steps.length - 1 && !active
      const branch = isLast ? "  └" : "  │"
      if (s.status === "done") {
        lines.push(`${branch}  ${green("✔")} ${s.label}`)
      } else if (s.status === "fail") {
        lines.push(`${branch}  ${red("✘")} ${red(s.label)}`)
      }
      if (s.children) {
        for (const c of s.children) {
          const childBranch = isLast ? "   " : "  │"
          lines.push(`${childBranch}    ${dim(c)}`)
        }
      }
    }
    if (active) {
      const frame = frames[active.tick++ % frames.length]
      lines.push(`  └  ${cyan(frame)} ${active.label}${dim("...")}`)
    }
    lines.push("")
    return lines.join("\n")
  }

  const clear = (lineCount) => {
    process.stdout.write(`\x1b[${lineCount}A\x1b[0J`)
  }

  let lastLineCount = 0

  const paint = () => {
    if (lastLineCount > 0) clear(lastLineCount)
    const output = render()
    process.stdout.write(output)
    lastLineCount = output.split("\n").length - 1
  }

  const startSpin = () => {
    paint()
    intervalId = setInterval(paint, 120)
  }

  const stopSpin = () => {
    if (intervalId) { clearInterval(intervalId); intervalId = null }
  }

  return {
    step: (label) => {
      stopSpin()
      if (active) {
        active.status = "done"
        steps.push(active)
      }
      active = { label, status: "active", tick: 0, start: Date.now(), children: null }
      startSpin()
    },

    sub: (text) => {
      if (active) {
        active.children = active.children || []
        active.children.push(text)
      }
      paint()
    },

    done: () => {
      stopSpin()
      if (active) { active.status = "done"; steps.push(active); active = null }
      paint()
    },

    fail: (label) => {
      stopSpin()
      if (active) { active.status = "fail"; if (label) active.label = label; steps.push(active); active = null }
      paint()
    },

    summary: (fn) => {
      stopSpin()
      if (active) { active.status = "done"; steps.push(active); active = null }
      paint()
      const total = elapsed(startTime)
      console.log(dim(`  Completed in ${total}`))
      console.log()
      fn()
    },
  }
}

const ask = (question) => new Promise(res => {
  const rl = createInterface({ input: process.stdin, output: process.stdout })
  rl.question(question, answer => { rl.close(); res(answer.trim().toLowerCase()) })
})

const HB_REPO = "https://github.com/permaweb/HyperBEAM.git"
const HB_TAG = "v0.9-FINAL"

const yellow = t => `\x1b[33m${t}\x1b[39m`

const which = async (cmd) => {
  try {
    const { stdout } = await exec(`which ${cmd} 2>/dev/null`)
    return stdout.trim()
  } catch { return null }
}

const getVersion = async (cmd, flag = "--version") => {
  try {
    const { stdout } = await exec(`${cmd} ${flag} 2>&1`)
    return stdout.trim().split("\n")[0]
  } catch { return null }
}

const detectGCC = async () => {
  for (const v of [14, 13, 12, 11]) {
    if (await which(`gcc-${v}`)) return v
  }
  if (await which("gcc")) return null
  return false
}

const detectHBEnv = async () => {
  const deps = {}
  const missing = []

  const erlPath = await which("erl")
  if (erlPath) {
    const ver = await getVersion("erl", "-eval '{ok, V} = file:read_file(filename:join([code:root_dir(), \"releases\", erlang:system_info(otp_release), \"OTP_VERSION\"])), io:fwrite(V), halt().' -noshell")
    deps.erl = { path: erlPath, version: ver }
  } else {
    missing.push("erlang (OTP 27+ required)")
  }

  const rebar3Path = await which("rebar3")
  if (rebar3Path) {
    deps.rebar3 = { path: rebar3Path }
  } else {
    missing.push("rebar3")
  }

  const gccVersion = await detectGCC()
  if (gccVersion === false) {
    missing.push("gcc")
  } else {
    deps.gcc = gccVersion
  }

  const cmakePath = await which("cmake")
  if (cmakePath) {
    deps.cmake = { path: cmakePath }
  } else {
    missing.push("cmake (>= 3.5)")
  }

  return { deps, missing }
}

const main = async () => {
  const appname = process.argv[2] ?? "waoapp"
  const appdir = resolve(process.cwd(), appname)
  if (existsSync(appdir)) return console.error(`\n  ${red("✘")} Directory already exists: ${bold(appdir)}\n`)
  const app = resolve(await dirname(), "workspace")

  console.log()
  console.log(`  ${bold("HyperBEAM:")}`)
  const col = 6 + HB_TAG.length
  const pad = " ".repeat(col - "Use existing".length)
  const pad2 = " ".repeat(col - "Skip".length)
  console.log(`    ${dim("1.")} Clone ${cyan(HB_TAG)}  ${dim("(from GitHub)")}`)
  console.log(`    ${dim("2.")} Use existing${pad}  ${dim("(path to local dir)")}`)
  console.log(`    ${dim("3.")} Skip${pad2}  ${dim("(in-memory AOS only)")}`)
  console.log()
  const hbChoice = await ask(`  Choose ${dim("(1/2/3)")} [1]: `)
  const hbMode = hbChoice === "2" ? "existing" : hbChoice === "3" ? "skip" : "clone"

  let hbPath = null
  if (hbMode === "existing") {
    hbPath = await ask(`  Path to HyperBEAM directory: `)
    const absPath = resolve(process.cwd(), hbPath)
    if (!existsSync(absPath)) {
      console.error(`\n  ${red("✘")} Directory not found: ${bold(absPath)}\n`)
      process.exit(1)
    }
    hbPath = absPath
  }

  console.log()
  console.log(`  ${bold("Frontend:")}`)
  console.log(`    ${dim("1.")} Skip             ${dim("(backend only)")}`)
  console.log(`    ${dim("2.")} Vite + wao/web   ${dim("(React SPA with ArConnect)")}`)
  console.log()
  const feChoice = await ask(`  Choose ${dim("(1/2)")} [1]: `)
  const withFrontend = feChoice === "2"

  console.log()

  const t = tree(`Creating ${cyan(appname)}`)

  try {
    t.step("Scaffolding project files")
    cpSync(app, appdir, { recursive: true, filter: (src) => {
      if (!withFrontend && src.includes("/frontend")) return false
      return true
    }})
    t.sub("src/counter.lua, token.lua, registry.lua")
    t.sub("test/ (aos, token, registry, hyperbeam)")
    t.sub("scripts/deploy.js")
    if (withFrontend) t.sub("frontend/ (Vite + React + wao/web)")
    t.done()

    t.step("Setting up Claude Code toolchain")
    const claudePath = await which("claude")
    if (claudePath) {
      const claudeVer = await getVersion("claude")
      t.sub(`${green("✔")} claude ${claudeVer || ""}`)
    } else {
      t.sub(`${red("✘")} claude — not found`)
    }
    t.sub("CLAUDE.md + docs/ (SDK, Lua, devices, debug) + agent teams")
    t.sub(".claude/rules/ (lua, testing, hyperbeam, deploy)")
    t.sub(".claude/skills/ (test, deploy, create-handler, create-device, debug, team)")
    t.sub(".claude/agents/ (builder, tester, device-builder)")
    writeFileSync(resolve(appdir, "CLAUDE.local.md"), `# Personal Preferences (auto-gitignored)\n# Uncomment and customize:\n\n# Preferred test file:\n# yarn test test/aos.test.js\n\n# HyperBEAM overrides:\n# HB_PORT=10001\n`)
    t.sub("CLAUDE.local.md (personal preferences)")
    t.done()

    t.step("Generating admin wallet")
    const arweave = Arweave.init()
    const jwk = await arweave.wallets.generate()
    const addr = await arweave.wallets.jwkToAddress(jwk)
    writeFileSync(resolve(appdir, ".wallet.json"), JSON.stringify(jwk))
    t.sub(`address: ${addr}`)
    t.done()

    t.step("Installing dependencies")
    const { error } = await exec(`cd ${appdir} && yarn --ignore-engines`)
    if (error) {
      t.fail("Failed to install dependencies")
      return
    }
    t.sub("root dependencies installed")
    await exec(`cd "${appdir}/dashboard" && npm install`)
    t.sub("dashboard dependencies installed")
    t.done()

    let hbWarnings = []

    if (hbMode !== "skip") {
      t.step("Detecting HyperBEAM environment")
      const { deps, missing } = await detectHBEnv()

      const hbCwd = hbMode === "existing" ? hbPath : "./HyperBEAM"
      const envLines = [`CWD=${hbCwd}`]

      if (deps.erl) t.sub(`${green("✔")} erlang ${deps.erl.version || ""}`)
      if (deps.rebar3) t.sub(`${green("✔")} rebar3`)
      if (deps.gcc) {
        t.sub(`${green("✔")} gcc-${deps.gcc}`)
        envLines.push(`CC=gcc-${deps.gcc}`)
        envLines.push(`CXX=g++-${deps.gcc}`)
      } else if (deps.gcc === null) {
        t.sub(`${yellow("~")} gcc found (no versioned binary, using default)`)
      }
      if (deps.cmake) {
        t.sub(`${green("✔")} cmake`)
        envLines.push(`CMAKE_POLICY_VERSION_MINIMUM=3.5`)
      }

      for (const m of missing) {
        t.sub(`${red("✘")} ${m} — not found`)
        hbWarnings.push(m)
      }

      writeFileSync(resolve(appdir, ".env.hyperbeam"), envLines.join("\n") + "\n")
      t.sub(`.env.hyperbeam → CWD=${hbCwd}`)
      t.done()

      if (hbMode === "clone") {
        t.step("Cloning HyperBEAM")
        t.sub(`tag: ${HB_TAG}`)
        await exec(`cd "${appdir}" && git clone --depth 1 --branch ${HB_TAG} ${HB_REPO} HyperBEAM`, { maxBuffer: 50 * 1024 * 1024, timeout: 600000 })
        t.done()

        if (deps.erl && deps.rebar3 && missing.length === 0) {
          t.step("Compiling HyperBEAM (with genesis-wasm)")
          try {
            const envParts = []
            if (deps.gcc) { envParts.push(`CC=gcc-${deps.gcc}`, `CXX=g++-${deps.gcc}`) }
            if (deps.cmake) { envParts.push(`CMAKE_POLICY_VERSION_MINIMUM=3.5`) }
            const envPrefix = envParts.join(" ")
            await exec(`cd "${appdir}/HyperBEAM" && ${envPrefix} rebar3 as genesis_wasm compile`, { maxBuffer: 50 * 1024 * 1024, timeout: 600000 })
            t.sub("genesis-wasm device compiled")
            t.done()
          } catch (e) {
            t.fail("Compile failed — check .env.hyperbeam and run manually")
          }
        } else if (missing.length > 0) {
          t.step("Skipping compilation (missing dependencies)")
          t.sub("Run 'cd HyperBEAM && rebar3 compile' after installing dependencies")
          t.done()
        }
      }

      // Generate wallet in HyperBEAM directory
      const hbWalletDir = resolve(appdir, hbCwd)
      t.step("Generating HyperBEAM wallet")
      const hbArweave = Arweave.init()
      const hbJwk = await hbArweave.wallets.generate()
      const hbAddr = await hbArweave.wallets.jwkToAddress(hbJwk)
      writeFileSync(resolve(hbWalletDir, ".wallet.json"), JSON.stringify(hbJwk))
      t.sub(`address: ${hbAddr}`)
      t.done()
    }

    if (withFrontend) {
      t.step("Setting up frontend")
      t.sub("Vite + React + wao/web")
      t.sub("vitest unit tests + Playwright E2E")
      t.done()
    }

    t.summary(() => {
      splash()
      console.log(`  ${green("🎉")} ${bold(appname)} is ready!`)

      if (!claudePath) {
        console.log()
        console.log(`  ${yellow("⚠")}  ${bold("Claude Code CLI not found")}`)
        console.log(`     WAO uses Claude Code for AI-powered builds, tests, and deployment.`)
        console.log()
        console.log(`     ${bold("Install:")}`)
        console.log(`       ${dim("$")} npm install -g @anthropic-ai/claude-code`)
        console.log()
        console.log(`     ${dim("Then run")} ${cyan("claude")} ${dim("inside your project to start building.")}`)
      }

      if (hbWarnings.length > 0) {
        console.log()
        console.log(`  ${yellow("⚠")}  ${bold("Missing HyperBEAM dependencies:")}`)
        for (const w of hbWarnings) console.log(`     ${dim("•")} ${w}`)
        console.log(`     ${dim("Install these before running HyperBEAM tests")}`)
      }

      console.log()
      console.log(`  ${bold("1. Build")}  ${dim("claude")}`)
      console.log()
      console.log(`    ${dim("$")} cd ${appname}`)
      console.log(`    ${dim("$")} ${bold(`claude`)}`)
      console.log(`    ${dim("Tell the agent what to build, or run")} ${cyan("/build")} ${dim("for the full workflow.")}`)
      console.log()
      console.log(`    ${dim("Quality gates (enforced automatically):")}`)
      console.log(`      ${dim("① Unit tests        in-memory AOS (mainnet WASM device)")}`)
      if (hbMode !== "skip") console.log(`      ${dim("② Integration test  HyperBEAM (requires Erlang)")}`)
      if (withFrontend) console.log(`      ${dim(`${hbMode !== "skip" ? "③" : "②"} Frontend tests    vitest + Playwright`)}`)
      console.log(`    ${dim("Agent cannot complete tasks until all gates pass.")}`)
      console.log()
      console.log(`  ${bold("2. Deploy to mainnet")}`)
      console.log()
      console.log(`    ${dim("$")} yarn deploy src/counter.lua --wallet .wallet.json`)
      console.log()
    })
  } catch (e) {
    t.fail(e.message)
  }
}

main()
