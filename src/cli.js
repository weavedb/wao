#!/usr/bin/env node
import pm2 from "pm2"
import { dirname } from "./utils.js"
import { resolve } from "path"
const args = process.argv.slice(2)
import util from "node:util"
import { exec as _exec } from "node:child_process"
const exec = util.promisify(_exec)
import { cpSync, existsSync } from "fs"

const cmds = {
  wao: { script: "run.js" },
  hub: { script: "hub/index.js" },
  fs: { script: "hub/fs.js" },
  proxy: { script: "hub/ws-proxy.js" },
  create: { script: "create.js" },
}

const cmd = cmds[args[0]] ?? cmds["wao"]
const isCreate = args[0] === "create"
if (cmds[args[0]]) args.shift()

if (!cmd) {
  console.log("The wrong command")
  process.exit()
}

const create = async () => {
  const appname = args[0] ?? "waoapp"
  const appdir = resolve(process.cwd(), appname)
  if (existsSync(appdir)) return console.error(`appdir exists: ${appdir}`)
  const app = resolve(await dirname(), "workspace")
  try {
    cpSync(app, appdir, { recursive: true })
    const { error, stdout, stderr } = await exec(`cd ${appdir} && yarn`)
    if (error) {
      console.error(`something went wrong...`)
    } else {
      console.log(`${appname} successfully created!`)
    }
  } catch (e) {
    console.error(e)
  }
}

if (isCreate) create()
else {
  pm2.connect(false, async err => {
    if (err) {
      console.error("Error connecting to PM2:", err)
      process.exit(2)
    }

    pm2.start(
      {
        script: resolve(await dirname(), cmd.script),
        nodeArgs: "--experimental-wasm-memory64",
        instances: 1,
        force: true,
        args: args,
        daemon: false,
        name: "wao",
      },
      err => {
        if (err) {
          console.error("Error starting process:", err)
          pm2.disconnect()
          process.exit(2)
        }
      }
    )
    pm2.streamLogs("all", 0, false)
  })

  process.on("SIGINT", () => {
    pm2.delete("wao", err => {
      pm2.disconnect()
      process.exit(err ? 1 : 0)
    })
  })
}
