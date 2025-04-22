#!/usr/bin/env node
import util from "node:util"
import { exec as _exec } from "node:child_process"
const exec = util.promisify(_exec)
import { cpSync, existsSync } from "fs"
import { resolve } from "path"
import { dirname } from "./utils.js"

const main = async () => {
  const appname = process.argv[2] ?? "waoapp"
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

main()
