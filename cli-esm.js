#!/usr/bin/env node
import pm2 from "pm2"

const args = process.argv.slice(2)

const cmds = {
  wao: { script: "./src/run.js" },
  hub: { script: "./src/hub/index.js" },
  fs: { script: "./src/hub/fs.js" },
  proxy: { script: "./src/hub/ws-proxy.js" },
}

const cmd = cmds[args[0]] ?? cmds["wao"]
if (cmds[args[0]]) args.shift()

if (!cmd) {
  console.log("The wrong command")
  process.exit()
}

pm2.connect(false, err => {
  if (err) {
    console.error("Error connecting to PM2:", err)
    process.exit(2)
  }

  pm2.start(
    {
      script: cmd.script,
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
