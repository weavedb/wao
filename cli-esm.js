#!/usr/bin/env node
import pm2 from "pm2"

const args = process.argv.slice(2)

pm2.connect(false, err => {
  if (err) {
    console.error("Error connecting to PM2:", err)
    process.exit(2)
  }

  pm2.start(
    {
      script: "./src/run.js",
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
    },
  )
  pm2.streamLogs("all", 0, false)
})

process.on("SIGINT", () => {
  pm2.delete("wao", err => {
    pm2.disconnect()
    process.exit(err ? 1 : 0)
  })
})
