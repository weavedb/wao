import chokidar from "chokidar"
import { generateId } from "./utils.js"
import WebSocket from "ws"
import yargs from "yargs"
let { port = 4006 } = yargs(process.argv.slice(2)).argv
const ws_server = new WebSocket.Server({ port })
import { keys, omit, isNil, mergeLeft } from "ramda"
import { resolve } from "path"
import { writeFileSync, readFileSync } from "fs"
const _dir = process.argv[2] ?? "./"
let sus = {}
let cbs = {}
let dir = null
let _socket = null
const cwd = process.cwd()
ws_server.on("connection", socket => {
  _socket = socket
  const clientId = generateId()
  console.log("new ws:", clientId)
  sus[clientId] = {
    socket,
    su: false,
    connections: {},
    address: null,
  }
  socket.send(JSON.stringify({ type: "registered", id: clientId, dir }))
  socket.on("message", message => {
    const data = JSON.parse(message)
    if (data.type === "save") {
      writeFileSync(
        resolve(cwd, _dir, data.path.replace(/^\//, "")),
        data.content
      )
    } else if (data.type === "data") {
      const content = readFileSync(
        resolve(cwd, _dir, data.path.replace(/^\//, "")),
        "utf8"
      )
      socket.send(
        JSON.stringify({
          content,
          type: "msg",
          path: data.path,
          subtype: "content",
        })
      )
    }
  })

  socket.on("close", () => {
    console.log("ws disconnected", clientId)
    delete sus[clientId]
  })
})
const sendDir = () => {
  if (_socket) {
    _socket.send(
      JSON.stringify({ type: "msg", subtype: "dir_change", dir: dir })
    )
  }
}
const sendContent = (content, path) => {
  if (_socket) {
    _socket.send(
      JSON.stringify({
        content,
        type: "msg",
        path,
        subtype: "content",
      })
    )
  }
}

console.log(`WAO FS running on port ${port}`)
const wd = resolve(cwd, _dir)
console.log("Directory:", wd)
chokidar
  .watch(wd, {
    ignored: (e, stats) => {
      const _e = resolve(cwd, e)
      return new RegExp(wd + "/node_modules").test(_e)
    },
  })
  .on("change", (e, p) => {
    const _e = resolve(cwd, e)
    const rel = _e.replace(new RegExp(wd + "/"), "")
    let paths = rel.split("/")
    let _dir = dir
    const content = readFileSync(resolve(cwd, _dir, _e), "utf8")
    sendContent(content, e.replace(wd, ""))
  })
  .on("add", (e, p) => {
    const _e = resolve(cwd, e)
    const rel = _e.replace(new RegExp(wd + "/"), "")
    let paths = rel.split("/")
    let _dir = dir
    while (paths.length > 0) {
      const p = paths.shift()
      if (paths.length === 0) {
        _dir[p] = Date.now()
      } else {
        _dir = _dir[p]
      }
    }
    sendDir()
  })
  .on("addDir", (e, p) => {
    const _e = resolve(cwd, e)
    if (_e === wd) {
      dir = {}
      return
    }
    const rel = _e.replace(new RegExp(wd + "/"), "")
    let paths = rel.split("/")
    let _dir = dir
    while (paths.length > 0) {
      const p = paths.shift()
      _dir[p] ??= {}
      _dir = _dir[p]
    }
    sendDir()
  })
  .on("unlink", (e, p) => {
    const _e = resolve(cwd, e)
    const rel = _e.replace(new RegExp(wd + "/"), "")
    let paths = rel.split("/")
    let _dir = dir
    while (paths.length > 0) {
      const p = paths.shift()
      if (paths.length === 0) {
        delete _dir[p]
      } else {
        _dir = _dir[p]
      }
    }
    sendDir()
  })
  .on("unlinkDir", (e, p) => {
    const _e = resolve(cwd, e)
    if (_e === wd) {
      dir = null
      return null
    }
    const rel = _e.replace(new RegExp(wd + "/"), "")
    let paths = rel.split("/")
    let _dir = dir
    while (paths.length > 0) {
      const p = paths.shift()
      if (paths.length === 0) {
        delete _dir[p]
      } else {
        _dir = _dir[p]
      }
    }
    sendDir()
  })
