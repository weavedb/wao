import { useEffect, useRef } from "react"
import chalk from "chalk"
import { Box } from "@chakra-ui/react"
import { Terminal } from "@xterm/xterm"
import "@xterm/xterm/css/xterm.css"
import { FitAddon } from "/lib/addon-fit"
import XtermStyle from "/components/styles/XtermStyle"
import { k } from "/lib/utils"
import g from "/lib/global"

const config = {
  cursorBlink: true,
  fontSize: 12,
  fontFamily: "monospace",
  theme: { background: "#1E1E1E" },
  convertEol: true,
}

const init = g => {
  const elem = document.getElementById("terminal")
  if (!elem) return
  g.term = new Terminal(config)
  g.termRef.current = g.term
  g.fitAddon = new FitAddon()
  g.term.loadAddon(g.fitAddon)
  setTimeout(() => g.fitAddon.fit(), 100)
  g.term.open(elem)
  g.welcome = () => {
    const txt = chalk.green(
      "Welcome to WAO: Your web operating system for AO.\n"
    )
    g.term.write("\u001b[2K\r")
    g.term.write(`${txt}\n`)
  }

  g.term.write(`select a process...... `)
}

const toggle = async g => {
  const on = !g.dryrun
  g.setDryrun(on)
  await g.prompt("toggling dryrun mode...... " + (on ? "on" : "off"))
}

const aoeval = async (data, g) => {
  try {
    const jwk = await g.getWallet()
    //if (!jwk) return await g.prompt("wallet not found")
    const fn = g.dryrun ? "dry" : "msg"
    const { res, mid } = await g.ao[fn]({
      act: "Eval",
      pid: g.proc.id,
      data,
      jwk,
    })
    if (!g.dryrun) g.logMsg(mid)
    g.addMsg(mid)
    if (res?.Output?.data) {
      const data = res.Output.data.output ?? res.Output.data
      g.term.write(`${data}\r\n`)
    }
    if (res?.Error) g.term.write(`${res.Error}\r\n`)
    const prompt = res?.Output?.prompt ?? res?.Output?.data?.prompt
    if (prompt) {
      g.term.write(`${prompt}`)
      setTimeout(() => (g.plen = g.term.buffer.active.cursorX), 0)
    } else await g.prompt(false)
  } catch (e) {
    g.term.write(`${e.toString()}\r\n`)
    await g.prompt(false)
  }
}

const exec = async g => {
  if (!g.proc) return
  const cmd = g.inputRef.current.trim()
  if (cmd) {
    g.history.push(cmd)
    g.historyIndex = g.history.length
    g.savedInput = null
    g.inputRef.current = ""
    g.term.write("\r\n")
    switch (cmd) {
      case ".dryrun":
        await toggle(g)
        break
      default:
        await aoeval(cmd, g)
    }
    return true
  }
  return false
}

const term = g => {
  init(g)
  let on = false
  g.cur = 0
  g.history = []
  g.historyIndex = -1
  g.savedInput = null
  g.term.onData(async d => {
    if (on) return
    const code = d.charCodeAt(0)
    if (d === k.ctrlF) k.moveR()
    else if (d === k.altD) k.cutW()
    else if (d === k.ctrlD) k.delete()
    else if (d === k.ctrlB) k.moveL()
    else if (d === k.ctrlA) k.top()
    else if (d === k.ctrlE) k.end()
    else if (d === k.ctrlK) k.cut()
    else if (d === k.ctrlV) k.paste()
    else if (d === k.left()) k.moveL()
    else if (d === k.right()) k.moveR()
    else if (d === k.del) k.delete()
    else if (d === k.up()) k.upH()
    else if (d === k.down()) k.downH()
    else if (d.startsWith("\x1b")) return
    else if (code === k.enter) {
      on = true
      if (await exec(g)) g.cur = 0
      on = false
    } else if (code === k.bs) k.backspace()
    else if (code >= 32) {
      for (const ch of d) {
        const left = g.inputRef.current.slice(0, g.cur)
        const right = g.inputRef.current.slice(g.cur)
        g.inputRef.current = left + ch + right
        g.term.write(ch)
        g.cur++
        g.term.write(right)
        if (right.length > 0) g.term.write(k.left(right.length))
      }
    }
  })
}

export default function XTerm({ global: g }) {
  const termRef = useRef(null)
  const inputRef = useRef("")
  useEffect(() => {
    g.inputRef = inputRef
    g.termRef = termRef
    try {
      term(g)
    } catch (error) {
      console.error("Terminal initialization error:", error)
    }
    return () => {
      if (termRef.current) {
        termRef.current.dispose()
        termRef.current = null
      }
    }
  }, [])

  return <XtermStyle />
}
