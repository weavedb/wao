import { useEffect, useRef } from "react"
import chalk from "chalk"
import { Box } from "@chakra-ui/react"
import { Terminal } from "@xterm/xterm"
import "@xterm/xterm/css/xterm.css"
import { FitAddon } from "../lib/addon-fit"
import XtermStyle from "./XtermStyle"
const config = {
  cursorBlink: true,
  fontSize: 12,
  fontFamily: "monospace",
  theme: { background: "#1E1E1E" },
  convertEol: true,
  //cols: 110,
  //rows: 11,
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
    const { res } = await g.ao[fn]({ act: "Eval", pid: g.proc.id, data, jwk })
    if (res?.Output?.data) {
      const data = res.Output.data.output ?? res.Output.data
      g.term.write(`${data}\r\n`)
    }
    if (res?.Error) g.term.write(`${res.Error}\r\n`)
    const prompt = res?.Output?.prompt ?? res?.Output?.data?.prompt
    if (prompt) g.term.write(`${prompt}`)
    else await g.prompt(false)
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

const updateInput = (g, newInput) => {
  const oldLen = g.inputRef.current.length
  const newLen = newInput.length
  const cur = g.cur
  if (cur > 0) g.term.write(`\x1b[${cur}D`)
  g.term.write(" ".repeat(oldLen))
  if (oldLen > 0) g.term.write(`\x1b[${oldLen}D`)
  g.inputRef.current = newInput
  g.cur = newLen
  g.term.write(newInput)
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
    if (d === "\x06") {
      if (g.cur < g.inputRef.current.length) {
        g.term.write("\x1b[C")
        g.cur++
      }
    } else if (d === "\x1bd") {
      // Alt-D
      if (g.cur < g.inputRef.current.length) {
        const isWordChar = c => /\w/.test(c)
        const text = g.inputRef.current
        let end = g.cur
        while (end < text.length && text[end] === " ") end++ // skip spaces
        const first = isWordChar(text[end])
        while (
          end < text.length &&
          (isWordChar(text[end]) === first || (!first && text[end] === " "))
        ) {
          end++
        }
        const left = text.slice(0, g.cur)
        const right = text.slice(end)
        g.inputRef.current = left + right
        g.term.write(right + " ".repeat(end - g.cur))
        g.term.write(`\x1b[${right.length + (end - g.cur)}D`)
      }
    } else if (d === "\x04") {
      if (g.cur < g.inputRef.current.length) {
        const left = g.inputRef.current.slice(0, g.cur)
        const right = g.inputRef.current.slice(g.cur + 1)
        g.inputRef.current = left + right
        g.term.write(right + " ")
        g.term.write(`\x1b[${right.length + 1}D`)
      }
    } else if (d === "\x02") {
      if (g.cur > 0) {
        g.term.write("\x1b[D")
        g.cur--
      }
    } else if (d === "\x01") {
      if (g.cur > 0) {
        g.term.write(`\x1b[${g.cur}D`)
        g.cur = 0
      }
    } else if (d === "\x05") {
      const move = g.inputRef.current.length - g.cur
      if (move > 0) {
        g.term.write(`\x1b[${move}C`)
        g.cur = g.inputRef.current.length
      }
    } else if (d === "\x0b") {
      const left = g.inputRef.current.slice(0, g.cur)
      const right = g.inputRef.current.slice(g.cur)
      g.inputRef.current = left

      g.term.write(" ".repeat(right.length))
      g.term.write(`\x1b[${right.length}D`)
    } else if (d === "\x16") {
      navigator.clipboard
        .readText()
        .then(paste => {
          for (const ch of paste) {
            const left = g.inputRef.current.slice(0, g.cur)
            const right = g.inputRef.current.slice(g.cur)
            g.inputRef.current = left + ch + right
            g.term.write(ch)
            g.cur++
            g.term.write(right)
            if (right.length > 0) g.term.write(`\x1b[${right.length}D`)
          }
        })
        .catch(err => {
          console.error("Clipboard read failed:", err)
        })
    } else if (d.startsWith("\x1b")) {
      if (d === "\x1b[D") {
        if (g.cur > 0) {
          g.term.write("\x1b[D")
          g.cur--
        }
      } else if (d === "\x1b[C") {
        if (g.cur < g.inputRef.current.length) {
          g.term.write("\x1b[C")
          g.cur++
        }
      } else if (d === "\x1b[A") {
        if (g.history.length > 0) {
          if (g.historyIndex === g.history.length) {
            g.savedInput = g.inputRef.current
          }
          if (g.historyIndex > 0) {
            g.historyIndex--
            updateInput(g, g.history[g.historyIndex])
          }
        }
      } else if (d === "\x1b[B") {
        if (g.historyIndex < g.history.length - 1) {
          g.historyIndex++
          updateInput(g, g.history[g.historyIndex])
        } else if (g.historyIndex === g.history.length - 1) {
          g.historyIndex++
          updateInput(g, g.savedInput ?? "")
          g.savedInput = null
        }
      } else if (d === "\x1b[3~") {
        if (g.cur < g.inputRef.current.length) {
          const left = g.inputRef.current.slice(0, g.cur)
          const right = g.inputRef.current.slice(g.cur + 1)
          g.inputRef.current = left + right
          g.term.write(right + " ")
          g.term.write(`\x1b[${right.length + 1}D`)
        }
      }
      return
    }

    const code = d.charCodeAt(0)
    if (code === 13) {
      on = true
      if (await exec(g)) g.cur = 0
      on = false
    } else if (code === 127) {
      if (g.cur > 0) {
        const left = g.inputRef.current.slice(0, g.cur - 1)
        const right = g.inputRef.current.slice(g.cur)
        g.inputRef.current = left + right
        g.cur--
        g.term.write("\b")
        g.term.write(right + " ")
        g.term.write(`\x1b[${right.length + 1}D`)
      }
    } else if (code < 32) {
      // Ignore other control characters
      return
    } else {
      for (const ch of d) {
        const left = g.inputRef.current.slice(0, g.cur)
        const right = g.inputRef.current.slice(g.cur)
        g.inputRef.current = left + ch + right
        g.term.write(ch)
        g.cur++
        g.term.write(right)
        if (right.length > 0) g.term.write(`\x1b[${right.length}D`)
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
