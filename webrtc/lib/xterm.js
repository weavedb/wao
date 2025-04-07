import { useEffect, useRef } from "react"
import { Box } from "@chakra-ui/react"
import { Terminal } from "@xterm/xterm"
import "@xterm/xterm/css/xterm.css"

let term = null
let count = 0
export default function XTerm({ global, ao }) {
  const terminalRef = useRef(null)
  const inputRef = useRef("")
  useEffect(() => {
    global.inputRef = inputRef
    let dryrun = false
    // Add scrollbar styles as a separate element to ensure they apply
    const styleElement = document.createElement("style")
    styleElement.textContent = `
      /* Custom scrollbar for terminal */
      #terminal .xterm-viewport::-webkit-scrollbar {
        width: 10px;
        background-color: #111;
      }
      
      #terminal .xterm-viewport::-webkit-scrollbar-thumb {
        background-color: #444;
        border-radius: 4px;
      }
      
      #terminal .xterm-viewport::-webkit-scrollbar-track {
        background-color: #222;
      }
      
      #terminal .xterm-viewport {
        scrollbar-width: thin;
        scrollbar-color: #444 #222;
      }
      
      /* Make terminal use full available height */
      #terminal .xterm {
        height: 200px !important;
      }
      
      #terminal .xterm-screen {
        height: 100% !important;
      }
      #terminal .xterm-rows{
        padding: 5px 10px;
      }
    `
    document.head.appendChild(styleElement)
    try {
      const terminalElement = document.getElementById("terminal")
      if (!terminalElement) return

      // Create terminal with more rows to fill the space
      /*term = new Terminal({
        cursorBlink: true,
        fontSize: 14,
        fontFamily: "monospace",
        theme: {
          background: "#1E1E1E",
          foreground: "#f0f0f0",
        },
        cols: 80,
        })*/

      term = new Terminal({
        cursorBlink: true,
        fontSize: 12,
        fontFamily: "monospace",
        theme: { background: "#1E1E1E" },
        convertEol: true,
        cols: 110,
        rows: 11,
      })
      global.term = term
      terminalRef.current = term
      global.terminalRef = terminalRef
      term.open(terminalElement)
      term.write(`select a process...... `)

      let processingCommand = false

      term.onData(async data => {
        if (processingCommand) return

        const code = data.charCodeAt(0)

        if (code === 13) {
          if (!global.proc) return
          // Enter key
          processingCommand = true

          const command = inputRef.current
          inputRef.current = ""

          term.write("\r\n")

          if (command.trim()) {
            if (command === ".dryrun") {
              const on = !global.dryrun
              global.setDryrun(on)
              await global.prompt(
                "toggling dryrun mode...... " + (on ? "on" : "off")
              )
            } else {
              try {
                const { res } = await ao[global.dryrun ? "dry" : "msg"]({
                  act: "Eval",
                  pid: global.proc.id,
                  data: command,
                })
                console.log(res)
                if (res?.Output?.data) {
                  const data = res.Output.data.output ?? res.Output.data
                  term.write(`${data}\r\n`)
                }
                if (res?.Error) term.write(`${res.Error}\r\n`)
                const prompt = res?.Output?.prompt ?? res?.Output?.data?.prompt
                if (prompt) {
                  term.write(`${prompt}`)
                } else {
                  await global.prompt(false)
                }
              } catch (e) {
                term.write(`${e.toString()}\r\n`)
                await global.prompt(false)
              }
            }
          } else {
            await global.prompt(false)
          }
          processingCommand = false
        } else if (code === 127) {
          // Backspace
          if (inputRef.current.length > 0) {
            term.write("\b \b")
            inputRef.current = inputRef.current.substring(
              0,
              inputRef.current.length - 1
            )
          }
        } else if (code < 32) {
          // Ignore other control characters
          return
        } else {
          term.write(data)
          inputRef.current += data
        }
      })
    } catch (error) {
      console.error("Terminal initialization error:", error)
    }

    return () => {
      document.head.removeChild(styleElement)
      if (terminalRef.current) {
        terminalRef.current.dispose()
        terminalRef.current = null
      }
    }
  }, [])

  return null
}
