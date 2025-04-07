import { useEffect, useRef } from "react"
import { Box } from "@chakra-ui/react"
import { Terminal } from "@xterm/xterm"
import "@xterm/xterm/css/xterm.css"

export default function XTerm() {
  const terminalRef = useRef(null)
  const inputRef = useRef("")

  useEffect(() => {
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
      const term = new Terminal({
        cursorBlink: true,
        fontSize: 14,
        fontFamily: "monospace",
        theme: {
          background: "#1E1E1E",
          foreground: "#f0f0f0",
        },
        rows: 10, // More rows to fill the space
        cols: 80,
      })

      terminalRef.current = term
      term.open(terminalElement)
      term.write("wao $ ")

      let processingCommand = false

      term.onData(data => {
        if (processingCommand) return

        const code = data.charCodeAt(0)

        if (code === 13) {
          // Enter key
          processingCommand = true

          const command = inputRef.current
          inputRef.current = ""

          term.write("\r\n")

          if (command.trim()) {
            term.write(`Command not found: ${command}\r\n`)
          }

          term.write("wao $ ")
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

  return <Box w="100%" h="200px" id="terminal" bg="#1E1E1E" borderRadius="0" />
}
