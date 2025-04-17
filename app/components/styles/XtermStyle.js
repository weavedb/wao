export default function XtermStyle() {
  return (
    <style jsx global>{`
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
      #terminal .xterm-rows {
        padding: 5px 10px;
      }
    `}</style>
  )
}
