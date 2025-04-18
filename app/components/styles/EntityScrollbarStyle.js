export default function EntityScrollbarStyle() {
  return (
    <style jsx global>{`
      .editor-tabs {
        scrollbar-width: thin;
        scrollbar-color: #444 #5137c5;
      }

      .editor-tabs::-webkit-scrollbar {
        width: 8px;
        height: 8px;
      }

      .editor-tabs::-webkit-scrollbar-track {
        background: #5137c5;
        border-radius: 4px;
      }

      .editor-tabs::-webkit-scrollbar-thumb {
        background: #bbbbbb;
        border-radius: 4px;
      }

      .editor-tabs::-webkit-scrollbar-thumb:hover {
        background: #999999;
      }
    `}</style>
  )
}
