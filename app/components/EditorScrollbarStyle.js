export default function EditorScrolbarStyle() {
  return (
    <style jsx global>{`
      .editor-tabs {
        scrollbar-width: thin;
        scrollbar-color: #444 #1e1e1e;
      }

      .editor-tabs::-webkit-scrollbar {
        width: 8px;
        height: 8px;
      }

      .editor-tabs::-webkit-scrollbar-track {
        background: #f1f1f1;
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
