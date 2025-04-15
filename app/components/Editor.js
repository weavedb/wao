import Monaco from "@monaco-editor/react"
import lf from "localforage"
import { getPreview } from "/lib/utils"

export default function Editor({
  file,
  editorRef,
  tab,
  setMonaco,
  setPreviewContent,
}) {
  function handleEditorDidMount(editor, monaco) {
    editorRef.current = editor
    setMonaco(monaco)
    fetch("/docs/README.md")
      .then(r => r.text())
      .then(txt => {
        editorRef.current.setValue(txt)
        getPreview(txt).then(setPreviewContent)
      })
  }

  return (
    <Monaco
      height={tab !== "Projects" ? "100%" : "calc(100vh - 120px)"}
      width="100%"
      theme="vs-dark"
      defaultLanguage={file?.ext === "js" ? "js" : file?.ext ? file.ext : "lua"}
      onMount={handleEditorDidMount}
      onChange={async () => {
        if (file) {
          const lua = editorRef.current.getValue()
          await lf.setItem(`file-${file.id}`, lua)
        }
      }}
    />
  )
}
