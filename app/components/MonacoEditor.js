import Monaco from "@monaco-editor/react"
import lf from "localforage"
import { getPreview } from "/lib/utils"
import use from "/lib/use"
import { useRef, useEffect, useState } from "react"
import global from "/lib/global"

export default function MonacoEditor({}) {
  const [monaco, setMonaco] = useState(null)
  const [file] = use("file")
  const [tab] = use("tab")
  const [_, setPreviewContent] = use("previewContent")
  global.monacoRef = useRef(null)
  useEffect(() => {
    global.monacoRef.current = monaco
  }, [monaco])

  function handleEditorDidMount(editor, monaco) {
    global.editorRef.current = editor
    setMonaco(monaco)
    fetch("/docs/README.md")
      .then(r => r.text())
      .then(txt => {
        global.editorRef.current.setValue(txt)
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
          const lua = global.editorRef.current.getValue()
          await lf.setItem(`file-${file.id}`, lua)
        }
      }}
    />
  )
}
