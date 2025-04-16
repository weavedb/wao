import Monaco from "@monaco-editor/react"
import lf from "localforage"
import { getPreview, filterFiles } from "/lib/utils"
import use from "/lib/use"
import { useRef, useEffect, useState } from "react"
import MonacoEditor from "/components/MonacoEditor"
import FilePath from "/components/FilePath"
import { Box, Flex, Icon } from "@chakra-ui/react"
import EditorScrollbarStyle from "/components/EditorScrollbarStyle"
import g from "/lib/global"
import { filter, map, includes } from "ramda"
import { FaX } from "react-icons/fa6"
import _assert from "assert"
import { DateMS, generateId } from "/lib/utils"
import { bfiles } from "/lib/guide"

export default function Editor({}) {
  const [tab, setTab] = use("tab")
  const [init, setInit] = use("init")
  const [previewContent, setPreviewContent] = use("previewContent")
  const [file, setFile] = use("file")
  const [projects, setProjects] = use("projects")
  const [preview, setPreview] = use("preview")
  const [openFiles, setOpenFiles] = use("openFiles")
  const [proc, setProc] = use("proc")
  const [files, setFiles] = use("files")
  const [tests, setTests] = use("tests")
  const [test, setTest] = use("test")
  const EditorBtns = () => (
    <Flex
      shrink="0"
      align="center"
      h="100%"
      overflowX="scroll"
      overflowY="hidden"
      className="editor-tabs"
      fontSize="11px"
    >
      {!file || file.ext !== "js" ? null : (
        <Flex
          mr={4}
          py={1}
          px={6}
          color="#5137C5"
          css={{
            borderRadius: "5px",
            cursor: "pointer",
            _hover: { opacity: 0.75 },
          }}
          onClick={async () => {
            try {
              const js = g.editorRef.current.getValue()
              const p = proc ? g.ao.p(proc.id) : null
              let descs = []
              const src = async path => {
                for (let v of files) {
                  if (v.name === path) {
                    return await lf.getItem(`file-${v.id}`)
                  }
                }
                return null
              }
              const assert = _assert
              const require = async name => {
                let module = { exports: null }
                const js = await src(name)
                eval(js)
                return module.exports
              }
              let i = 0
              const it = (desc, fn) => {
                descs[i].tests.push({ desc, fn })
              }

              const describe = (desc2, fn) => {
                descs.push({ desc: desc2, fn, tests: [] })
              }
              eval(js)
              const ts = DateMS.now()
              let success = 0
              let fail = 0
              let res = []
              for (let v of descs) {
                let _res = []
                let _success = 0
                let _fail = 0
                await v.fn({ require })
                for (let v2 of v.tests) {
                  const start = DateMS.now()
                  try {
                    await v2.fn({
                      ao: g.ao,
                      src,
                      p,
                    })
                    _res.push({
                      description: v2.desc,
                      success: true,
                      error: null,
                      duration: DateMS.now() - start,
                    })
                    _success++
                    success++
                  } catch (e) {
                    _res.push({
                      description: v2.desc,
                      success: false,
                      error: e.toString(),
                      duration: DateMS.now() - start,
                    })
                    _fail++
                    fail++
                  }
                  res.push({
                    description: v.desc,
                    cases: _res,
                    success: _success,
                    fail: _fail,
                  })
                }
                i++
              }
              const result = {
                file: file.name,
                id: generateId(),
                date: ts,
                duration: DateMS.now() - ts,
                tests: res,
                success,
                fail,
              }
              if (success > 0 || fail > 0) {
                setTab("Tests")
                setTest(result)
                setTests([result, ...tests])
              }
            } catch (e) {
              console.log(e)
            }
          }}
        >
          Test
        </Flex>
      )}
      {file?.ext !== "md" || file?.nodel ? null : (
        <Flex
          py={1}
          px={6}
          color="#5137C5"
          css={{
            borderRadius: "5px",
            cursor: "pointer",
            _hover: { opacity: 0.75 },
          }}
          onClick={async () => {
            if (!preview) {
              const txt = g.editorRef.current.getValue()
              setPreviewContent(await getPreview(txt))
            }
            setPreview(!preview)
          }}
        >
          {preview ? "Edit" : "Preview"}
        </Flex>
      )}
      {file?.local ? (
        <Flex
          py={1}
          px={6}
          color="#5137C5"
          css={{
            borderRadius: "5px",
            cursor: "pointer",
            _hover: { opacity: 0.75 },
          }}
          onClick={async () => {
            const content = g.editorRef.current.getValue()
            g.hub1.socket.send(
              JSON.stringify({ type: "save", content, path: file.filename })
            )
          }}
        >
          Save
        </Flex>
      ) : file?.nodel ? null : (
        <Flex
          py={1}
          px={4}
          color="#5137C5"
          css={{
            borderRadius: "5px",
            cursor: "pointer",
            _hover: { opacity: 0.75 },
          }}
          onClick={async () => {
            if (confirm("Would you like to delete the file?")) {
              g.editorRef.current.setValue("")
              await lf.removeItem(`file-${file.id}`)
              const _files = filter(v => file.id !== v.id)(files)
              const _openFiles = filter(v => file.id !== v.id)(openFiles)
              await lf.setItem(`files`, filterFiles(_files))
              setFiles(_files)
              setOpenFiles(_openFiles)
              let exists = false
              for (let v of _openFiles) {
                exists = true
                setFile(v)
                g.setType(v.ext)
                g.editorRef.current.setValue(
                  (await lf.getItem(`file-${v.id}`)) ?? ""
                )
                break
              }
              if (!exists) {
                setFile(bfiles[0])
                setOpenFiles([bfiles[0]])
                g.setType(bfiles[0].ext)
                fetch("/docs/README.md")
                  .then(r => r.text())
                  .then(txt => {
                    g.editorRef.current.setValue(txt)
                    setPreview(true)
                    getPreview(txt).then(setPreviewContent)
                  })
              }
            }
          }}
        >
          Delete
        </Flex>
      )}
    </Flex>
  )

  const EditorTabs = () => (
    <Flex
      flex={1}
      h="100%"
      overflowX="scroll"
      overflowY="hidden"
      className="editor-tabs"
    >
      <EditorScrollbarStyle />
      {map(v => {
        const open = v.id === file?.id
        return (
          <Flex
            fontSize="11px"
            w="140px"
            h="25px"
            bg={open ? "#1e1e1e" : "#444"}
            px={3}
            color="#c6c6c6"
            align="center"
            onClick={async () => {
              const txt = (await lf.getItem(`file-${v.id}`)) ?? ""
              setFile(v)
              g.setType(v.ext)
              // todo: handle this better
              if (v.ext === "md" && v.nodel) setPreview(true)
              if (v.ext === "md") setPreviewContent(await getPreview(txt))
              setTimeout(() => g.editorRef.current.setValue(txt), 100)
            }}
            css={{
              cursor: open ? "default" : "pointer",
              _hover: { opacity: open ? 1 : 0.75 },
            }}
          >
            <Flex
              color={
                v.ext === "lua"
                  ? "#7FDBFF"
                  : v.ext === "md"
                    ? "#39CCCC"
                    : includes(v.ext, ["js", "ts"])
                      ? "#FFDC00"
                      : "#3d9977"
              }
              mr={2}
              fontWeight="bold"
            >
              {v.ext === "lua"
                ? "Lua"
                : v.ext == "md"
                  ? "MD"
                  : v.ext == "js"
                    ? "JS"
                    : v.ext === "ts"
                      ? "TS"
                      : "{ }"}
            </Flex>
            <Box
              fontSize="11px"
              title={v.name}
              w="70px"
              css={{
                overflow: "hidden",
                whiteSpace: "nowrap",
                textOverflow: "ellipsis",
              }}
            >
              {v.name}
            </Box>
            <Box flex={1} />
            <Box>
              <Icon
                boxSize="10px"
                css={{
                  cursor: "pointer",
                  _hover: { opacity: 0.75, color: "#FFDC00" },
                }}
                onClick={async e => {
                  e.stopPropagation()
                  let opens = filter(v2 => v2.id !== v.id)(openFiles)
                  setOpenFiles(opens)
                  if (open) {
                    let exists = false
                    for (let v of opens) {
                      exists = true
                      setFile(v)
                      let txt = ""
                      if (v.fetch) {
                        txt = await fetch(v.fetch).then(r => r.text())
                      } else {
                        txt = (await lf.getItem(`file-${v.id}`)) ?? ""
                      }
                      g.setType(v.ext)
                      g.editorRef.current.setValue(txt)
                      if (v.ext === "md" && v.nodel) setPreview(true)
                      if (v.ext === "md" && (preview || v.nodel)) {
                        setPreviewContent(await getPreview(txt))
                      }

                      break
                    }
                    if (!exists) {
                      setFile(bfiles[0])
                      setOpenFiles([bfiles[0]])
                      g.setType(bfiles[0].ext)
                      fetch("/docs/README.md")
                        .then(r => r.text())
                        .then(txt => {
                          g.editorRef.current.setValue(txt)
                          setPreview(true)
                          getPreview(txt).then(setPreviewContent)
                        })
                    }
                  }
                }}
              >
                <FaX />
              </Icon>
            </Box>
          </Flex>
        )
      })(openFiles)}
    </Flex>
  )

  const isPreview = preview && file?.ext === "md"
  return !init ? null : (
    <Flex
      direction="column"
      w="100%"
      css={{ borderLeft: "1px solid #eee" }}
      h="100%"
    >
      <Flex align="center" color="#5137C5" w="100%">
        <EditorTabs />
        <EditorBtns />
      </Flex>
      <Flex
        bg="#1e1e1e"
        px={3}
        fontSize="10px"
        w="100%"
        h="25px"
        pb="5px"
        color="#999"
        align="center"
      >
        <FilePath {...{ file, projects }} />
      </Flex>
      <Flex w="100%" flex={1} css={{ overflowY: "auto" }}>
        {isPreview ? (
          <Flex justify="center" flex={1} h="100%" css={{ overflowY: "auto" }}>
            <Box w="100%">
              <Box
                p={6}
                w="100%"
                className="markdown-body"
                dangerouslySetInnerHTML={{ __html: previewContent }}
              />
            </Box>
          </Flex>
        ) : null}
        <Box
          display={isPreview ? "none" : "block"}
          w="100%"
          bg="#1E1E1E"
          h="100%"
        >
          <MonacoEditor />
        </Box>
      </Flex>
    </Flex>
  )
}
