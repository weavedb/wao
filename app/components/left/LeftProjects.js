import use from "/lib/use"
import { Box, Flex, Icon } from "@chakra-ui/react"
import { Tooltip } from "@/components/ui/tooltip"
import Hub from "/lib/hub"

import {
  FaFileCirclePlus,
  FaRegSquarePlus,
  FaFolderPlus,
  FaAngleDown,
  FaRegFolder,
  FaRegFolderOpen,
  FaRegFileCode,
  FaAngleRight,
} from "react-icons/fa6"
import g from "/lib/global"
import { filter, clone, indexBy, prop, prepend, map, compose } from "ramda"
import { generateId, getPreview, filterFiles, filterProjects } from "/lib/utils"
import lf from "localforage"
import md5 from "md5"

export default function Left() {
  const [files, setFiles] = use("files")
  const [file, setFile] = use("file")
  const [wsid, setWSID] = use("wsid")
  const [projects, setProjects] = use("projects")
  const [localFS, setLocalFS] = use("localFS")
  const [localFSOpen, setLocalFSOpen] = use("localFSOpen")
  const [modal, setModal] = use("modal")
  const [modal2, setModal2] = use("modal2")
  const [modal3, setModal3] = use("modal3")
  const [modal4, setModal4] = use("modal4")
  const [openFiles, setOpenFiles] = use("openFiles")
  const [preview, setPreview] = use("preview")
  const [previewContent, setPreviewContent] = use("previewContent")

  const _projects = [
    ...(localFS.length > 0
      ? [{ id: "2", name: "Local Computer", open: localFSOpen }]
      : []),
    ...projects,
  ]

  const handleImportClick = () => {
    g.fileInputRef.current.click()
  }
  const handleFileChange = event => {
    const file = event.target.files[0]
    if (file) {
      const reader = new FileReader()
      reader.onload = async e => {
        const txt = e.target.result
        const id = generateId()
        const fileext = file.name.split(".").pop().toLowerCase()
        const _file = {
          name: file.name,
          update: Date.now(),
          id,
          ext: fileext,
        }
        const _files = prepend(_file, files)
        console.log(_files)
        await lf.setItem("files", filterFiles(_files))
        await lf.setItem(`file-${id}`, txt)
        setFiles(_files)
        setFile(_file)
        event.target.value = ""
        g.setType(fileext.ext)
        // todo: handle this better
        setTimeout(() => g.editorRef.current.setValue(txt), 100)
      }

      reader.readAsText(file)
    }
  }
  const pfiles = g.getFiles()
  const buttons = (
    <Flex
      h="60px"
      align="center"
      px={3}
      fontSize="12px"
      css={{ borderBottom: "1px solid #ddd" }}
    >
      <>
        <Tooltip
          content={"Create Project"}
          positioning={{ placement: "bottom-end" }}
          openDelay={0}
          closeDelay={0}
        >
          <Flex
            py={1}
            pr={2}
            fontSize="12px"
            color="#5137C5"
            css={{
              borderRadius: "5px",
              cursor: "pointer",
              _hover: { opacity: 0.75 },
            }}
            onClick={async () => {
              setModal3(true)
            }}
          >
            <Icon size="md">
              <FaRegSquarePlus />
            </Icon>
          </Flex>
        </Tooltip>
        <Tooltip
          content={"Create Folder"}
          positioning={{ placement: "bottom-end" }}
          openDelay={0}
          closeDelay={0}
        >
          <Flex
            py={1}
            px={2}
            fontSize="12px"
            color="#5137C5"
            css={{
              borderRadius: "5px",
              cursor: "pointer",
              _hover: { opacity: 0.75 },
            }}
            onClick={async () => {
              setModal4(true)
            }}
          >
            <Icon size="md">
              <FaFolderPlus />
            </Icon>
          </Flex>
        </Tooltip>
        <Tooltip
          content={"Create File"}
          positioning={{ placement: "bottom-end" }}
          openDelay={0}
          closeDelay={0}
        >
          <Flex
            py={1}
            px={2}
            fontSize="12px"
            color="#5137C5"
            css={{
              borderRadius: "5px",
              cursor: "pointer",
              _hover: { opacity: 0.75 },
            }}
            onClick={async () => {
              setModal(true)
            }}
          >
            <Icon size="md">
              <FaFileCirclePlus />
            </Icon>
          </Flex>
        </Tooltip>
        <Box flex={1} />

        <Flex
          ml={2}
          py={1}
          px={3}
          fontSize="10px"
          color="#ddd"
          bg="#5137C5"
          css={{
            borderRadius: "5px",
            cursor: "pointer",
            _hover: { opacity: 0.75 },
          }}
          onClick={handleImportClick}
        >
          <input
            type="file"
            ref={g.fileInputRef}
            onChange={handleFileChange}
            accept=".lua, .js, .json, .md, .ts"
            style={{ display: "none" }}
          />
          Import
        </Flex>
        {wsid ? (
          <Flex
            ml={3}
            fontSize="10px"
            bg="white"
            color="#5137C5"
            py={1}
            px={3}
            css={{
              border: "1px solid #5137C5",
              borderRadius: "5px",
              cursor: "pointer",
              _hover: { opacity: 0.75 },
            }}
            onClick={() => {
              if (confirm("Disconnect from WAO FS?")) {
                g.hub1.disconnect()
                setWSID(null)
              }
            }}
          >
            FS : 9090
          </Flex>
        ) : (
          <Flex
            ml={3}
            fontSize="10px"
            color="#ddd"
            bg="#5137C5"
            py={1}
            px={3}
            css={{
              borderRadius: "5px",
              cursor: "pointer",
              _hover: { opacity: 0.75 },
            }}
            onClick={async () => {
              const getP = v => {
                if (v.p.length === 0) {
                  return "/" + v.name
                } else {
                  return "/" + v.p.join("/") + "/" + v.name
                }
              }
              const dirmap = indexBy(prop("id"), localFS)
              const updateDir = _localFS => {
                const _files = []
                const ls = (fs, p = [], dpath = []) => {
                  for (let k in fs) {
                    const path = `/${p.length === 0 ? "" : p.join("/") + "/"}`
                    const _dpath = `/${dpath.length === 0 ? "" : dpath.join("/") + "/"}`
                    const id = md5(
                      `2${_dpath}${k}`.replace(new RegExp("/", "g"), "#")
                    )
                    _files.push({
                      dir: typeof fs[k] === "object",
                      open: dirmap[id]?.open ?? false,
                      name: k,
                      pid: "2",
                      p,
                      ext:
                        typeof fs[k] === "object" ? null : k.split(".").pop(),
                      id,
                      path,
                      local: true,
                      filename: `${_dpath}${k}`,
                    })
                    if (typeof fs[k] === "object")
                      ls(fs[k], [...p, id], [...dpath, k])
                  }
                }
                ls(_localFS)
                setLocalFS(_files)
              }
              g.hub1 = new Hub("ws://localhost:9090")
              g.hub1.onMsg = async obj => {
                console.log("New FS Msg:", obj)
                if (obj.subtype === "content") {
                  const ext = obj.path.split(".").pop()
                  const file = g.fileRef.current
                  const id = md5(
                    `2${obj.path}`.replace(new RegExp("/", "g"), "#")
                  )
                  await lf.setItem(`file-${id}`, obj.content)
                  if (file?.id === id) {
                    setTimeout(() => {
                      g.setType(ext)
                      g.editorRef.current.setValue(obj.content)
                    }, 100)
                  }
                } else if (obj.subtype === "dir_change") {
                  updateDir(obj.dir)
                }
              }
              g.hub1.onClose = () => setWSID(null)

              g.hub1.onRegister = msg => {
                setWSID(msg.id)
                let lfs = null
                updateDir(msg.dir)
              }
              g.hub1.connect()
            }}
          >
            Local FS
          </Flex>
        )}
      </>
    </Flex>
  )
  return (
    <Box
      fontSize="12px"
      w="315px"
      h="100%"
      css={{ borderRight: "1px solid #ddd", overflowY: "auto" }}
    >
      {buttons}
      {map(v => {
        return (
          <>
            <Flex
              h="30px"
              px={4}
              align="center"
              bg="#eee"
              onClick={async () => {
                if (v.id === "2") {
                  setLocalFSOpen(!localFSOpen)
                  return
                }
                const pr = clone(projects)
                for (let v2 of pr) {
                  if (v.id === v2.id) v2.open = !v2.open
                }
                setProjects(pr)
                await lf.setItem("projects", filterProjects(pr))
              }}
              css={{ cursor: "pointer", _hover: { opacity: 0.75 } }}
            >
              <Icon size="sm" color="#5137C5" mr={2}>
                {!v.open ? <FaAngleRight /> : <FaAngleDown />}
              </Icon>
              <Box>{v.name}</Box>
              <Box flex={1} />
            </Flex>
            {!v.open
              ? null
              : compose(
                  map(v =>
                    !v.show ? null : (
                      <>
                        <Flex
                          h="30px"
                          px={4}
                          align="center"
                          bg={v.id === file?.id ? "#5137C5" : "white"}
                          color={v.id === file?.id ? "#ddd" : "#222"}
                          css={{
                            cursor: "pointer",
                            _hover: { opacity: 0.75 },
                          }}
                          onClick={async () => {
                            if (v.dir) {
                              if (v.pid === "2") {
                                let opens = clone(localFS)
                                for (let v2 of opens) {
                                  if (v2.id === v.id) {
                                    v2.open = !v2.open
                                    break
                                  }
                                }
                                setLocalFS(opens)
                              } else {
                                let opens = clone(files)
                                for (let v2 of opens) {
                                  if (v2.id === v.id) {
                                    v2.open = !v2.open
                                    break
                                  }
                                }
                                setFiles(opens)
                              }
                            } else {
                              setFile(v)
                              let opens = clone(openFiles)
                              let exists = false
                              for (let v2 of openFiles) {
                                if (v2.id === v.id) {
                                  exists = true
                                  break
                                }
                              }
                              if (!exists) {
                                opens.push(v)
                                setOpenFiles(opens)
                              }
                              g.setType(v.ext)
                              if (v.pid === "2") {
                                g.hub1.socket.send(
                                  JSON.stringify({
                                    type: "data",
                                    path: v.filename,
                                  })
                                )
                              } else {
                                let txt = ""
                                if (v.fetch) {
                                  txt = await fetch(v.fetch).then(r => r.text())
                                } else {
                                  txt = (await lf.getItem(`file-${v.id}`)) ?? ""
                                }
                                setTimeout(
                                  () => g.editorRef.current.setValue(txt),
                                  100
                                )
                                if (v.ext === "md" && preview) {
                                  setPreviewContent(await getPreview(txt))
                                }
                              }
                            }
                          }}
                        >
                          <Box pl={20 * (v.p.length + 1) + "px"} />
                          <Icon
                            size="sm"
                            mr={2}
                            color={v.id === file?.id ? "#ddd" : "#5137C5"}
                          >
                            {v.dir ? (
                              v.open ? (
                                <FaRegFolderOpen />
                              ) : (
                                <FaRegFolder />
                              )
                            ) : (
                              <FaRegFileCode />
                            )}
                          </Icon>
                          <Box>{v.name}</Box>
                        </Flex>
                      </>
                    )
                  ),
                  filter(v2 => v2.pid === v.id)
                )(pfiles[v.id] ?? [])}
          </>
        )
      })(_projects)}
    </Box>
  )
}
