import use from "/lib/use"
import _assert from "assert"
import { DataItem } from "arbundles"
import { Box, Flex, Icon } from "@chakra-ui/react"
import { Tooltip } from "@/components/ui/tooltip"
import Hub from "../lib/hub"
import { Spinner } from "@chakra-ui/react"

import {
  FaNetworkWired,
  FaFileCirclePlus,
  FaRegSquarePlus,
  FaFolderPlus,
  FaAngleDown,
  FaRegFolder,
  FaRegFolderOpen,
  FaRegFileCode,
  FaAnglesUp,
  FaAnglesDown,
  FaAngleUp,
  FaDatabase,
  FaHardDrive,
  FaAngleRight,
  FaX,
} from "react-icons/fa6"

import g from "/lib/global"
import { hb_url, src_data_lua } from "/lib/scripts"
import { Adaptor } from "wao/web"
import WebRTC from "/lib/webrtc"
import {
  without,
  isNil,
  filter,
  append,
  addIndex,
  clone,
  indexBy,
  prop,
  prepend,
  map,
  compose,
  keys,
  includes,
} from "ramda"
import {
  DateMS,
  generateId,
  wait,
  tags,
  dayjs,
  getPreview,
  filterFiles,
  filterProjects,
} from "/lib/utils"
import lf from "localforage"
import md5 from "md5"
import { ctypes } from "/lib/data"

export default function Left() {
  const [tests, setTests] = use("tests")
  const [test, setTest] = use("test")
  const [cache, setCache] = use("cache")
  const [networks, setNetworks] = use("networks")
  const [modules, setModules] = use("modules")
  const [message, setMessage] = use("message")
  const [subs, setSubs] = use("subs")
  const [hbs, setHBs] = use("hbs")
  const [msgs, setMsgs] = use("msgs")
  const [files, setFiles] = use("files")
  const [file, setFile] = use("file")
  const [tab, setTab] = use("tab")
  const [wsid, setWSID] = use("wsid")
  const [projects, setProjects] = use("projects")
  const [localFS, setLocalFS] = use("localFS")
  const [localFSOpen, setLocalFSOpen] = use("localFSOpen")
  const [ctype, setCtype] = use("ctype")
  const [modal, setModal] = use("modal")
  const [modal2, setModal2] = use("modal2")
  const [modal3, setModal3] = use("modal3")
  const [modal4, setModal4] = use("modal4")
  const [mod, setModule] = use("module")
  const [proc, setProc] = use("proc")
  const [messages, setMessages] = use("messages")
  const [procs, setProcs] = use("procs")
  const [client, setClient] = use("client")
  const [clients, setClients] = use("clients")
  const [suid, setSUID] = use("suid")
  const [psid, setPSID] = use("psid")
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
          update: DateMS.now(),
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
      h="50px"
      align="center"
      p={4}
      fontSize="12px"
      css={{ borderBottom: "1px solid #ddd" }}
    >
      {tab === "Modules" ? (
        <>
          <Flex
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
            onClick={async () => {
              const jwk = await g.getWallet()
              //if (!jwk) return alert("wallet not connected")
              let pid, p
              ;({ pid, p } = await g.ao.deploy({ module: mod.id, jwk }))
              g.logSpawn(pid)
              const v = pid
              let _proc = clone(g.ao.mem.env[v])
              delete _proc.memory
              _proc.tags = clone(g.ao.mem.msgs[v]?.tags ?? [])
              _proc.id = v
              let _module = clone(mod)
              _module.processes.push(pid)
              setModule(_module)
              setProc(_proc)
              setMessages(
                addIndex(map)((v, i) => {
                  return {
                    id: v,
                    ...g.ao.mem.msgs[v],
                    slot: i,
                    http_msg: g.ao.mem.msgs[v],
                  }
                })(_proc.results)
              )

              let mmap = {}
              for (let k in g.ao.mem.modules) mmap[g.ao.mem.modules[k]] = k
              setProcs(append({ txid: pid, module: mmap[_proc.module] }, procs))
              setTab("Processes")
            }}
          >
            Spawn
          </Flex>
          <Box flex={1} />
        </>
      ) : tab === "Processes" ? (
        !proc || !file || file.ext !== "lua" ? null : (
          <>
            <Flex
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
              onClick={async () => {
                if (!proc) {
                  alert("Select a processl")
                } else {
                  const p = g.ao.p(proc.id)
                  const lua = g.editorRef.current.getValue()
                  const jwk = await g.getWallet()
                  //if (!jwk) return alert("wallet not connected")
                  const res = await p.msg("Eval", { data: lua, jwk })
                  g.logMsg(res.mid)
                  g.addMsg(res.mid)
                }
              }}
            >
              Eval
            </Flex>
            <Box flex={1} />
          </>
        )
      ) : tab === "Tests" ? (
        !file || file.ext !== "js" ? null : (
          <>
            <Flex
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
                    let _module = { exports: null }
                    const js = await src(name)
                    eval(js)
                    return _module.exports
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
                    }
                    res.push({
                      description: v.desc,
                      cases: _res,
                      success: _success,
                      fail: _fail,
                    })
                    i++
                  }
                  const result = {
                    file: file.name,
                    process: proc?.id ?? null,
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
              Run Test
            </Flex>
            <Box flex={1} />
          </>
        )
      ) : tab === "Projects" ? (
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
      ) : tab === "Networks" ? (
        <Flex align="center" w="100%">
          <Flex
            mr={2}
            fontSize="10px"
            color="white"
            bg="#5137C5"
            py={1}
            px={4}
            css={{
              borderRadius: "5px",
              cursor: "pointer",
              _hover: { opacity: 0.75 },
            }}
            onClick={() => {
              setModal2(true)
            }}
          >
            Launch Network
          </Flex>
          <Box flex={1} />
          {suid ? (
            <Flex
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
                if (confirm("Disconnect from WAO Hub?")) {
                  g.hub1.disconnect()
                  setSUID(null)
                  for (let k in g.peer2) g.peer2[k].close()
                  g.peer2 = {}
                  setClients([])
                  setClient(null)
                }
              }}
            >
              HUB : 8080
            </Flex>
          ) : (
            <Flex
              fontSize="10px"
              color="white"
              bg="#5137C5"
              py={1}
              px={3}
              css={{
                borderRadius: "5px",
                cursor: "pointer",
                _hover: { opacity: 0.75 },
              }}
              onClick={async () => {
                const processes = keys(g.ao.mem.env)
                if (processes.length === 0) {
                  const { p, pid, err } = await g.ao.deploy({
                    src_data: src_data_lua,
                  })
                  console.log(await p.d("Hello"))
                }
                g.hub1 = new Hub("ws://localhost:8080")
                g.hub1.onMsg = async obj => {
                  const recover = async (process, force) => {
                    if (force || isNil(g.ao.mem.env[process])) {
                      const { success } = await g.ao.recover(process)
                      if (!success) {
                        g.hub1.socket.send(
                          JSON.stringify({
                            id: obj.id,
                            status: 404,
                            type: "msg",
                            error: `not found`,
                          })
                        )
                      }
                      return success
                    }
                    return true
                  }
                  console.log("New Msg:", obj)
                  if (obj.subtype === "dryrun") {
                    let { process } = obj.message
                    if (!(await recover(process))) return
                    const res2 = await g.ao.dryrun(obj.message)
                    delete res2.Memory
                    g.hub1.socket.send(
                      JSON.stringify({
                        id: obj.id,
                        status: 200,
                        type: "msg",
                        msg: JSON.stringify(res2),
                      })
                    )
                    return
                  } else if (obj.subtype === "result") {
                    let { process, message } = obj
                    // todo: check if recovery is ongoing and wait if so
                    if (!(await recover(process))) return
                    const slot = message
                    if (!/^--[0-9a-zA-Z_-]{43,44}$/.test(message)) {
                      message = g.ao.mem.env[process]?.results?.[slot]
                    }
                    if (isNil(message)) {
                      // it's likely that hb is directly asking for a result without bundling
                      await recover(process, true) // force recovery
                      message = g.ao.mem.env[process]?.results?.[slot]
                      if (isNil(message)) {
                        g.hub1.socket.send(
                          JSON.stringify({
                            id: obj.id,
                            status: 404,
                            type: "msg",
                            error: `not found`,
                          })
                        )
                        return
                      }
                    }
                    let res2
                    let i = 0
                    while (i < 30) {
                      res2 = await g.ao.result({ message, process })
                      if (res2) break
                      await wait(100)
                      i++
                    }
                    if (typeof res2 === "undefined") {
                      g.hub1.socket.send(
                        JSON.stringify({
                          id: obj.id,
                          status: 404,
                          type: "msg",
                          error: `not found`,
                        })
                      )
                      return
                    }
                    g.hub1.socket.send(
                      JSON.stringify({
                        id: obj.id,
                        status: 200,
                        type: "msg",
                        msg: JSON.stringify(res2),
                      })
                    )
                    return
                  } else {
                    const t = tags(obj.message.http_msg.tags)
                    if (t.Type === "Process") {
                      const pid = await g.ao.spawn(obj.message)
                      g.logSpawn(pid)
                      const val = g.ao.mem.env[pid]
                      let mmap = {}
                      for (let k in g.ao.mem.modules)
                        mmap[g.ao.mem.modules[k]] = k
                      setProcs(
                        append({ txid: pid, module: mmap[val.module] }, procs)
                      )
                    } else {
                      let { process } = obj.message
                      if (!(await recover(process))) return
                      const mid = await g.ao.message(obj.message)
                      g.logMsg(mid)
                      g.addMsg(mid)
                    }
                    g.hub1.socket.send(
                      JSON.stringify({
                        id: obj.id,
                        status: 200,
                        type: "msg",
                        msg: "success",
                      })
                    )

                    return
                  }
                }

                g.hub1.onList = res => {
                  setHBs(res.hb)
                }
                g.hub1.onSubscribe = res => {
                  setSubs(res.accept)
                }
                g.hub1.onClose = () => {
                  setSUID(null)
                  setSubs({})
                  setHBs([])
                }
                g.hub1.onRegister = msg => {
                  g.hub1.socket.send(
                    JSON.stringify({ type: "list", target: "hb" })
                  )
                  g.hub1.registerSU()
                  setSUID(msg.clientId)
                }
                g.hub1.onOffer = async (offer, id) => {
                  setClients(c => append(id, c))
                  g.peer2[id] = new WebRTC()
                  g.peer2[id].onDataChannelMessage = async msg => {
                    const _msg = JSON.parse(msg)
                    if (_msg.type === "msg") {
                      console.log("New Message:", msg)
                      const p = g.ao.p(_msg.pid)
                      const res = await p.msg("Post", {
                        content: _msg.msg,
                      })
                      console.log(await p.d("Get"))
                    } else if (_msg.type === "processes") {
                      const processes = keys(g.ao.mem.env)
                      g.peer2[id].sendMessage(
                        JSON.stringify({
                          type: "processes",
                          processes,
                        })
                      )
                    } else {
                      setMsgs(m =>
                        prepend(
                          {
                            type: "Client",
                            msg: _msg.msg,
                            id,
                            date: DateMS.now(),
                          },
                          m
                        )
                      )
                    }
                  }
                  g.peer2[id].onConnectionStateChange = status => {
                    if (status === "disconnected") {
                      g.peer2[id].close()
                      delete g.peer2[id]
                      setClients(c => without([id], c))
                    }
                  }
                  g.hub1.sendAnswer(await g.peer2[id].createAnswer(offer), id)
                }
                g.hub1.connect()
              }}
            >
              HUB
            </Flex>
          )}

          {psid ? (
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
                if (confirm("Disconnect from WAO Proxy?")) {
                  g.hub1.disconnect()
                  setPSID(null)
                }
              }}
            >
              PROXY : 7070
            </Flex>
          ) : (
            <Flex
              ml={3}
              fontSize="10px"
              color="white"
              bg="#5137C5"
              py={1}
              px={3}
              css={{
                borderRadius: "5px",
                cursor: "pointer",
                _hover: { opacity: 0.75 },
              }}
              onClick={async () => {
                const adaptor = new Adaptor({ hb_url, aoconnect: g.ao.mem })
                g.hub1 = new Hub("ws://localhost:7070")
                g.hub1.onMsg = async obj => {
                  console.log("New PX Msg:", obj)
                  adaptor.get(obj.req, res => {
                    if (obj.req.device === "mu" && obj.req.path === "/") {
                      try {
                        let body = obj.req.body
                        if (
                          obj.req.body.type === "Buffer" &&
                          Array.isArray(obj.req.body.data)
                        ) {
                          body = new Uint8Array(body.data)
                        }

                        const _tags = new DataItem(body).tags
                        if (tags(_tags).Type === "Process") {
                          g.logSpawn(res.json.id)
                        } else {
                          g.logMsg(res.json.id)
                          g.addMsg(res.json.id)
                        }
                      } catch (e) {
                        console.log(e)
                      }
                    }
                    g.hub1.socket.send(
                      JSON.stringify({
                        type: "msg",
                        id: obj.id,
                        res: res ?? { status: 404, error: "not found" },
                      })
                    )
                  })
                }

                g.hub1.onClose = () => {
                  setPSID(null)
                }
                g.hub1.onRegister = msg => setPSID(msg.id)
                g.hub1.connect()
              }}
            >
              PROXY
            </Flex>
          )}
        </Flex>
      ) : null}
    </Flex>
  )

  const [init, setInit] = use("init")
  return !init ? null : (
    <Box
      fontSize="12px"
      w="315px"
      h="100%"
      css={{ borderRight: "1px solid #ddd", overflowY: "auto" }}
    >
      {buttons}
      {tab === "Messages" ? (
        map(v => {
          if (!v.http_msg?.tags && v.http_msg?.item) {
            v.http_msg.tags = new DataItem(v.http_msg.item.binary).tags
          }
          const t = tags(v.http_msg?.tags ?? [])
          return (
            <Flex
              h="50px"
              bg={v.id === message?.id ? "#5137C5" : "white"}
              fontSize="12px"
              p={4}
              direction="column"
              justify="center"
              onClick={() => {
                let _msg = clone(g.ao.mem.msgs[v.id])
                _msg.id = v.id
                if (_msg.http_msg) setMessage(_msg)
                else {
                  if (!_msg.tags) {
                    _msg.tags = new DataItem(_msg.item.binary).tags
                  }
                  setMessage({
                    item: v.item,
                    res: _msg.res,
                    http_msg: _msg,
                    id: _msg.id,
                    slot: v.slot,
                  })
                }
              }}
              css={{
                borderBottom: "1px solid #ddd",
                cursor: "pointer",
                _hover: { opacity: 0.75 },
              }}
            >
              <Flex
                fontWeight="bold"
                color={v.id !== message?.id ? "#5137C5" : "#ddd"}
              >
                <Flex w="20px" mr={2}>
                  [{v.slot}]
                </Flex>
                <Box>{t.Type === "Process" ? "Process" : t.Action}</Box>
              </Flex>
              <Box
                color={v.id !== message?.id ? "#222" : "#ddd"}
                fontSize="10px"
              >
                {v.id}
              </Box>
            </Flex>
          )
        })(messages)
      ) : tab === "Processes" ? (
        map(v => (
          <Flex
            h="50px"
            bg={v.txid === proc?.id ? "#5137C5" : "white"}
            fontSize="12px"
            p={4}
            direction="column"
            justify="center"
            onClick={() => {
              let _proc = clone(g.ao.mem.env[v.txid])
              delete _proc.memory
              _proc.tags = clone(g.ao.mem.msgs[v.txid]?.tags ?? [])
              _proc.id = v.txid
              setProc(_proc)
              setMessage(null)
            }}
            css={{
              borderBottom: "1px solid #ddd",
              cursor: "pointer",
              _hover: { opacity: 0.75 },
            }}
          >
            <Box
              fontWeight="bold"
              color={v.txid !== proc?.id ? "#5137C5" : "#ddd"}
            >
              {v.module}
            </Box>
            <Box fontSize="10px" color={v.txid !== proc?.id ? "#222" : "#ddd"}>
              {v.txid}
            </Box>
          </Flex>
        ))(procs)
      ) : tab === "Modules" ? (
        map(v => (
          <Flex
            h="50px"
            bg={v.txid === mod?.id ? "#5137C5" : "white"}
            fontSize="12px"
            p={4}
            direction="column"
            justify="center"
            onClick={() => {
              g._setModule(v.txid)
              let mmap = {}
              for (let k in g.ao.mem.modules) mmap[g.ao.mem.modules[k]] = k
              setProc(null)
              setMessages([])
              setMessage(null)
            }}
            css={{
              borderBottom: "1px solid #ddd",
              cursor: "pointer",
              _hover: { opacity: 0.75 },
            }}
          >
            <Box
              fontWeight="bold"
              color={v.txid !== mod?.id ? "#5137C5" : "#ddd"}
            >
              {v.name}
            </Box>
            <Box fontSize="10px" color={v.txid !== mod?.id ? "#222" : "#ddd"}>
              {v.txid}
            </Box>
          </Flex>
        ))(modules)
      ) : tab === "Projects" ? (
        map(v => {
          return (
            <>
              <Flex
                h="25px"
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
                            h="25px"
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
                                    txt = await fetch(v.fetch).then(r =>
                                      r.text()
                                    )
                                  } else {
                                    txt =
                                      (await lf.getItem(`file-${v.id}`)) ?? ""
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
        })(_projects)
      ) : tab === "Tests" ? (
        map(v => (
          <Flex
            h="50px"
            bg={v.id === test?.id ? "#5137C5" : "white"}
            fontSize="12px"
            p={4}
            direction="column"
            justify="center"
            onClick={async () => {
              setTest(v)
            }}
            css={{
              borderBottom: "1px solid #ddd",
              cursor: "pointer",
              _hover: { opacity: 0.75 },
            }}
          >
            <Flex
              align="center"
              fontWeight="bold"
              color={v.id !== test?.id ? "#5137C5" : "#ddd"}
            >
              <Box
                mr={4}
                px={2}
                bg="#bbb"
                color="#222"
                fontWeight="normal"
                css={{ borderRadius: "3px" }}
              >
                {v.file}
              </Box>
              success {v.success} : fail {v.fail} : {v.duration} ms
            </Flex>
            <Box color={v.id !== test?.id ? "#222" : "#ddd"}>
              {dayjs(v.date).fromNow()}
            </Box>
          </Flex>
        ))(tests)
      ) : (
        <>
          <Flex h="25px" px={4} align="center" bg="#eee" fontSize="12px">
            <Icon size="sm" color="#5137C5" mr={2}>
              <FaNetworkWired />
            </Icon>
            <Box>AO Networks</Box>
          </Flex>
          {map(v => {
            return (
              <Flex
                h="50px"
                bg={v.tag === ctype ? "#5137C5" : "white"}
                fontSize="12px"
                p={4}
                justify="center"
                onClick={() => setCtype(v.tag)}
                css={{
                  borderBottom: "1px solid #ddd",
                  cursor: "pointer",
                  _hover: { opacity: 0.75 },
                }}
              >
                <Flex direction="column" justify="center">
                  <Box
                    fontWeight="bold"
                    color={v.tag !== ctype ? "#5137C5" : "#ddd"}
                  >
                    {v.tag}
                  </Box>
                  <Box color={v.tag !== ctype ? "#222" : "#ddd"}>{v.desc}</Box>
                </Flex>
                <Box flex={1} />
                {cache === v.tag ? (
                  <Spinner
                    color={v.tag !== ctype ? "#5137C5" : "#ddd"}
                    animationDuration="1s"
                  />
                ) : null}
              </Flex>
            )
          })(networks)}
          <Flex h="25px" px={4} align="center" bg="#eee" fontSize="12px">
            <Icon size="sm" color="#5137C5" mr={2}>
              <FaNetworkWired />
            </Icon>
            <Box>Connections</Box>
          </Flex>
          {map(v => (
            <Flex
              h="50px"
              bg={v.key === ctype ? "#5137C5" : "white"}
              fontSize="12px"
              p={4}
              direction="column"
              justify="center"
              onClick={() => {
                if (!includes(v.key, ["hb"])) {
                  return alert("Coming Soon!")
                }
                setCtype(v.key)
              }}
              css={{
                borderBottom: "1px solid #ddd",
                cursor: "pointer",
                _hover: { opacity: 0.75 },
              }}
            >
              <Box
                fontWeight="bold"
                color={v.key !== ctype ? "#5137C5" : "#ddd"}
              >
                {v.name}
              </Box>
              <Box color={v.key !== ctype ? "#222" : "#ddd"}>{v.desc}</Box>
            </Flex>
          ))(ctypes)}
        </>
      )}
    </Box>
  )
}
