import { Link, ssr } from "arnext"
import { Icon } from "@chakra-ui/react"
import _assert from "assert"
import {
  Input,
  NativeSelect,
  Image,
  Box,
  Flex,
  Textarea,
} from "@chakra-ui/react"
import { useRef, useEffect, useState } from "react"
import dayjs from "dayjs"
import relativeTime from "dayjs/plugin/relativeTime"
import {
  FaBookOpen,
  FaDiscord,
  FaXTwitter,
  FaGithub,
  FaAngleRight,
  FaX,
} from "react-icons/fa6"
import lf from "localforage"
function generateId() {
  return Math.random().toString(36).substring(2, 15)
}

dayjs.extend(relativeTime)
const wait = ms => new Promise(res => setTimeout(() => res(), ms))
import {
  addIndex,
  prop,
  indexBy,
  includes,
  clone,
  isNil,
  keys,
  prepend,
  reverse,
  append,
  map,
  without,
  fromPairs,
  filter,
} from "ramda"
import { AO, acc } from "wao/web"
import { HB } from "wao"
import Hub from "../lib/hub"
import WebRTC from "../lib/webrtc"
import Editor from "@monaco-editor/react"
let peer1 = null
let peer2 = {}

let hub1 = null
let hub2 = null
let ao = null
const tags = tags => fromPairs(map(v => [v.name, v.value])(tags))

const src_data = `-- Initialize a table to store messages
local messages = {}
local MAX_MESSAGES = 10
local json = require("json")
-- Handler for "Hello" (from your original code)
Handlers.add("Hello", "Hello", function (msg)
  msg.reply({ Data = true })
end)

-- Handler for "Post" to prepend a new message
Handlers.add("Post", "Post", function (msg)
  -- Get the message content from the request
  local content = msg.content
  
  -- Validate that content exists
  if not content then
    msg.reply({ Success = false, Error = "No message content provided" })
    return
  end
  
  -- Prepend the new message to the messages table
  table.insert(messages, 1, {
    id = #messages + 1,
    content = content,
    timestamp = os.time()
  })
  
  -- We're keeping all messages in the table, no removal
  
  -- Reply with success
  msg.reply({ Success = true, MessageId = messages[1].id })
end)

-- Handler for "Get" to return recent messages
Handlers.add("Get", "Get", function (msg)
  -- Get the count parameter, default to MAX_MESSAGES
  local count = tonumber(msg.count) or MAX_MESSAGES
  
  -- Ensure count doesn't exceed MAX_MESSAGES
  count = math.min(count, MAX_MESSAGES)
  
  -- Get the most recent messages up to the count
  local recent_messages = {}
  for i = 1, math.min(count, #messages) do
    table.insert(recent_messages, messages[i])
  end
  
  -- Reply with the messages
  msg.reply({ 
    Data = json.encode({
     Messages = recent_messages,
      Count = #recent_messages,
      Total = #messages
    })
  })
end)`

export default function Home({}) {
  const [modal, setModal] = useState(false)
  const [subs, setSubs] = useState({})
  const [clients, setClients] = useState([])
  const [processes, setProcesses] = useState([])
  const [prc, setPRC] = useState(null)
  const [msg, setMsg] = useState("")
  const [msgs, setMsgs] = useState([])
  const [msg2, setMsg2] = useState("")
  const [sus, setSUs] = useState([])
  const [hbs, setHBs] = useState([])
  const [su, setSU] = useState(null)
  const [suid, setSUID] = useState(null)
  const [cid, setCID] = useState(null)
  const [client, setClient] = useState(null)
  const [tab, setTab] = useState("Modules")
  const [init, setInit] = useState(false)
  const [modules, setModules] = useState([])
  const [module, setModule] = useState(null)
  const [procs, setProcs] = useState([])
  const [proc, setProc] = useState(null)
  const [messages, setMessages] = useState([])
  const [message, setMessage] = useState(null)
  const [ctype, setCtype] = useState("hb")
  const [files, setFiles] = useState([])
  const [file, setFile] = useState(null)
  const [filename, setFilename] = useState("")
  const [fileext, setFileext] = useState("js")
  const [monaco, setMonaco] = useState(null)
  const tabs = [
    "Modules",
    "Processes",
    "Messages",
    "Accounts",
    "Tokens",
    "Storage",
    "Database",
    "Files",
    "Networks",
  ]
  const _setModule = id => {
    let mmap = {}
    for (let k in ao.mem.modules) {
      mmap[ao.mem.modules[k]] = k
    }
    let _procs = []
    let mod = clone(ao.mem.txs[id])
    mod.processes = []
    for (let k in ao.mem.env) {
      for (let v of ao.mem.msgs[k]?.tags ?? []) {
        if (v.name === "Module" && v.value === mod.id) {
          mod.processes.push(k)
          const val = ao.mem.env[k]
          _procs.push({ txid: k, module: mmap[val.module] })
        }
      }
    }
    setProcs(_procs)
    setModule(mod)
  }
  useEffect(() => {
    ;(async () => {
      const files = (await lf.getItem("files")) ?? []
      setFiles(files)
    })()
  }, [])
  useEffect(() => {
    ;(async () => {
      ao = await new AO({ hb_url: "http://localhost:10001" }).init(acc[0])
      await ao.mem.init()
      let _modules = []
      let mmap = {}
      for (let k in ao.mem.modules) {
        _modules.push({ name: k, txid: ao.mem.modules[k] })
        mmap[ao.mem.modules[k]] = k
      }
      setModules(_modules)
      let _procs = []
      for (let k in ao.mem.env) {
        const val = ao.mem.env[k]
        _procs.push({ txid: k, module: mmap[val.module] })
      }
      setProcs(_procs)
      _setModule("Do_Uc2Sju_ffp6Ev0AnLVdPtot15rvMjP-a9VVaA5fM")
      setInit(true)
    })()
  }, [])
  const ctypes = [
    {
      key: "hb",
      name: "HyperBEAM Nodes",
      desc: "Websocket subscriptions to remote HyperBEAM nodes",
    },
    {
      key: "su",
      name: "Scheduler Units ( SUs )",
      desc: "Connections to browser SUs via WebRTC",
    },
    {
      key: "cu",
      name: "Compute Units ( CUs )",
      desc: "Connections to browser CUs via WebRTC",
    },
    {
      key: "c",
      name: "Clients",
      desc: "Other browsers subscribing to your SU & CU via WebRTC",
    },
    {
      key: "hub",
      name: "WAO Hubs",
      desc: "Websocket connections to WAO hubs",
    },
  ]
  const modmap = indexBy(prop("txid"))(modules ?? [])
  const editorRef = useRef(null)

  function handleEditorDidMount(editor, monaco) {
    editorRef.current = editor
    editorRef.current.setValue(src_data)
    setMonaco(monaco)
  }

  const fileInputRef = useRef(null)

  const handleImportClick = () => {
    fileInputRef.current.click()
  }

  const handleFileChange = event => {
    const file = event.target.files[0]
    if (file) {
      const reader = new FileReader()
      reader.onload = async e => {
        const txt = e.target.result
        const id = generateId()
        const fileext = file.name.split(".").pop().toLowerCase()
        const _file = { name: file.name, update: Date.now(), id }
        const _files = prepend(_file, files)
        await lf.setItem("files", _files)
        await lf.setItem(`file-${id}`, txt)
        setFiles(_files)
        setFile(_file)
        event.target.value = ""
        monaco.editor.setModelLanguage(
          editorRef.current.getModel(),
          fileext === "js" ? "javascript" : fileext
        )
        // todo: handle this better
        setTimeout(() => editorRef.current.setValue(txt), 100)
      }

      reader.readAsText(file)
    }
  }
  let act = null
  let meta = []
  if (message) {
    const t = tags(message.http_msg.tags)
    act = t.Type === "Process" ? "Process" : t.Action
    meta.push({ name: "ID", value: message.id })
    meta.push({
      name: "Process",
      value: t.Type === "Process" ? message.id : message.http_msg.process,
    })
    meta.push({ name: "Slot", value: message.slot })
    const tx = ao?.mem.txs[message.id]
    if (tx?.bundle) {
      const _tx = ao.mem.txs[tx.bundle]
      meta.push({ name: "From", value: _tx.owner })
    }
  }
  return (
    <>
      <style jsx global>{`
        * {
          scrollbar-width: thin;
          scrollbar-color: #bbbbbb #f1f1f1;
        }

        *::-webkit-scrollbar {
          width: 8px;
          height: 8px;
        }

        *::-webkit-scrollbar-track {
          background: #f1f1f1;
          border-radius: 4px;
        }

        *::-webkit-scrollbar-thumb {
          background: #bbbbbb;
          border-radius: 4px;
        }

        *::-webkit-scrollbar-thumb:hover {
          background: #999999;
        }
      `}</style>
      <Flex
        align="center"
        justify="center"
        height="50px"
        w="100%"
        bg="white"
        css={{
          top: 0,
          left: 0,
          position: "fixed",
          boxShadow: "0px 2px 10px 0px rgba(0,0,0,0.75);",
          zIndex: 6,
        }}
      >
        <Flex w="100%" mx={4}>
          <Flex
            fontWeight="bold"
            color="#5137C5"
            fontSize="14px"
            align="center"
          >
            WAO LOCALNET
          </Flex>
          <Flex flex={1} />
          {suid ? (
            <Flex
              fontSize="14px"
              bg="white"
              color="#5137C5"
              py={1}
              px={4}
              css={{
                border: "1px solid #5137C5",
                borderRadius: "5px",
                cursor: "pointer",
                _hover: { opacity: 0.75 },
              }}
              onClick={() => {
                if (confirm("Disconnect from WAO Hub?")) {
                  hub1.disconnect()
                  setSUID(null)
                  for (let k in peer2) peer2[k].close()
                  peer2 = {}
                  setClients([])
                  setClient(null)
                }
              }}
            >
              WAO Hub : ws://localhost:8080
            </Flex>
          ) : (
            <Flex
              fontSize="14px"
              color="white"
              bg="#5137C5"
              py={1}
              px={4}
              css={{
                borderRadius: "5px",
                cursor: "pointer",
                _hover: { opacity: 0.75 },
              }}
              onClick={async () => {
                const processes = keys(ao.mem.env)
                if (processes.length === 0) {
                  const { p, pid, err } = await ao.deploy({
                    src_data,
                  })
                  console.log(await p.d("Hello"))
                }
                hub1 = new Hub("ws://localhost:8080")
                hub1.onMsg = async obj => {
                  const recover = async (process, force) => {
                    if (force || isNil(ao.mem.env[process])) {
                      const { success } = await ao.recover(process)
                      if (!success) {
                        hub1.socket.send(
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
                    const res2 = await ao.dryrun(obj.message)
                    delete res2.Memory
                    hub1.socket.send(
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
                      message = ao.mem.env[process]?.results?.[slot]
                    }
                    if (isNil(message)) {
                      // it's likely that hb is directly asking for a result without bundling
                      await recover(process, true) // force recovery
                      message = ao.mem.env[process]?.results?.[slot]
                      if (isNil(message)) {
                        hub1.socket.send(
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
                      res2 = await ao.result({ message, process })
                      if (res2) break
                      await wait(100)
                      i++
                    }
                    if (typeof res2 === "undefined") {
                      hub1.socket.send(
                        JSON.stringify({
                          id: obj.id,
                          status: 404,
                          type: "msg",
                          error: `not found`,
                        })
                      )
                      return
                    }
                    hub1.socket.send(
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
                      const pid = await ao.spawn(obj.message)
                      const val = ao.mem.env[pid]
                      let mmap = {}
                      for (let k in ao.mem.modules) mmap[ao.mem.modules[k]] = k
                      setProcs(
                        append({ txid: pid, module: mmap[val.module] }, procs)
                      )
                    } else {
                      let { process } = obj.message
                      if (!(await recover(process))) return
                      await ao.message(obj.message)
                    }
                    hub1.socket.send(
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

                hub1.onList = res => {
                  setHBs(res.hb)
                }
                hub1.onSubscribe = res => {
                  setSubs(res.accept)
                }
                hub1.onClose = () => {
                  setSUID(null)
                  setSubs({})
                  setHBs([])
                }
                hub1.onRegister = id => {
                  hub1.socket.send(
                    JSON.stringify({ type: "list", target: "hb" })
                  )
                  hub1.registerSU()
                  setSUID(id)
                }
                hub1.onOffer = async (offer, id) => {
                  setClients(c => append(id, c))
                  peer2[id] = new WebRTC()
                  peer2[id].onDataChannelMessage = async msg => {
                    const _msg = JSON.parse(msg)
                    if (_msg.type === "msg") {
                      console.log("New Message:", msg)
                      const p = ao.p(_msg.pid)
                      const res = await p.msg("Post", {
                        content: _msg.msg,
                      })
                      console.log(await p.d("Get"))
                    } else if (_msg.type === "processes") {
                      const processes = keys(ao.mem.env)
                      peer2[id].sendMessage(
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
                            date: Date.now(),
                          },
                          m
                        )
                      )
                    }
                  }
                  peer2[id].onConnectionStateChange = status => {
                    if (status === "disconnected") {
                      peer2[id].close()
                      delete peer2[id]
                      setClients(c => without([id], c))
                    }
                  }
                  hub1.sendAnswer(await peer2[id].createAnswer(offer), id)
                }
                hub1.connect()
              }}
            >
              Connect to WAO Hub
            </Flex>
          )}
        </Flex>
      </Flex>
      <Flex h="100vh" css={{ zIndex: 0 }} pt="50px">
        {!init ? null : (
          <Flex bg="#eee" css={{ overflowY: "auto" }}>
            <Flex direction="column" w="150px">
              {map(v => {
                return (
                  <Flex
                    h="50px"
                    w="100%"
                    fontSize="12px"
                    align="center"
                    css={{
                      cursor: "pointer",
                      _hover: { opacity: 0.75 },
                      flexShrink: 0,
                    }}
                    onClick={() => {
                      if (v === "Messages" && proc === null) return
                      if (
                        includes(v, [
                          "Accounts",
                          "Tokens",
                          "Storage",
                          "Database",
                        ])
                      ) {
                        return alert("Coming Soon!")
                      }
                      setTab(v)
                    }}
                    bg={v === tab ? "#5137C5" : ""}
                    color={
                      includes(v, [
                        "Accounts",
                        "Tokens",
                        "Storage",
                        "Database",
                      ]) ||
                      (v === "Messages" && proc === null)
                        ? "#999"
                        : v === tab
                          ? "white"
                          : ""
                    }
                    justify="center"
                  >
                    {v.toUpperCase()}
                  </Flex>
                )
              })(tabs)}
            </Flex>
          </Flex>
        )}
        <Flex direction="column">
          <Flex
            h="50px"
            align="center"
            p={4}
            fontSize="12px"
            css={{ borderBottom: "1px solid #ddd" }}
          >
            {tab === "Files" ? (
              <>
                <Box flex={1} />
                <Flex
                  py={1}
                  px={4}
                  fontSize="12px"
                  color="#ddd"
                  bg="#5137C5"
                  css={{
                    borderRadius: "5px",
                    cursor: "pointer",
                    border: "1px solid #5137C5",
                    _hover: { opacity: 0.75 },
                  }}
                  onClick={async () => {
                    setModal(true)
                  }}
                >
                  <input
                    type="file"
                    ref={fileInputRef}
                    onChange={handleFileChange}
                    accept=".lua"
                    style={{ display: "none" }}
                  />
                  New
                </Flex>
                <Flex
                  ml={4}
                  py={1}
                  px={4}
                  fontSize="12px"
                  color="#ddd"
                  bg="#5137C5"
                  css={{
                    borderRadius: "5px",
                    cursor: "pointer",
                    border: "1px solid #5137C5",
                    _hover: { opacity: 0.75 },
                  }}
                  onClick={handleImportClick}
                >
                  <input
                    type="file"
                    ref={fileInputRef}
                    onChange={handleFileChange}
                    accept=".lua, .js, .json"
                    style={{ display: "none" }}
                  />
                  Import
                </Flex>
              </>
            ) : tab === "Networks" ? (
              <Flex align="center">
                <Box
                  bg="#ddd"
                  mr={2}
                  css={{ borderRadius: "3px" }}
                  px={2}
                  py={1}
                >
                  LOCANNET
                </Box>
              </Flex>
            ) : (
              <>
                {!module ||
                !includes(tab, ["Modules", "Processes", "Messages"]) ? null : (
                  <Flex
                    align="center"
                    css={{
                      cursor: "pointer",
                      _hover: { opacity: 0.75 },
                    }}
                    onClick={() => setTab("Modules")}
                  >
                    <Box
                      bg="#ddd"
                      mr={2}
                      css={{ borderRadius: "3px" }}
                      px={2}
                      py={1}
                    >
                      {modmap[module.id].name}
                    </Box>
                    {includes(tab, ["Modules", "Processes"]) ? (
                      <Box mx={2}>{module.id}</Box>
                    ) : null}
                  </Flex>
                )}
                {tab !== "Modules" ? null : (
                  <>
                    <Box flex={1} />
                    <Flex
                      py={1}
                      px={4}
                      fontSize="12px"
                      color="#ddd"
                      bg="#5137C5"
                      css={{
                        borderRadius: "5px",
                        cursor: "pointer",
                        border: "1px solid #5137C5",
                        _hover: { opacity: 0.75 },
                      }}
                      onClick={async () => {
                        let pid, p
                        ;({ pid, p } = await ao.deploy({ module: module.id }))
                        const v = pid
                        let _proc = clone(ao.mem.env[v])
                        delete _proc.memory
                        _proc.tags = clone(ao.mem.msgs[v]?.tags ?? [])
                        _proc.id = v
                        setProc(_proc)
                        setMessages(
                          addIndex(map)((v, i) => {
                            return {
                              id: v,
                              ...ao.mem.msgs[v],
                              slot: i,
                              http_msg: ao.mem.msgs[v],
                            }
                          })(_proc.results)
                        )

                        let mmap = {}
                        for (let k in ao.mem.modules)
                          mmap[ao.mem.modules[k]] = k
                        setProcs(
                          append(
                            { txid: pid, module: mmap[_proc.module] },
                            procs
                          )
                        )
                        setTab("Processes")
                      }}
                    >
                      Spawn
                    </Flex>
                  </>
                )}
                {!proc || !includes(tab, ["Processes", "Messages"]) ? null : (
                  <>
                    <Icon size="md" color="#5137C5">
                      <FaAngleRight />
                    </Icon>
                    <Flex
                      align="center"
                      ml={2}
                      css={{
                        cursor: "pointer",
                        _hover: { opacity: 0.75 },
                      }}
                      onClick={() => setTab("Processes")}
                    >
                      <Box mr={2}>{proc?.id}</Box>
                    </Flex>
                  </>
                )}
                {tab !== "Processes" ||
                !proc ||
                !file ||
                file.ext !== "lua" ? null : (
                  <>
                    <Box flex={1} />
                    <Flex
                      py={1}
                      px={4}
                      fontSize="12px"
                      color="#ddd"
                      bg="#5137C5"
                      css={{
                        borderRadius: "5px",
                        cursor: "pointer",
                        border: "1px solid #5137C5",
                        _hover: { opacity: 0.75 },
                      }}
                      onClick={async () => {
                        if (!proc) {
                          alert("Select a processl")
                        } else {
                          const p = ao.p(proc.id)
                          const lua = editorRef.current.getValue()
                          const res = await p.msg("Eval", { data: lua })
                          console.log(res)
                        }
                      }}
                    >
                      Eval
                    </Flex>
                  </>
                )}
                {!message || !includes(tab, ["Messages"]) ? null : (
                  <>
                    <Icon size="md" color="#5137C5">
                      <FaAngleRight />
                    </Icon>
                    <Flex
                      align="center"
                      ml={2}
                      css={{
                        cursor: "pointer",
                        _hover: { opacity: 0.75 },
                      }}
                      onClick={() => setTab("Messages")}
                    >
                      <Flex mr={4}>
                        <Box
                          py={1}
                          px={2}
                          bg="#bbb"
                          css={{ borderRadius: "3px 0 0 3px" }}
                        >
                          {message.slot}
                        </Box>
                        <Box
                          py={1}
                          px={2}
                          bg="#ddd"
                          css={{ borderRadius: "0 3px 3px 0" }}
                        >
                          {act}
                        </Box>
                      </Flex>
                      <Box>{message.id}</Box>
                    </Flex>
                  </>
                )}
              </>
            )}
          </Flex>
          <Flex
            flex={1}
            w={init ? (tab === "Files" ? "385px" : "965px") : "100vw"}
          >
            {!init ? null : tab === "Messages" ? (
              <Flex w="100%">
                <Box
                  w="385px"
                  h="calc(100vh - 130px)"
                  css={{ borderRight: "1px solid #ddd", overflowY: "auto" }}
                >
                  {map(v => {
                    const tags = tags =>
                      fromPairs(map(v => [v.name, v.value])(tags))
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
                          let _msg = clone(ao.mem.msgs[v.id])
                          _msg.id = v.id
                          if (_msg.http_msg) setMessage(_msg)
                          else
                            setMessage({
                              res: _msg.res,
                              http_msg: _msg,
                              id: _msg.id,
                              slot: v.slot,
                            })
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
                          <Box>
                            {t.Type === "Process" ? "Process" : t.Action}
                          </Box>
                        </Flex>
                        <Box color={v.id !== message?.id ? "#222" : "#ddd"}>
                          {v.id}
                        </Box>
                      </Flex>
                    )
                  })(messages)}
                </Box>
                {!message ? null : (
                  <Box
                    px={4}
                    py={2}
                    fontSize="12px"
                    flex={1}
                    h="calc(100vh - 130px)"
                    css={{ overflowY: "auto" }}
                  >
                    <Flex my={2} fontWeight="bold" color="#5137C5">
                      Metadata
                    </Flex>
                    {map(v => {
                      if (includes(v.name, ["signature", "signature-input"])) {
                        return null
                      }
                      return (
                        <Flex my={2} align="center">
                          <Box
                            w="130px"
                            color="white"
                            bg="#5137C5"
                            px={2}
                            mr={4}
                            css={{ borderRadius: "3px" }}
                          >
                            {v.name}
                          </Box>
                          <Box
                            flex={1}
                            css={{ wordBreak: "break-all", whiteSpace: "wrap" }}
                          >
                            {v.value}
                          </Box>
                        </Flex>
                      )
                    })(meta ?? [])}
                    <Flex my={2} fontWeight="bold" color="#5137C5">
                      Tags
                    </Flex>
                    {map(v => {
                      if (includes(v.name, ["signature", "signature-input"])) {
                        return null
                      }
                      return (
                        <Flex my={2} align="center">
                          <Box
                            w="130px"
                            color="white"
                            bg="#5137C5"
                            px={2}
                            mr={4}
                            css={{ borderRadius: "3px" }}
                          >
                            {v.name}
                          </Box>
                          <Box
                            flex={1}
                            css={{ wordBreak: "break-all", whiteSpace: "wrap" }}
                          >
                            {v.value}
                          </Box>
                        </Flex>
                      )
                    })(message.http_msg.tags)}
                    <Flex mt={4} fontWeight="bold" mb={2} color="#5137C5">
                      Data
                    </Flex>
                    <code>
                      <Box
                        as="pre"
                        bg="#eee"
                        p={4}
                        css={{
                          borderRadius: "3px",
                          wordBreak: "break-word",
                          whiteSpace: "pre-wrap",
                          overflow: "auto",
                        }}
                      >
                        {message.http_msg?.data}
                      </Box>
                    </code>

                    <Flex mt={4} mb={2} fontWeight="bold" color="#5137C5">
                      Result
                    </Flex>
                    <code>
                      <Box
                        as="pre"
                        bg="#eee"
                        p={4}
                        css={{
                          borderRadius: "3px",
                          wordBreak: "break-word",
                          whiteSpace: "pre-wrap",
                          overflow: "auto",
                        }}
                      >
                        {!message.res
                          ? ""
                          : JSON.stringify(message.res, undefined, 2)}
                      </Box>
                    </code>
                  </Box>
                )}
              </Flex>
            ) : tab === "Processes" ? (
              <Flex w="100%">
                <Box
                  w="385px"
                  h="calc(100vh - 130px)"
                  css={{ borderRight: "1px solid #ddd", overflowY: "auto" }}
                >
                  {map(v => (
                    <Flex
                      h="50px"
                      bg={v.txid === proc?.id ? "#5137C5" : "white"}
                      fontSize="12px"
                      p={4}
                      direction="column"
                      justify="center"
                      onClick={() => {
                        let _proc = clone(ao.mem.env[v.txid])
                        delete _proc.memory
                        _proc.tags = clone(ao.mem.msgs[v.txid]?.tags ?? [])
                        _proc.id = v.txid
                        setProc(_proc)
                        setMessage(null)
                        setMessages(
                          addIndex(map)((v, i) => {
                            if (!v.http_msg) {
                              return {
                                http_msg: ao.mem.msgs[v],
                                id: v,
                                slot: i,
                              }
                            } else {
                              return ao.mem.msgs[v]
                            }
                          })(_proc.results)
                        )
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
                      <Box color={v.txid !== proc?.id ? "#222" : "#ddd"}>
                        {v.txid}
                      </Box>
                    </Flex>
                  ))(procs)}
                </Box>
                {!proc ? null : (
                  <Box
                    px={4}
                    py={2}
                    fontSize="12px"
                    flex={1}
                    h="calc(100vh - 130px)"
                    css={{ overflowY: "auto" }}
                  >
                    <Flex my={2} fontWeight="bold" color="#5137C5">
                      Tags
                    </Flex>
                    {map(v => {
                      if (includes(v.name, ["signature", "signature-input"])) {
                        return null
                      }
                      return (
                        <Flex my={2} align="center">
                          <Box
                            w="130px"
                            color="white"
                            bg="#5137C5"
                            px={2}
                            mr={4}
                            css={{ borderRadius: "3px" }}
                          >
                            {v.name}
                          </Box>
                          <Box flex={1} css={{ wordBreak: "break-all" }}>
                            {v.value}
                          </Box>
                        </Flex>
                      )
                    })(proc.tags)}
                    <Flex mt={4} mb={2} fontWeight="bold" color="#5137C5">
                      Messages ( {proc.results.length} )
                    </Flex>
                    {map(v => (
                      <Flex
                        py={2}
                        align="center"
                        css={{
                          borderBottom: "1px solid #ddd",
                          cursor: "pointer",
                          _hover: { opacity: 0.75 },
                        }}
                        onClick={() => {
                          let _msg = clone(ao.mem.msgs[v.id])
                          _msg.id = v.id
                          if (_msg.http_msg) setMessage(_msg)
                          else
                            setMessage({
                              res: _msg.res,
                              http_msg: _msg,
                              id: _msg.id,
                              slot: v.slot,
                            })
                          setTab("Messages")
                        }}
                      >
                        <Flex color="#222" align="center">
                          <Flex
                            w="30px"
                            mr={4}
                            align="center"
                            fontSize="10px"
                            bg="#ddd"
                            justify="center"
                            css={{ borderRadius: "3px" }}
                          >
                            {v.slot}
                          </Flex>
                          <Box>{v.id}</Box>
                        </Flex>
                      </Flex>
                    ))(messages)}
                  </Box>
                )}
              </Flex>
            ) : tab === "Modules" ? (
              <Flex w="100%">
                <Box
                  w="385px"
                  h="calc(100vh - 130px)"
                  css={{ borderRight: "1px solid #ddd", overflowY: "auto" }}
                >
                  {map(v => (
                    <Flex
                      h="50px"
                      bg={v.txid === module?.id ? "#5137C5" : "white"}
                      fontSize="12px"
                      p={4}
                      direction="column"
                      justify="center"
                      onClick={() => {
                        _setModule(v.txid)
                        let mmap = {}
                        for (let k in ao.mem.modules)
                          mmap[ao.mem.modules[k]] = k
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
                        color={v.txid !== module?.id ? "#5137C5" : "#ddd"}
                      >
                        {v.name}
                      </Box>
                      <Box color={v.txid !== module?.id ? "#222" : "#ddd"}>
                        {v.txid}
                      </Box>
                    </Flex>
                  ))(modules)}
                </Box>
                {!module ? null : (
                  <Box
                    px={4}
                    py={2}
                    fontSize="12px"
                    flex={1}
                    h="calc(100vh - 130px)"
                    css={{ overflowY: "auto" }}
                  >
                    <Flex my={2} fontWeight="bold" color="#5137C5">
                      Tags
                    </Flex>
                    {map(v => (
                      <Flex my={2} align="center">
                        <Box
                          w="130px"
                          color="white"
                          bg="#5137C5"
                          px={2}
                          mr={4}
                          css={{ borderRadius: "3px" }}
                        >
                          {v.name}
                        </Box>
                        <Box>{v.value}</Box>
                      </Flex>
                    ))(module.tags)}
                    <Flex mt={4} mb={2} fontWeight="bold" color="#5137C5">
                      Processes ( {module.processes.length} )
                    </Flex>
                    {map(v => (
                      <Flex
                        py={1}
                        align="center"
                        css={{
                          borderBottom: "1px solid #ddd",
                          cursor: "pointer",
                          _hover: { opacity: 0.75 },
                        }}
                        onClick={() => {
                          let _proc = clone(ao.mem.env[v])
                          delete _proc.memory
                          _proc.tags = clone(ao.mem.msgs[v]?.tags ?? [])
                          _proc.id = v
                          setProc(_proc)
                          setMessages(
                            addIndex(map)((v, i) => {
                              if (!v.http_msg) {
                                return {
                                  http_msg: ao.mem.msgs[v],
                                  id: v,
                                  slot: i,
                                }
                              } else {
                                return ao.mem.msgs[v]
                              }
                            })(_proc.results)
                          )
                          setTab("Processes")
                        }}
                      >
                        <Box color="#222">{v}</Box>
                      </Flex>
                    ))(module.processes)}
                  </Box>
                )}
              </Flex>
            ) : tab === "Files" ? (
              <Flex>
                <Box
                  w="385px"
                  h="calc(100vh - 130px)"
                  css={{ borderRight: "1px solid #ddd", overflowY: "auto" }}
                >
                  {map(v => (
                    <Flex
                      h="50px"
                      bg={v.id === file?.id ? "#5137C5" : "white"}
                      fontSize="12px"
                      p={4}
                      direction="column"
                      justify="center"
                      onClick={async () => {
                        const txt = (await lf.getItem(`file-${v.id}`)) ?? ""
                        setFile(v)
                        monaco.editor.setModelLanguage(
                          editorRef.current.getModel(),
                          v.ext === "js" ? "javascript" : v.ext ? v.ext : "lua"
                        )
                        // todo: handle this better
                        setTimeout(() => editorRef.current.setValue(txt), 100)
                      }}
                      css={{
                        borderBottom: "1px solid #ddd",
                        cursor: "pointer",
                        _hover: { opacity: 0.75 },
                      }}
                    >
                      <Box
                        fontWeight="bold"
                        color={v.id !== file?.id ? "#5137C5" : "#ddd"}
                      >
                        {v.name}
                      </Box>
                      <Box color={v.id !== file?.id ? "#222" : "#ddd"}>
                        {dayjs(v.update).fromNow()}
                      </Box>
                    </Flex>
                  ))(files)}
                </Box>
              </Flex>
            ) : (
              <>
                <Flex>
                  <Box
                    w="385px"
                    h="calc(100vh - 130px)"
                    css={{ borderRight: "1px solid #ddd", overflowY: "auto" }}
                  >
                    {map(v => (
                      <Flex
                        h="50px"
                        bg={v.key === ctype ? "#5137C5" : "white"}
                        fontSize="12px"
                        p={4}
                        direction="column"
                        justify="center"
                        onClick={() => {
                          if (!includes(v, ["hb"])) {
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
                        <Box color={v.key !== ctype ? "#222" : "#ddd"}>
                          {v.desc}
                        </Box>
                      </Flex>
                    ))(ctypes)}
                  </Box>

                  {ctype === "hb" ? (
                    suid ? (
                      <Box
                        flex={1}
                        h="calc(100vh - 130px)"
                        css={{ overflowY: "auto" }}
                      >
                        <Flex
                          px={4}
                          fontWeight="bold"
                          color="#5137C5"
                          h="50px"
                          align="center"
                        >
                          HyperBEAM Nodes ( {hbs.length} )
                        </Flex>
                        {map(v => {
                          return (
                            <Flex
                              h="50px"
                              fontSize="14px"
                              py={2}
                              px={4}
                              align="center"
                              css={{
                                borderTop: "1px solid #ddd",
                                cursor: "pointer",
                                _hover: { opacity: 0.75 },
                              }}
                            >
                              <Box color="#222">{v.address}</Box>
                              <Box flex={1}></Box>
                              {subs?.hb?.[v.address]?.["*"] ? (
                                <Flex
                                  bg="white"
                                  color="#5137C5"
                                  px={4}
                                  ml={4}
                                  css={{
                                    border: "1px solid #5137C5",
                                    borderRadius: "5px",
                                    cursor: "pointer",
                                    _hover: { opacity: 0.75 },
                                  }}
                                  onClick={() => {
                                    hub1.socket.send(
                                      JSON.stringify({
                                        type: "subscribe",
                                        accept: { hb: { [v.address]: false } },
                                      })
                                    )
                                  }}
                                >
                                  Unsubscribe
                                </Flex>
                              ) : (
                                <Flex
                                  color="#ddd"
                                  bg="#5137C5"
                                  px={4}
                                  ml={4}
                                  css={{
                                    border: "1px solid #5137C5",
                                    borderRadius: "5px",
                                    cursor: "pointer",
                                    _hover: { opacity: 0.75 },
                                  }}
                                  onClick={() => {
                                    hub1.socket.send(
                                      JSON.stringify({
                                        type: "subscribe",
                                        accept: {
                                          hb: { [v.address]: { "*": true } },
                                        },
                                      })
                                    )
                                  }}
                                >
                                  Subscribe
                                </Flex>
                              )}
                            </Flex>
                          )
                        })(hbs)}
                      </Box>
                    ) : (
                      <Box p={4}>Not Connected </Box>
                    )
                  ) : (
                    <>
                      <Box flex={1} p={6}>
                        <Box
                          fontSize="20px"
                          fontWeight="bold"
                          color="#5137C5"
                          css={{ textDecoration: "underline" }}
                          mb={2}
                        >
                          Scheduler Unit (SU)
                        </Box>
                        <Box w="100%" maxW="700px">
                          <Box>
                            {suid ? (
                              <>
                                <Box>
                                  SUID: {suid} ({" "}
                                  <Box
                                    as="span"
                                    onClick={() => {}}
                                    color="#5137C5"
                                    css={{
                                      textDecoration: "underline",
                                      cursor: "pointer",
                                    }}
                                  >
                                    Disconnect
                                  </Box>{" "}
                                  )
                                </Box>
                                <Flex align="center" mt={2}>
                                  <Box mr={4}>
                                    Connected Clients ( {clients.length} )
                                  </Box>
                                  {map(
                                    v => (
                                      <Box
                                        mr={4}
                                        px={2}
                                        onClick={() => setClient(v)}
                                        color="#5137C5"
                                        css={{
                                          borderRadius: "3px",
                                          cursor: "pointer",
                                          border: "1px solid #5137C5",
                                          _hover: { opacity: 0.75 },
                                        }}
                                      >
                                        {v}
                                      </Box>
                                    ),
                                    clients
                                  )}
                                </Flex>
                                {client ? (
                                  <>
                                    <Box
                                      mt={4}
                                      mb={2}
                                      fontSize="16px"
                                      fontWeight="bold"
                                      color="#5137C5"
                                    >
                                      Send Message to Client ({client})
                                    </Box>
                                    <Textarea
                                      value={msg2}
                                      onChange={e => setMsg2(e.target.value)}
                                      css={{ border: "1px solid #5137C5" }}
                                    />
                                    <Flex mt={2}>
                                      <Box flex={1} />
                                      <Box
                                        onClick={async () => {
                                          await peer2[client].sendMessage(
                                            JSON.stringify({ msg: msg2 })
                                          )
                                          setMsg2("")
                                        }}
                                        fontSize="16px"
                                        color="#5137C5"
                                        px={2}
                                        css={{
                                          borderRadius: "3px",
                                          cursor: "pointer",
                                          border: "1px solid #5137C5",
                                          _hover: { opacity: 0.75 },
                                        }}
                                      >
                                        Send
                                      </Box>
                                    </Flex>
                                  </>
                                ) : null}
                              </>
                            ) : (
                              <>
                                <Box
                                  onClick={async () => {}}
                                  fontSize="16px"
                                  color="#5137C5"
                                  px={2}
                                  py={1}
                                  css={{
                                    borderRadius: "3px",
                                    cursor: "pointer",
                                    border: "1px solid #5137C5",
                                    _hover: { opacity: 0.75 },
                                  }}
                                >
                                  Launch SU
                                </Box>
                              </>
                            )}
                            <Box as="hr" my={4} />
                            <Box
                              fontSize="20px"
                              fontWeight="bold"
                              color="#5137C5"
                              css={{ textDecoration: "underline" }}
                              mb={2}
                            >
                              Client
                            </Box>
                            {!su && sus.length === 0 ? (
                              <Box
                                onClick={async () => {
                                  hub2 = new Hub("ws://localhost:8080")
                                  hub2.onRegister = id => {
                                    hub2.getSUs()
                                    setCID(id)
                                  }

                                  hub2.onSUs = ids => setSUs(ids)

                                  hub2.onAnswer = async (answer, id) => {
                                    await peer1.setAnswer(answer)
                                    hub2.disconnect()
                                    setTimeout(() => {
                                      peer1.sendMessage(
                                        JSON.stringify({ type: "processes" })
                                      )
                                    }, 1000)
                                  }
                                  hub2.connect()
                                }}
                                fontSize="16px"
                                color="#5137C5"
                                py={1}
                                px={2}
                                css={{
                                  borderRadius: "3px",
                                  cursor: "pointer",
                                  border: "1px solid #5137C5",
                                  _hover: { opacity: 0.75 },
                                }}
                              >
                                List Available SUs
                              </Box>
                            ) : null}
                            <Flex>
                              {map(
                                v => (
                                  <Box
                                    mr={4}
                                    px={2}
                                    onClick={async () => {
                                      peer1 = new WebRTC()
                                      peer1.onDataChannelMessage = msg => {
                                        const _msg = JSON.parse(msg)
                                        if (_msg.type === "processes") {
                                          setProcesses(_msg.processes)
                                        } else {
                                          setMsgs(m2 =>
                                            prepend(
                                              {
                                                id: v,
                                                msg: _msg.msg,
                                                type: "SU",
                                                date: Date.now(),
                                              },
                                              m2
                                            )
                                          )
                                        }
                                      }
                                      peer1.onConnectionStateChange =
                                        status => {
                                          if (status === "disconnected") {
                                            peer1.close()
                                            peer1 = null
                                            setSU(null)
                                          }
                                        }

                                      hub2.sendOffer(
                                        await peer1.createOffer(),
                                        v
                                      )
                                      setSU(v)
                                      setSUs([])
                                    }}
                                    color="#5137C5"
                                    css={{
                                      borderRadius: "3px",
                                      cursor: "pointer",
                                      border: "1px solid #5137C5",
                                      _hover: { opacity: 0.75 },
                                    }}
                                  >
                                    {v}
                                  </Box>
                                ),
                                sus
                              )}
                            </Flex>
                            {!su ? null : (
                              <>
                                <Flex align="center" mb={2}>
                                  <Box mr={4}>
                                    Processes ( {processes.length} )
                                  </Box>
                                  {map(
                                    v => (
                                      <Box
                                        mr={4}
                                        px={2}
                                        onClick={() => setPRC(v)}
                                        color="#5137C5"
                                        css={{
                                          borderRadius: "3px",
                                          cursor: "pointer",
                                          border: "1px solid #5137C5",
                                          _hover: { opacity: 0.75 },
                                        }}
                                      >
                                        {v.slice(0, 5)}
                                      </Box>
                                    ),
                                    processes
                                  )}
                                </Flex>
                                {prc ? (
                                  <>
                                    <Flex mb={2}>
                                      <Box
                                        fontSize="16px"
                                        fontWeight="bold"
                                        color="#5137C5"
                                      >
                                        Send Message to SU ({su} :{" "}
                                        {prc ? prc.slice(0, 5) : ""})
                                      </Box>
                                      <Box flex={1} />
                                      <Box
                                        onClick={() => {
                                          setSU(null)
                                          peer1.close()
                                        }}
                                        color="#5137C5"
                                        css={{
                                          textDecoration: "underline",
                                          cursor: "pointer",
                                        }}
                                      >
                                        Disconnect
                                      </Box>
                                    </Flex>
                                    <Textarea
                                      value={msg}
                                      onChange={e => setMsg(e.target.value)}
                                      css={{ border: "1px solid #5137C5" }}
                                    />

                                    <Flex mt={2}>
                                      <Box flex={1} />
                                      <Box
                                        onClick={async () => {
                                          await peer1.sendMessage(
                                            JSON.stringify({
                                              msg: msg,
                                              pid: prc,
                                              type: "msg",
                                            })
                                          )
                                          setMsg("")
                                        }}
                                        fontSize="16px"
                                        color="#5137C5"
                                        px={2}
                                        css={{
                                          borderRadius: "3px",
                                          cursor: "pointer",
                                          border: "1px solid #5137C5",
                                          _hover: { opacity: 0.75 },
                                        }}
                                      >
                                        Send
                                      </Box>
                                    </Flex>
                                  </>
                                ) : null}
                              </>
                            )}
                          </Box>
                        </Box>
                      </Box>
                      <Box
                        flex={1}
                        p={6}
                        css={{ borderLeft: "1px solid  #5137C5" }}
                      >
                        <Box
                          fontSize="20px"
                          fontWeight="bold"
                          color="#5137C5"
                          css={{ textDecoration: "underline" }}
                          mb={2}
                        >
                          Received Messages
                        </Box>
                        <Box>
                          {map(m => {
                            return (
                              <Flex>
                                <Box mr={4}>
                                  [{m.type} : {m.id} : {m.date}]
                                </Box>
                                <Box>{m.msg}</Box>
                              </Flex>
                            )
                          })(msgs)}
                        </Box>
                      </Box>
                    </>
                  )}
                </Flex>
              </>
            )}
          </Flex>
        </Flex>
        {!init ? null : (
          <Flex
            direction="column"
            flex={1}
            css={{ borderLeft: "1px solid #eee" }}
          >
            <Flex h="50px" align="center" px={4} color="#5137C5">
              <Box>{file ? file.name : "Editor"}</Box>
              <Box flex={1} />
              {!file || file.ext !== "js" ? null : (
                <Flex
                  mr={4}
                  py={1}
                  px={4}
                  fontSize="12px"
                  color="#ddd"
                  bg="#5137C5"
                  css={{
                    borderRadius: "5px",
                    cursor: "pointer",
                    border: "1px solid #5137C5",
                    _hover: { opacity: 0.75 },
                  }}
                  onClick={async () => {
                    const js = editorRef.current.getValue()
                    const p = proc ? ao.p(proc.id) : null
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

                    for (let v of descs) {
                      await v.fn({ require })
                      for (let v2 of v.tests) {
                        const start = Date.now()
                        try {
                          const success = await v2.fn({
                            ao,
                            src,
                            p,
                            duration: Date.now() - start,
                          })
                          v2.res = { success, error: null }
                        } catch (e) {
                          v2.res = {
                            success: false,
                            error: e,
                            duration: Date.now() - start,
                          }
                        }
                      }
                      i++
                    }
                  }}
                >
                  Test
                </Flex>
              )}
              <Flex
                py={1}
                px={4}
                fontSize="12px"
                color="#5137C5"
                css={{
                  borderRadius: "5px",
                  cursor: "pointer",
                  border: "1px solid #5137C5",
                  _hover: { opacity: 0.75 },
                }}
                onClick={async () => {
                  if (confirm("Would you like to delete the file?")) {
                    editorRef.current.setValue("")
                    await lf.removeItem(`file-${file.id}`)
                    const _files = filter(v => file.id !== v.id)(files)
                    await lf.setItem(`files`, _files)
                    setFiles(_files)
                    for (let v of _files) {
                      setFile(v)
                      editorRef.current.setValue(
                        (await lf.getItem(`file-${v.id}`)) ?? ""
                      )
                      break
                    }
                  }
                }}
              >
                Delete
              </Flex>
            </Flex>
            <Flex w="100%">
              <Editor
                width={
                  tab === "Files"
                    ? "calc(100vw - 520px)"
                    : "calc(100vw - 1100px)"
                }
                height="calc(100vh - 130px)"
                theme="vs-dark"
                defaultLanguage={
                  file?.ext === "js" ? "js" : file?.ext ? file.ext : "lua"
                }
                onMount={handleEditorDidMount}
                onChange={async () => {
                  if (file) {
                    const lua = editorRef.current.getValue()
                    await lf.setItem(`file-${file.id}`, lua)
                  }
                  /*
                  for (let v of files)
                    if (v.id === file.id) v.update = Date.now()
                  setFiles(files)
                  await lf.setItem(`files`, files)
                  */
                }}
              />
            </Flex>
          </Flex>
        )}
      </Flex>
      <Flex
        w="100%"
        h="30px"
        align="center"
        justify="flex-end"
        px={4}
        bg="white"
        color="#222"
        css={{
          borderTop: "1px solid #ddd",
          position: "fixed",
          left: 0,
          bottom: 0,
          zIndex: 50,
        }}
      >
        <Box
          mr={3}
          as="a"
          target="_blank"
          href="https://github.com/weavedb/wao"
        >
          <Icon
            size="md"
            color="#666"
            css={{ cursor: "pointer", _hover: { opacity: 0.75 } }}
          >
            <FaGithub />
          </Icon>
        </Box>

        <Box mr={3} as="a" target="_blank" href="https://x.com/waoeco">
          <Icon
            size="md"
            color="#666"
            css={{ cursor: "pointer", _hover: { opacity: 0.75 } }}
          >
            <FaXTwitter />
          </Icon>
        </Box>
        <Box mr={3} as="a" target="_blank" href="https://discord.gg/vCkuVhkugY">
          <Icon
            size="md"
            color="#666"
            css={{ cursor: "pointer", _hover: { opacity: 0.75 } }}
          >
            <FaDiscord />
          </Icon>
        </Box>
        <Box as="a" target="_blank" href="https://docs.wao.eco">
          <Icon
            size="md"
            color="#666"
            css={{ cursor: "pointer", _hover: { opacity: 0.75 } }}
          >
            <FaBookOpen />
          </Icon>
        </Box>
      </Flex>
      {!init ? (
        <Flex
          w="100%"
          h="100%"
          align="center"
          justify="center"
          fontSize="40px"
          bg="#5137C5"
          color="#9C89F6"
          css={{ position: "fixed", top: 0, left: 0, zIndex: 5 }}
        >
          Booting Up...
        </Flex>
      ) : null}
      {!modal ? null : (
        <Flex
          align="center"
          justify="center"
          css={{ position: "fixed", top: 0, left: 0, zIndex: 5 }}
          bg="rgba(1,1,1,0.5)"
          w="100vw"
          h="100vh"
        >
          <Box
            mb="50px"
            w="600px"
            bg="white"
            css={{ borderRadius: "10px", position: "relative" }}
          >
            <Flex
              justify="flex-end"
              w="100%"
              p={2}
              css={{ position: "absolute" }}
            >
              <Icon
                size="md"
                color="#999"
                m={2}
                css={{
                  cursor: "pointer",
                  _hover: { opacity: 0.75 },
                }}
                onClick={() => setModal(false)}
              >
                <FaX />
              </Icon>
            </Flex>
            <Box p={6}>
              <Box color="#5137C5" fontWeight="bold" mb={4} fontSize="20px">
                Create a New File
              </Box>
              <Box fontSize="12px" color="#666" mb={2}>
                File Name
              </Box>
              <Flex align="flex-end">
                <Input
                  flex={1}
                  value={filename}
                  onChange={e => setFilename(e.target.value)}
                />
                <Box mx={2}>.</Box>
                <NativeSelect.Root w="80px">
                  <NativeSelect.Field
                    value={fileext}
                    onChange={e => setFileext(e.target.value)}
                  >
                    <option value="lua">lua</option>
                    <option value="js">js</option>
                    <option value="json">json</option>
                  </NativeSelect.Field>
                  <NativeSelect.Indicator />
                </NativeSelect.Root>
              </Flex>
              <Flex
                align="center"
                justify="center"
                mt={4}
                p={1}
                mb={1}
                color="#5137C5"
                css={{
                  borderRadius: "3px",
                  border: "1px solid #5137C5",
                  cursor: "pointer",
                  _hover: { opacity: 0.75 },
                }}
                onClick={async () => {
                  if (/^\s*$/.test(filename)) return alert("Enter a filename.")
                  const txt = ""
                  const id = generateId()
                  const _file = {
                    name: `${filename}.${fileext}`,
                    update: Date.now(),
                    id,
                    ext: fileext,
                  }
                  const _files = prepend(_file, files)
                  await lf.setItem("files", _files)
                  await lf.setItem(`file-${id}`, txt)
                  setFiles(_files)
                  setFile(_file)
                  monaco.editor.setModelLanguage(
                    editorRef.current.getModel(),
                    fileext === "js" ? "javascript" : fileext
                  )
                  // todo: handle this better
                  setTimeout(() => editorRef.current.setValue(txt), 100)
                  setModal(false)
                }}
              >
                Create
              </Flex>
            </Box>
          </Box>
        </Flex>
      )}
    </>
  )
}
