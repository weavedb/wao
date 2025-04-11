import markdownIt from "markdown-it"
import { toHtml } from "hast-util-to-html"
//import GithubCSS from "../lib/GithubCSS.js"
import { common, createStarryNight } from "@wooorm/starry-night"
import { Link, ssr } from "arnext"
import { Spinner } from "@chakra-ui/react"
import { Icon } from "@chakra-ui/react"
import { DataItem } from "arbundles"
import { Tooltip } from "@/components/ui/tooltip"
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
  FaCode,
  FaBug,
  FaWallet,
  FaCoins,
  FaCodeCompare,
  FaEnvelopesBulk,
  FaCubes,
  FaRegFloppyDisk,
  FaRegFolder,
  FaRegFolderOpen,
  FaRegFileCode,
  FaAnglesUp,
  FaAnglesDown,
  FaBookOpen,
  FaDiscord,
  FaXTwitter,
  FaGithub,
  FaNetworkWired,
  FaDatabase,
  FaHardDrive,
  FaAngleRight,
  FaX,
} from "react-icons/fa6"
import lf from "localforage"
function generateId() {
  return Math.random().toString(36).substring(2, 15)
}

dayjs.extend(relativeTime)
const wait = ms => new Promise(res => setTimeout(() => res(), ms))
const hb_url = "http://localhost:10001"
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
import { AO, acc, Adaptor } from "wao/web"
import { HB } from "wao"
import Hub from "../lib/hub"
import WebRTC from "../lib/webrtc"
import Editor from "@monaco-editor/react"
import dynamic from "next/dynamic"
const Terminal = dynamic(
  () =>
    import("../lib/xterm").then(mod => {
      return mod
    }),
  { ssr: false }
)

let peer1 = null
let peer2 = {}

let hub1 = null
let hub2 = null
let ao = null
const tags = tags => fromPairs(map(v => [v.name, v.value])(tags))

const src_data = `# WAO LOCALNET`

const src_data_js = `describe("WAO", ()=>{
  it("should run", async ({ ao, p, src })=> {
    // write your test here
  })
})`

const src_data_lua = `Handlers.add("Hello", "Hello", function (msg)
  msg.reply({ Data = "Hello, World!" })
end)`

let global = {
  dryrun: true,
  getWallet: async () => {
    arweaveWallet.connect(
      ["ACCESS_ADDRESS", "SIGN_TRANSACTION", "ACCESS_PUBLIC_KEY"],
      {
        name: "WAO LOCALNET",
      }
    )
    const userAddress = await arweaveWallet.getActiveAddress()
    return userAddress ? arweaveWallet : null
  },
}
const getPreview = async txt => {
  const starryNight = await createStarryNight(common)
  const markdownItInstance = markdownIt({
    highlight(value, lang) {
      const scope = starryNight.flagToScope(lang)
      return toHtml({
        type: "element",
        tagName: "pre",
        properties: {
          className: scope
            ? [
                "highlight",
                "highlight-" +
                  scope.replace(/^source\./, "").replace(/\./g, "-"),
              ]
            : undefined,
        },
        children: scope
          ? /** @type {Array<ElementContent>} */ (
              starryNight.highlight(value, scope).children
            )
          : [{ type: "text", value }],
      })
    },
  })
  return markdownItInstance.render(txt)
}
const readme = { ext: "md", name: "README.md", nodel: true, id: "readme" }
export default function Home({}) {
  const [localFS, setLocalFS] = useState(null)
  const [dryrun, setDryrun] = useState(true)
  const [ttab, setTtab] = useState("lua")
  const [modal, setModal] = useState(false)
  const [modal2, setModal2] = useState(false)
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
  const [wallet, setWallet] = useState(null)
  const [wsid, setWSID] = useState(null)
  const [psid, setPSID] = useState(null)
  const [cid, setCID] = useState(null)
  const [client, setClient] = useState(null)
  const [tab, setTab] = useState("Projects")
  const [init, setInit] = useState(false)
  const [modules, setModules] = useState([])
  const [module, setModule] = useState(null)
  const [procs, setProcs] = useState([])
  const [proc, setProc] = useState(null)
  const [messages, setMessages] = useState([])
  const [message, setMessage] = useState(null)
  const [ctype, setCtype] = useState("ao.WLN.1")
  const [files, setFiles] = useState([readme])
  const [openFiles, setOpenFiles] = useState([readme])
  const [tests, setTests] = useState([])
  const [test, setTest] = useState(null)
  const [file, setFile] = useState(readme)
  const [preview, setPreview] = useState(true)
  const [previewContent, setPreviewContent] = useState("")
  const [filename, setFilename] = useState("")
  const [fileext, setFileext] = useState("js")
  const [monaco, setMonaco] = useState(null)
  const [bigTerminal, setBigTerminal] = useState(false)
  const [cache, setCache] = useState("ao.WLN.1")
  const [ntag, setNtag] = useState("")
  const [nver, setNver] = useState("")
  const [ndesc, setNdesc] = useState("")
  const [networks, setNetworks] = useState([
    { tag: "ao.WLN.1", desc: "WAO LOCALNET 1" },
  ])
  const tabmap = {
    Projects: { icon: <FaCode /> },
    Tests: { icon: <FaBug /> },
    Modules: { icon: <FaCubes /> },
    Processes: { icon: <FaCodeCompare /> },
    Messages: { icon: <FaEnvelopesBulk /> },
    //    Accounts: { icon: <FaWallet /> },
    //    Tokens: { icon: <FaCoins /> },
    //    Storage: { icon: <FaHardDrive /> },
    //    Database: { icon: <FaDatabase /> },
    Networks: { icon: <FaNetworkWired /> },
  }
  const tabs = keys(tabmap)
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
      const _wallet = await lf.getItem("wallet")
      if (_wallet) setWallet(_wallet)
    })()
  }, [])

  useEffect(() => {
    if (global.termRef) {
      global.termRef.current.resize(110, bigTerminal ? 29 : 11)
    }
  }, [bigTerminal])

  useEffect(() => {
    global.dryrun = dryrun
  }, [dryrun])
  useEffect(() => {
    ;(async () => {
      global.setDryrun = setDryrun
      if (proc && ao) {
        global.proc = proc
        global.ao = ao
        global.prompt = async txt => {
          const { res } = await ao.dry({
            act: "Eval",
            pid: proc.id,
            data: "ao.id",
          })
          const prompt = res?.Output?.prompt ?? res?.Output?.data?.prompt
          if (prompt) {
            global.term.write("\u001b[2K\r")
            if (txt) {
              txt = `${txt}\n`
            } else if (txt === false) {
              txt = ""
            } else {
              txt = `connecting to a process... ${proc.id}\n`
            }
            global.term.write(txt)
            global.term.write(prompt)

            // Reprint current input
            global.term.write(global.inputRef.current)

            // Restore cursor position
            const tail = global.inputRef.current.slice(global.cur)
            if (tail.length > 0) {
              global.term.write(`\x1b[${tail.length}D`)
            }
            //global.term.write(`${txt}${prompt}`)
            //global.term.write(`${global.inputRef.current}`)
          }
        }
        await global.prompt()
      }
      if (!proc && global.term) {
        global.term.write("\u001b[2K\r")
        global.term.write(`select a process......`)
        global.term.write(`${global.inputRef.current}`)
      }
    })()
  }, [proc])
  const editorRef = useRef(null)
  const setType = fileext =>
    monaco.editor.setModelLanguage(
      editorRef.current.getModel(),
      fileext === "js"
        ? "javascript"
        : fileext === "ts"
          ? "typescript"
          : fileext === "md"
            ? "markdown"
            : fileext
    )

  useEffect(() => {
    ;(async () => {
      const files = (await lf.getItem("files")) ?? []
      setFiles(files)
      const networks = await lf.getItem("networks")
      if (networks) setNetworks(networks)
    })()
  }, [])
  const fileRef = useRef(null)

  useEffect(() => {
    fileRef.current = file
  }, [file])
  useEffect(() => {
    ;(async () => {
      try {
        ao = await new AO({ variant: cache, cache, hb_url }).init(acc[0])
      } catch (e) {
        ao = await new AO({ variant: cache, cache }).init(acc[0])
      }
      global.ao = ao
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
      setMessage(null)
      setMessages([])
      setInit(true)
    })()
  }, [cache])

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
  function handleEditorDidMount(editor, monaco) {
    editorRef.current = editor
    editorRef.current.setValue(src_data)
    setMonaco(monaco)
    getPreview(src_data).then(setPreviewContent)
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
        console.log(id)
        const fileext = file.name.split(".").pop().toLowerCase()
        const _file = { name: file.name, update: Date.now(), id, ext: fileext }
        const _files = prepend(_file, files)
        await lf.setItem("files", _files)
        await lf.setItem(`file-${id}`, txt)
        setFiles(_files)
        setFile(_file)
        event.target.value = ""
        setType(fileext.ext)
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
  const ttabs = [
    {
      key: "lua",
      name: `LUA EVAL${dryrun ? " ( DRYRUN )" : ""} ${!proc ? "" : " > " + proc.id.slice(0, 7) + "..." + proc.id.slice(-7)}`,
    },
    { key: "log", name: "LOGS" },
  ]
  let selNetwork = null
  for (let v of networks) {
    if (v.tag === ctype) selNetwork = v
  }
  const isPreview = preview && file?.ext === "md"
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
        height="30px"
        w="100%"
        bg="white"
        css={{
          top: 0,
          left: 0,
          position: "fixed",
          borderBottom: "1px solid #ddd",
          zIndex: 6,
        }}
      >
        <Flex w="100%">
          <Flex
            fontWeight="bold"
            color="#5137C5"
            fontSize="12px"
            align="center"
            justify="center"
            w="50px"
          >
            WAO
          </Flex>
          <Flex w="385px" justify="center">
            <Input
              fontSize="12px"
              h="30px"
              align="center"
              w="385px"
              css={{ border: "1px solid #5137C5", borderRadius: "0px" }}
              placeholder="search modules / processes / messages"
              onKeyDown={e => {
                if (e.code === "Enter") alert("Search is coming!")
              }}
            />
          </Flex>
          <Flex flex={1} />
          {wallet ? (
            <Flex
              justify="center"
              fontSize="14px"
              color="#5137C5"
              w="200px"
              px={4}
              align="center"
              css={{
                border: "1px solid #5137C5",
                cursor: "pointer",
                _hover: { opacity: 0.75 },
              }}
              onClick={async () => {
                setWallet(null)
                await lf.removeItem("wallet")
              }}
            >
              {wallet.address.slice(0, 5) + "..." + wallet.address.slice(-5)}
            </Flex>
          ) : (
            <Flex
              justify="center"
              fontSize="14px"
              bg="#5137C5"
              color="#ddd"
              w="200px"
              px={4}
              align="center"
              css={{
                cursor: "pointer",
                _hover: { opacity: 0.75 },
              }}
              onClick={async () => {
                try {
                  arweaveWallet.connect(
                    ["ACCESS_ADDRESS", "SIGN_TRANSACTION", "ACCESS_PUBLIC_KEY"],
                    {
                      name: "WAO LOCALNET",
                    }
                  )
                  const userAddress = await arweaveWallet.getActiveAddress()
                  setWallet({ address: userAddress })
                  await lf.setItem("wallet", { address: userAddress })
                } catch (e) {
                  alert("Arweave wallet not found")
                }
              }}
            >
              Connect Wallet
            </Flex>
          )}
        </Flex>
      </Flex>
      <Flex h="100vh" css={{ zIndex: 0 }} pt="30px">
        {!init ? null : (
          <Flex css={{ overflowY: "auto", borderRight: "1px solid #ddd" }}>
            <Flex direction="column" w="50px">
              {map(v => {
                return (
                  <Tooltip
                    content={v.toUpperCase()}
                    positioning={{ placement: "right-end" }}
                    openDelay={0}
                    closeDelay={0}
                  >
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
                      <Icon size="lg">{tabmap[v].icon}</Icon>
                    </Flex>
                  </Tooltip>
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
            {tab === "Projects" ? (
              <>
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
                    accept=".lua, .js, .json, .md, .ts"
                    style={{ display: "none" }}
                  />
                  Import
                </Flex>
                <Box flex={1} />
                {wsid ? (
                  <Flex
                    ml={4}
                    fontSize="12px"
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
                      if (confirm("Disconnect from WAO FS?")) {
                        hub1.disconnect()
                        setWSID(null)
                      }
                    }}
                  >
                    FS : localhost:9090
                  </Flex>
                ) : (
                  <Flex
                    ml={4}
                    fontSize="12px"
                    color="#ddd"
                    bg="#5137C5"
                    py={1}
                    px={4}
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
                      const updateDir = _localFS => {
                        let lfs = null
                        let fsmap = {}
                        if (localFS) fsmap = indexBy(prop("path"))(localFS)
                        if (_localFS) {
                          lfs = []
                          const listFiles = (fs, p = [], open = true) => {
                            for (let k in fs) {
                              if (typeof fs[k] === "number") {
                                let v = {
                                  name: k,
                                  updated: fs[k],
                                  p,
                                  show: open,
                                }
                                v.path = getP(v)
                                if (fsmap[v.path]) v.show = fsmap[v.path].show

                                lfs.push(v)
                              } else {
                                let v = {
                                  name: k,
                                  show: open,
                                  open: false,
                                  dir: true,
                                  p,
                                }
                                v.path = getP(v)
                                if (fsmap[v.path]) {
                                  v.open = fsmap[v.path].open
                                  v.show = fsmap[v.path].show
                                }
                                lfs.push(v)
                                listFiles(fs[k], [...p, k], false)
                              }
                            }
                          }
                          listFiles(_localFS)
                        }
                        const setOpen = (_path, _open) => {
                          for (let v2 of lfs) {
                            let _path2 = "/" + v2.p.join("/")
                            if (_path && _path2 === _path) {
                              if (_open) {
                                v2.show = true
                              } else {
                                v2.show = false
                              }
                              if (v2.dir) setOpen(_path2 + "/" + v2.name, _open)
                            }
                          }
                        }
                        //for (let v of lfs) setOpen(v.path, v.open)
                        setLocalFS(lfs)
                      }
                      hub1 = new Hub("ws://localhost:9090")
                      hub1.onMsg = async obj => {
                        console.log("New FS Msg:", obj)
                        if (obj.subtype === "content") {
                          const ext = obj.path.split(".").pop()
                          const file = fileRef.current
                          if (file?.id === obj.path) {
                            setTimeout(() => {
                              setType(ext)
                              editorRef.current.setValue(obj.content)
                            }, 100)
                          }
                        } else if (obj.subtype === "dir_change") {
                          console.log(obj)
                          updateDir(obj.dir)
                        }
                      }

                      hub1.onClose = () => {
                        setWSID(null)
                      }
                      hub1.onRegister = msg => {
                        setWSID(msg.id)
                        let lfs = null
                        updateDir(msg.dir)
                      }
                      hub1.connect()
                    }}
                  >
                    Local FS
                  </Flex>
                )}
              </>
            ) : tab === "Networks" ? (
              <Flex align="center" w="100%">
                <Flex
                  mr={4}
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
                  onClick={() => {
                    setModal2(true)
                  }}
                >
                  Launch Network
                </Flex>
                <Box flex={1} />
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
                    Hub : localhost:8080
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
                        const { p, pid, err } = await ao.deploy({ src_data })
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
                            for (let k in ao.mem.modules)
                              mmap[ao.mem.modules[k]] = k
                            setProcs(
                              append(
                                { txid: pid, module: mmap[val.module] },
                                procs
                              )
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
                      hub1.onRegister = msg => {
                        hub1.socket.send(
                          JSON.stringify({ type: "list", target: "hb" })
                        )
                        hub1.registerSU()
                        setSUID(msg.clientId)
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
                    WAO Hub
                  </Flex>
                )}

                {psid ? (
                  <Flex
                    ml={4}
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
                      if (confirm("Disconnect from WAO Proxy?")) {
                        hub1.disconnect()
                        setPSID(null)
                      }
                    }}
                  >
                    Proxy : localhost:7070
                  </Flex>
                ) : (
                  <Flex
                    ml={4}
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
                      const adaptor = new Adaptor({ hb_url, aoconnect: ao.mem })
                      hub1 = new Hub("ws://localhost:7070")
                      hub1.onMsg = async obj => {
                        console.log("New PX Msg:", obj)
                        adaptor.get(obj.req, res => {
                          hub1.socket.send(
                            JSON.stringify({
                              type: "msg",
                              id: obj.id,
                              res: res ?? { status: 404, error: "not found" },
                            })
                          )
                        })
                      }

                      hub1.onClose = () => {
                        setPSID(null)
                      }
                      hub1.onRegister = msg => setPSID(msg.id)
                      hub1.connect()
                    }}
                  >
                    WAO Proxy
                  </Flex>
                )}
              </Flex>
            ) : (
              <>
                <Flex
                  align="center"
                  css={{
                    cursor: "pointer",
                    _hover: { opacity: 0.75 },
                  }}
                  onClick={() => setTab("Networks")}
                >
                  <Box
                    bg="#ddd"
                    mr={2}
                    css={{ borderRadius: "3px" }}
                    px={2}
                    py={1}
                  >
                    {cache}
                  </Box>
                </Flex>
                {!module ||
                !includes(tab, ["Modules", "Processes", "Messages"]) ? null : (
                  <>
                    <Icon size="md" color="#5137C5">
                      <FaAngleRight />
                    </Icon>
                    <Flex
                      ml={2}
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
                    </Flex>
                  </>
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
                        const jwk = await global.getWallet()
                        if (!jwk) return alert("wallet not connected")
                        let pid, p
                        ;({ pid, p } = await ao.deploy({
                          module: module.id,
                          jwk,
                        }))
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
                          const jwk = await global.getWallet()
                          if (!jwk) return alert("wallet not connected")
                          const res = await p.msg("Eval", { data: lua, jwk })
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
                    </Flex>
                  </>
                )}
              </>
            )}
          </Flex>
          <Flex
            flex={1}
            w={init ? (tab === "Projects" ? "385px" : "965px") : "100vw"}
          >
            {!init ? null : tab === "Messages" ? (
              <Flex w="100%">
                <Box
                  w="385px"
                  h="calc(100vh - 110px)"
                  css={{ borderRight: "1px solid #ddd", overflowY: "auto" }}
                >
                  {map(v => {
                    if (!v.http_msg?.tags && v.http_msg?.item) {
                      v.http_msg.tags = new DataItem(
                        v.http_msg.item.binary
                      ).tags
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
                          let _msg = clone(ao.mem.msgs[v.id])
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
                    h="calc(100vh - 110px)"
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
                  h="calc(100vh - 110px)"
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
                    h="calc(100vh - 110px)"
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
                          else {
                            if (!_msg.tags) {
                              _msg.tags = new DataItem(_msg.item.binary).tags
                            }
                            setMessage({
                              res: _msg.res,
                              http_msg: _msg,
                              id: _msg.id,
                              slot: v.slot,
                            })
                          }
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
                  h="calc(100vh - 110px)"
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
                    h="calc(100vh - 110px)"
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
            ) : tab === "Projects" ? (
              <Flex>
                <Box
                  w="385px"
                  h="calc(100vh - 110px)"
                  fontSize="12px"
                  css={{ borderRight: "1px solid #ddd", overflowY: "auto" }}
                >
                  {!localFS ? null : (
                    <Box css={{ borderBottom: "1px solid #ddd" }}>
                      <Flex h="25px" p={4} align="center" bg="#eee">
                        <Icon size="sm" color="#5137C5" mr={2}>
                          <FaRegFloppyDisk />
                        </Icon>
                        <Box>Local Computer ( http://localhost:9090 )</Box>
                      </Flex>
                      {map(v => {
                        return !v.show ? null : (
                          <Flex
                            bg={file?.id === v.path ? "#5137C5" : "white"}
                            color={file?.id === v.path ? "#ddd" : "#222"}
                            h="25px"
                            px={4}
                            align="center"
                            css={{
                              cursor: "pointer",
                              _hover: { opacity: 0.75 },
                            }}
                            onClick={() => {
                              if (v.dir) {
                                let lfs = clone(localFS)
                                let i = 0
                                let _path = null
                                let _open = null
                                const setOpen = (_path, _open) => {
                                  for (let v2 of lfs) {
                                    let _path2 = "/" + v2.p.join("/")
                                    if (_path && _path2 === _path) {
                                      if (_open) {
                                        v2.show = true
                                      } else {
                                        v2.show = false
                                      }
                                      if (v2.dir)
                                        setOpen(_path2 + "/" + v2.name, _open)
                                    }
                                  }
                                }
                                for (let v2 of lfs) {
                                  if (v.path === v2.path) {
                                    v2.open = !v2.open
                                    _path = v.path
                                    _open = v2.open
                                  }
                                  setOpen(_path, _open)
                                }
                                setLocalFS(lfs)
                              } else {
                                const ext = v.name
                                  .split(".")
                                  .pop()
                                  .toLowerCase()
                                setFile({
                                  name: v.name,
                                  id: v.path,
                                  ext,
                                  local: true,
                                })
                                hub1.socket.send(
                                  JSON.stringify({ type: "data", path: v.path })
                                )
                              }
                            }}
                          >
                            <Box pl={20 * (v.p.length + 1) + "px"} />
                            <Icon
                              size="sm"
                              color={file?.id !== v.path ? "#5137C5" : "#ddd"}
                              mr={2}
                              key={v.path + "-icon"}
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
                        )
                      })(localFS)}
                    </Box>
                  )}
                  <Flex h="25px" px={4} align="center" bg="#eee">
                    <Icon size="sm" color="#5137C5" mr={2}>
                      <FaRegFloppyDisk />
                    </Icon>
                    <Box>Quick Start Guide</Box>
                  </Flex>
                  {map(v => (
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
                          const txt = (await lf.getItem(`file-${v.id}`)) ?? ""
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
                          setType(v.ext)
                          // todo: handle this better
                          setTimeout(() => editorRef.current.setValue(txt), 100)
                          if (v.ext === "md" && preview) {
                            setPreviewContent(await getPreview(txt))
                          }
                        }}
                      >
                        <Box pl={20 * 1 + "px"} />
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
                  ))([readme])}
                  <Flex h="25px" px={4} align="center" bg="#eee">
                    <Icon size="sm" color="#5137C5" mr={2}>
                      <FaRegFloppyDisk />
                    </Icon>
                    <Box>Web Project</Box>
                  </Flex>
                  {map(v => (
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
                          const txt = (await lf.getItem(`file-${v.id}`)) ?? ""
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
                          setType(v.ext)
                          // todo: handle this better
                          setTimeout(() => editorRef.current.setValue(txt), 100)
                          if (v.ext === "md" && preview) {
                            setPreviewContent(await getPreview(txt))
                          }
                        }}
                      >
                        <Box pl={20 * 1 + "px"} />
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
                  ))(files)}
                </Box>
              </Flex>
            ) : tab === "Tests" ? (
              <Flex w="100%">
                <Box
                  w="385px"
                  h="calc(100vh - 110px)"
                  css={{ borderRight: "1px solid #ddd", overflowY: "auto" }}
                >
                  {map(v => (
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
                  ))(tests)}
                </Box>
                {!test ? null : (
                  <Box
                    px={4}
                    py={2}
                    fontSize="12px"
                    flex={1}
                    h="calc(100vh - 110px)"
                    css={{ overflowY: "auto" }}
                  >
                    <Flex my={2} fontWeight="bold" color="#5137C5">
                      Stats
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
                    })([
                      { name: "File", value: test.file },
                      { name: "Duration", value: `${test.duration} ms` },
                      { name: "Success", value: `${test.success}` },
                      { name: "Fail", value: `${test.fail}` },
                      { name: "Date", value: dayjs(test.date).fromNow() },
                    ])}
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
                        {JSON.stringify(test.tests, undefined, 2)}
                      </Box>
                    </code>
                  </Box>
                )}
              </Flex>
            ) : (
              <>
                <Flex w="100%">
                  <Box
                    w="385px"
                    h="calc(100vh - 110px)"
                    css={{ borderRight: "1px solid #ddd", overflowY: "auto" }}
                  >
                    <Flex
                      h="25px"
                      px={4}
                      align="center"
                      bg="#eee"
                      fontSize="12px"
                    >
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
                            <Box color={v.tag !== ctype ? "#222" : "#ddd"}>
                              {v.desc}
                            </Box>
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
                    <Flex
                      h="25px"
                      px={4}
                      align="center"
                      bg="#eee"
                      fontSize="12px"
                    >
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
                        h="calc(100vh - 110px)"
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
                  ) : selNetwork ? (
                    <>
                      <Box
                        px={4}
                        py={2}
                        fontSize="12px"
                        flex={1}
                        h="calc(100vh - 110px)"
                        css={{ overflowY: "auto" }}
                      >
                        <Flex my={2} fontWeight="bold" color="#5137C5">
                          AO Network
                        </Flex>
                        {map(v => {
                          if (
                            includes(v.name, ["signature", "signature-input"])
                          ) {
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
                                css={{
                                  wordBreak: "break-all",
                                  whiteSpace: "wrap",
                                }}
                              >
                                {v.value}
                              </Box>
                            </Flex>
                          )
                        })([
                          {
                            name: "Data-Protocol",
                            value: selNetwork.tag.split(".")[0],
                          },
                          { name: "Variant", value: selNetwork.tag },
                          { name: "Description", value: selNetwork.desc },
                        ])}
                        {ctype === cache ? (
                          <Flex
                            mt={4}
                            w="100%"
                            py={2}
                            align="center"
                            color="#5137C5"
                            justify="center"
                            css={{
                              border: "1px solid #5137C5",
                              borderRadius: "5px",
                            }}
                          >
                            Currently Running......
                          </Flex>
                        ) : (
                          <Flex
                            mt={4}
                            w="100%"
                            py={2}
                            bg="#5137C5"
                            color="#ddd"
                            justify="center"
                            css={{
                              borderRadius: "5px",
                              cursor: "pointer",
                              _hover: { opacity: 0.75 },
                            }}
                            onClick={() => {
                              if (
                                confirm(`Would you like to switch to ${ctype}?`)
                              ) {
                                setCache(ctype)
                              }
                            }}
                          >
                            Switch Networks
                          </Flex>
                        )}
                      </Box>
                    </>
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
            <Flex h="30px" align="center" color="#5137C5">
              <Flex direction="column" justify="flex-end" h="100%">
                <Box flex={1} px={4}></Box>
                <Flex>
                  {map(v => {
                    const open = v.id === file?.id
                    return (
                      <Flex
                        fontSize="12px"
                        w="150px"
                        h="30px"
                        bg={open ? "#1e1e1e" : "#444"}
                        px={3}
                        color="#c6c6c6"
                        align="center"
                        onClick={async () => {
                          const txt = (await lf.getItem(`file-${v.id}`)) ?? ""
                          setFile(v)
                          setType(v.ext)
                          // todo: handle this better
                          setTimeout(() => editorRef.current.setValue(txt), 100)
                          setPreviewContent(await getPreview(txt))
                        }}
                        css={{
                          borderTop: `3px solid ${open ? "#5137C5" : "#444"}`,
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
                          mr={3}
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
                        <Box w="80px" css={{ overflow: "hidden" }}>
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
                              let opens = filter(v2 => v2.id !== v.id)(
                                openFiles
                              )
                              setOpenFiles(opens)
                              if (open) {
                                let exists = false
                                for (let v of opens) {
                                  exists = true
                                  setFile(v)
                                  const txt =
                                    (await lf.getItem(`file-${v.id}`)) ?? ""
                                  setType(v.ext)
                                  editorRef.current.setValue(txt)
                                  if (v.ext === "md" && preview) {
                                    setPreviewContent(await getPreview(txt))
                                  }

                                  break
                                }
                                if (!exists) {
                                  setFile(readme)
                                  setOpenFiles([readme])
                                  setType(readme.ext)
                                  editorRef.current.setValue(src_data)
                                  setPreview(true)
                                  getPreview(src_data).then(setPreviewContent)
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
              </Flex>
              <Box flex={1} />
              <Flex px={4}>
                {!file || file.ext !== "js" ? null : (
                  <Flex
                    mr={4}
                    py={1}
                    px={4}
                    fontSize="12px"
                    color="#5137C5"
                    css={{
                      borderRadius: "5px",
                      cursor: "pointer",
                      _hover: { opacity: 0.75 },
                    }}
                    onClick={async () => {
                      try {
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
                        const ts = Date.now()
                        let success = 0
                        let fail = 0
                        let res = []
                        for (let v of descs) {
                          let _res = []
                          let _success = 0
                          let _fail = 0
                          await v.fn({ require })
                          for (let v2 of v.tests) {
                            const start = Date.now()
                            try {
                              await v2.fn({
                                ao,
                                src,
                                p,
                              })
                              _res.push({
                                description: v2.desc,
                                success: true,
                                error: null,
                                duration: Date.now() - start,
                              })
                              _success++
                              success++
                            } catch (e) {
                              _res.push({
                                description: v2.desc,
                                success: false,
                                error: e.toString(),
                                duration: Date.now() - start,
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
                          duration: Date.now() - ts,
                          tests: res,
                          success,
                          fail,
                        }
                        console.log(result)
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
                {file?.ext !== "md" ? null : (
                  <Flex
                    py={1}
                    px={4}
                    fontSize="12px"
                    color="#5137C5"
                    css={{
                      borderRadius: "5px",
                      cursor: "pointer",
                      _hover: { opacity: 0.75 },
                    }}
                    onClick={async () => {
                      if (!preview) {
                        const txt = editorRef.current.getValue()
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
                    px={4}
                    fontSize="12px"
                    color="#5137C5"
                    css={{
                      borderRadius: "5px",
                      cursor: "pointer",
                      _hover: { opacity: 0.75 },
                    }}
                    onClick={async () => {
                      const content = editorRef.current.getValue()
                      hub1.socket.send(
                        JSON.stringify({
                          type: "save",
                          content,
                          path: file.id,
                        })
                      )
                    }}
                  >
                    Save
                  </Flex>
                ) : file?.nodel ? null : (
                  <Flex
                    py={1}
                    px={4}
                    fontSize="12px"
                    color="#5137C5"
                    css={{
                      borderRadius: "5px",
                      cursor: "pointer",
                      _hover: { opacity: 0.75 },
                    }}
                    onClick={async () => {
                      if (confirm("Would you like to delete the file?")) {
                        editorRef.current.setValue("")
                        await lf.removeItem(`file-${file.id}`)
                        const _files = filter(v => file.id !== v.id)(files)
                        const _openFiles = filter(v => file.id !== v.id)(
                          openFiles
                        )
                        await lf.setItem(`files`, _files)
                        setFiles(_files)
                        setOpenFiles(_openFiles)
                        let exists = false
                        for (let v of _openFiles) {
                          exists = true
                          setFile(v)
                          setType(v.ext)
                          editorRef.current.setValue(
                            (await lf.getItem(`file-${v.id}`)) ?? ""
                          )
                          break
                        }
                        if (!exists) {
                          setFile(readme)
                          setOpenFiles([readme])
                          setType(readme.ext)
                          editorRef.current.setValue(src_data)
                          setPreview(true)
                          getPreview(src_data).then(setPreviewContent)
                        }
                      }
                    }}
                  >
                    Delete
                  </Flex>
                )}
              </Flex>
            </Flex>
            <Flex
              bg="#1e1e1e"
              px={3}
              fontSize="11px"
              w="100%"
              h="20px"
              color="#999"
              align="center"
            >
              <Box>Quick Start Guide</Box>
              <Icon boxSize="11px" mx={2}>
                <FaAngleRight />
              </Icon>
              <Box>{file?.name}</Box>
              <Box flex={1} />
            </Flex>
            <Flex w="100%">
              {isPreview ? (
                <Flex
                  justify="center"
                  w={
                    tab === "Projects"
                      ? "calc(100vw - 437px)"
                      : "calc(100vw - 1017px)"
                  }
                  h={
                    bigTerminal ? "calc(100vh - 640px)" : "calc(100vh - 335px)"
                  }
                  css={{ overflowY: "auto" }}
                >
                  <Box
                    p="32px"
                    w="894px"
                    className="markdown-body"
                    dangerouslySetInnerHTML={{ __html: previewContent }}
                  />
                </Flex>
              ) : null}
              <Box display={isPreview ? "none" : "block"}>
                <Editor
                  width={
                    tab === "Projects"
                      ? "calc(100vw - 437px)"
                      : "calc(100vw - 1017px)"
                  }
                  height={
                    bigTerminal ? "calc(100vh - 640px)" : "calc(100vh - 335px)"
                  }
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
                  }}
                />
              </Box>
            </Flex>
            <Flex
              fontSize="12px"
              h="25px"
              bg="#1E1E1E"
              color="#999"
              css={{ border: "1px solid #666" }}
            >
              {map(v => {
                return (
                  <Flex
                    flex={1}
                    align="center"
                    css={{
                      borderRight: "1px solid #666",
                      cursor: "pointer",
                      _hover: { opacity: 0.75 },
                    }}
                    px={6}
                    bg={ttab === v.key ? "#5137C5" : ""}
                    color={ttab === v.key ? "#ddd" : "#999"}
                    justify="center"
                    onClick={async () => {
                      if (ttab === v.key && v.key === "lua" && global.prompt) {
                        setDryrun(!dryrun)
                        const on = !global.dryrun
                        global.setDryrun(on)
                        await global.prompt(
                          "toggling dryrun mode...... " + (on ? "on" : "off")
                        )
                      } else {
                        setTtab(v.key)
                      }
                    }}
                  >
                    {v.name}
                  </Flex>
                )
              })(ttabs)}
              <Flex
                w="50px"
                align="center"
                css={{
                  borderRight: "1px solid #666",
                  cursor: "pointer",
                  _hover: { opacity: 0.75 },
                }}
                color={"#999"}
                justify="center"
                onClick={() => setBigTerminal(!bigTerminal)}
              >
                <Icon
                  size="sm"
                  color="#666"
                  css={{ cursor: "pointer", _hover: { opacity: 0.75 } }}
                >
                  {bigTerminal ? <FaAnglesDown /> : <FaAnglesUp />}
                </Icon>
              </Flex>
            </Flex>
            <Box visibility={ttab === "js" ? "block" : "none"}>
              <Box
                w="100%"
                h={bigTerminal ? "505px" : "200px"}
                id="terminal"
                bg="#1E1E1E"
                borderRadius="0"
                css={{ overflow: "hidden" }}
              >
                <Terminal {...{ global }} />
              </Box>
            </Box>
          </Flex>
        )}
      </Flex>

      <Flex
        w="100%"
        h="30px"
        align="center"
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
        <Box fontSize="12px" color="#666">
          LOCALNET v 1.0.0
        </Box>
        <Box flex={1} />
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
          fontSize="30px"
          bg="#5137C5"
          color="#9C89F6"
          direction="column"
          css={{ position: "fixed", top: 0, left: 0, zIndex: 5 }}
        >
          <Flex css={{ position: "relative" }} align="center" justify="center">
            <Spinner
              boxSize="350px"
              css={{ position: "absolute" }}
              borderWidth="5px"
              animationDuration="1s"
            />
            <Image src="/logo.png" boxSize="300px" />
          </Flex>
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
                Create New File
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
                    <option value="md">md</option>
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
                  const id = generateId()
                  const _file = {
                    name: `${filename}.${fileext}`,
                    update: Date.now(),
                    id,
                    ext: fileext,
                  }
                  const txt =
                    fileext === "js"
                      ? src_data_js
                      : fileext === "lua"
                        ? src_data_lua
                        : ""
                  const _files = prepend(_file, files)
                  await lf.setItem("files", _files)
                  await lf.setItem(`file-${id}`, txt)
                  setFiles(_files)
                  console.log(_file)
                  setOpenFiles([...openFiles, _file])
                  setFile(_file)
                  setPreview(false)
                  setType(fileext)
                  // todo: handle this better
                  setTimeout(() => editorRef.current.setValue(txt), 100)
                  setModal(false)
                  setFilename("")
                }}
              >
                Create
              </Flex>
            </Box>
          </Box>
        </Flex>
      )}
      {!modal2 ? null : (
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
                onClick={() => setModal2(false)}
              >
                <FaX />
              </Icon>
            </Flex>
            <Box p={6}>
              <Box color="#5137C5" fontWeight="bold" mb={4} fontSize="20px">
                Launch New AO Network
              </Box>

              <Box>
                <Box fontSize="12px" color="#666" mb={2}>
                  Network Tag
                </Box>
                <Flex align="flex-end">
                  <Input value="ao" disabled={true} w="100px" />
                  <Box mx={2}>.</Box>
                  <Input
                    w="100px"
                    placeholder="WLN"
                    value={ntag}
                    onChange={e => setNtag(e.target.value)}
                  />
                  <Box mx={2}>.</Box>
                  <Input
                    w="100px"
                    placeholder="1"
                    value={nver}
                    onChange={e => setNver(e.target.value)}
                  />
                </Flex>
              </Box>
              <Box flex={1} mt={4}>
                <Box fontSize="12px" color="#666" mb={2}>
                  Description
                </Box>
                <Input
                  placeholder=""
                  value={ndesc}
                  onChange={e => setNdesc(e.target.value)}
                />
              </Box>

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
                  if (/^\s*$/.test(ntag.trim()))
                    return alert("Enter a network name")
                  if (/^\s*$/.test(nver.trim()))
                    return alert("Enter a network version")
                  const tag = `ao.${ntag}.${nver}`
                  for (let v of networks) {
                    if (tag === v.tag) return alert(`${ntag} already exists!`)
                  }

                  const _networks = append({ tag, desc: ndesc }, networks)
                  setNetworks(_networks)
                  setCache(tag)
                  setCtype(tag)
                  setNtag("")
                  setNver("")
                  setNdesc("")
                  setModal2(false)
                  await lf.setItem("networks", _networks)
                }}
              >
                Launch
              </Flex>
            </Box>
          </Box>
        </Flex>
      )}
    </>
  )
}
