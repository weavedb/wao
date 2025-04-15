// react
import { useRef, useEffect, useState } from "react"
import { PanelGroup, Panel, PanelResizeHandle } from "react-resizable-panels"
import use from "/lib/use"

// chakra-ui
import {
  Input,
  NativeSelect,
  Image,
  Box,
  Flex,
  Textarea,
  Icon,
} from "@chakra-ui/react"
import { Spinner } from "@chakra-ui/react"
import { Toaster, toaster } from "@/components/ui/toaster"
import { Tooltip } from "@/components/ui/tooltip"
import {
  FaRegFolder,
  FaRegFolderOpen,
  FaRegFileCode,
  FaAnglesUp,
  FaAnglesDown,
  FaAngleUp,
  FaAngleDown,
  FaNetworkWired,
  FaDatabase,
  FaHardDrive,
  FaAngleRight,
  FaX,
  FaRegSquarePlus,
  FaFileCirclePlus,
  FaFolderPlus,
} from "react-icons/fa6"

// styles
import GlobalStyle from "/components/GlobalStyle"

// components

import Sidebar from "/components/Sidebar"
import Logo from "/components/Logo"
import Footer from "/components/Footer"
import Editor from "/components/Editor"
import Terminal from "/components/Terminal"
import CreateFileModal from "/components/CreateFileModal"
import CreateFolderModal from "/components/CreateFolderModal"
import LaunchNetworkModal from "/components/LaunchNetworkModal"
import CreateProjectModal from "/components/CreateProjectModal"

//data
import { ctypes } from "/lib/data"

// utils
import chalk from "chalk"
import { src_data_js, src_data_lua } from "/lib/scripts"
import md5 from "md5"

import lf from "localforage"

import {
  ftype,
  dayjs,
  tags,
  wait,
  generateId,
  getPreview,
  useResizeObserver,
  resolvePath,
  DateMS,
} from "/lib/utils"

import g from "/lib/global"

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
  compose,
  without,
  fromPairs,
  filter,
  propEq,
  mergeLeft,
  is,
} from "ramda"

// wao sdk
import { AO, acc, Adaptor } from "wao/web"
import { HB } from "wao"
import { DataItem } from "arbundles"

// websocket/webrtc
import Hub from "../lib/hub"
import WebRTC from "../lib/webrtc"

// guide
import { default_projects, bps, bfiles } from "/lib/guide"

// variables
const hb_url = "http://localhost:10001"
const mod = "JArYBF-D8q2OmZ4Mok00sD2Y_6SYEQ7Hjx-6VZ_jl3g"
let peer1 = null
let peer2 = {}
let hub1 = null
let hub2 = null
let ao = null

export default function Home({}) {
  const filesRef = useRef(null)
  const openFilesRef = useRef(null)
  const editorRef = useRef(null)
  g.editorRef = editorRef
  const fileRef = useRef(null)
  const containerRef = useRef(null)
  g.containerRef = containerRef
  const walletRef = useRef(null)
  const fileInputRef = useRef(null)
  g.walletRef = walletRef

  const { width, height } = useResizeObserver(containerRef)
  const [projects, setProjects] = use("projects")
  const [tab, setTab] = use("tab")
  const [ttab, setTtab] = use("ttab")
  const [ctype, setCtype] = use("ctype")
  const [cache, setCache] = use("cache")
  const [init, setInit] = use("init")
  const [dryrun, setDryrun] = use("dryrun")
  const [proc, setProc] = use("proc")
  const [file, setFile] = use("file")
  const [previewContent, setPreviewContent] = use("previewContent")
  const [logs, setLogs] = use("logs")
  const [modal, setModal] = use("modal")
  const [modal2, setModal2] = use("modal2")
  const [modal3, setModal3] = use("modal3")
  const [modal4, setModal4] = use("modal4")

  const [localFS, setLocalFS] = useState(null)
  const [localFSOpen, setLocalFSOpen] = useState(true)

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

  const [modules, setModules] = useState([])
  const [module, setModule] = useState(null)
  const [procs, setProcs] = useState([])

  const [messages, setMessages] = useState([])
  const [message, setMessage] = useState(null)
  const [files, setFiles] = use("files")
  const [openFiles, setOpenFiles] = use("openFiles")
  const [tests, setTests] = use("tests")
  const [test, setTest] = use("test")
  const [preview, setPreview] = use("preview")
  const [selDir, setSelDir] = use("selDir")
  const [networks, setNetworks] = use("networks")

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
  const clickFile = async v => {
    let opens = clone(openFilesRef.current)
    let exists = false
    for (let v2 of openFilesRef.current) {
      if (v2.id === v.id) {
        exists = true
        break
      }
    }
    if (!exists) {
      let _opens = []
      for (let v2 of openFilesRef.current) {
        _opens.push(v2.id === fileRef.current.id ? v : v2)
      }
      setOpenFiles(_opens)
    }
    setFile(v)
    g.setType(v.ext)
    if (v.pid === "2") {
      hub1.socket.send(
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
      setTimeout(() => editorRef.current.setValue(txt), 100)
      if (v.ext === "md" && preview) setPreviewContent(await getPreview(txt))
    }
  }

  useEffect(() => {
    if (g.fitAddon) g.fitAddon.fit()
  }, [height, width])

  useEffect(() => {
    const handler = e => {
      const el = e.target.closest("a.relative-link")
      if (el) {
        e.preventDefault()
        const href = el.getAttribute("data-href")
        const idmap = []
        const get = (p, d) => {
          for (let v of filesRef.current) {
            if (v.pid === fileRef.current.pid) {
              if (v.path === p) {
                idmap.push({
                  ...v,
                  dirname: d,
                  filename: `${d}${v.name}${v.dir ? "/" : ""}`,
                })
                if (v.dir) get(`${p}${v.id}/`, `${d}${v.name}/`)
              }
            }
          }
        }
        get("/", "/")
        const idmap2 = indexBy(prop("filename"))(idmap)
        const idmap3 = indexBy(prop("id"))(idmap)
        const p = decodeURIComponent(
          resolvePath(idmap3[fileRef.current.id].dirname, href)
        )
        if (idmap2[p]) clickFile(idmap2[p])
      }
    }
    document.addEventListener("click", handler)
    return () => document.removeEventListener("click", handler)
  }, [])

  useEffect(() => {
    ;(async () => {
      const _wallet = await lf.getItem("wallet")
      if (_wallet) {
        g.walletRef.current = _wallet
        setWallet(_wallet)
      }
    })()
  }, [])

  useEffect(() => {
    g.dryrun = dryrun
  }, [dryrun])

  const connectProc = async proc => {
    g.setDryrun = setDryrun
    g.proc = proc
    g.ao = ao

    g.stats = async (txt, wallet) => {
      let owner = null
      txt += `Process:\t${proc.id}\n`
      owner = ao.mem.env[proc.id].owner
      txt += `Owner:\t\t${owner}\n`
      let addr = null
      if (wallet) {
        addr = await wallet.getActiveAddress()
        txt += `Wallet:\t\t${addr}\n\n`
      } else {
        addr = ao.ar.addr
        txt += `Wallet:\t\t${ao.ar.addr}\n\n`
      }
      if (addr !== owner) {
        const txt2 = chalk.red(
          `you are not the owner of the process, which limits certain features like Eval...\n\n`
        )
        txt += txt2
      }
      return txt
    }

    g.connect = async (txt = "") => {
      txt += `connecting to a process... ${proc.id}]\n\n`
      const wallet = await g.getWallet()
      if (!wallet) {
        txt += `wallet not connected, using the preset default wallet...\n\n`
      }
      return await g.stats(txt, wallet)
    }

    g.prompt = async txt => {
      const { res } = await ao.dry({ act: "Eval", pid: proc.id, data: "ao.id" })
      const prompt = res?.Output?.prompt ?? res?.Output?.data?.prompt
      if (prompt) {
        g.term.write("\u001b[2K\r")
        if (txt) {
          txt = `${txt}\n`
        } else if (txt === false) {
          txt = ""
        } else {
          txt = await g.connect()
        }
        console.log(txt)
        g.term.write(txt)
        g.term.write(prompt)

        // Reprint current input
        g.term.write(g.inputRef.current)

        // Restore cursor position
        const tail = g.inputRef.current.slice(g.cur)
        if (tail.length > 0) g.term.write(`\x1b[${tail.length}D`)
      }
    }
    await g.prompt()
    if (!proc && g.term) {
      g.term.write("\u001b[2K\r")
      g.term.write(`select a process......`)
      g.term.write(`${g.inputRef.current}`)
    } else {
      addLog(
        `Connected to Process: ${proc.id}`,
        {},
        {
          title: "Connected to Process!",
          type: "success",
          description: proc.id.slice(0, 30) + "...",
        }
      )
    }
  }

  useEffect(() => {
    ;(async () => {
      if (proc && ao) {
        await connectProc(proc)
      }
    })()
  }, [proc])

  g.setType = fileext =>
    g.monacoRef.current.editor.setModelLanguage(
      editorRef.current.getModel(),
      ftype(fileext)
    )

  useEffect(() => {
    ;(async () => {
      const _prs = await lf.getItem("projects")
      if (_prs) setProjects(_prs)
      const _files = (await lf.getItem("files")) ?? []
      setFiles([...bfiles, ...bps, ..._files])
      const networks = await lf.getItem("networks")
      if (networks) setNetworks(networks)
    })()
  }, [])

  useEffect(() => {
    fileRef.current = file
  }, [file])

  useEffect(() => {
    openFilesRef.current = openFiles
  }, [openFiles])

  useEffect(() => {
    filesRef.current = [...(localFS ?? []), ...files]
  }, [files, localFS])

  useEffect(() => {
    ;(async () => {
      try {
        ao = await new AO({ variant: cache, cache, hb_url }).init(acc[0])
      } catch (e) {
        ao = await new AO({ variant: cache, cache }).init(acc[0])
      }
      g.ao = ao
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
      _setModule(mod)
      setMessage(null)
      setMessages([])
      setInit(true)
    })()
  }, [cache])
  useEffect(() => {
    if (init && ao) {
      ;(async () => {
        let default_process = await lf.getItem("default_process")
        if (!default_process) {
          const { p, pid } = await ao.deploy({ module: mod })
          addLog(
            `Process Spawned: ${pid}`,
            {},
            {
              title: "Process Spawned!",
              type: "success",
              description: pid.slice(0, 30) + "...",
            }
          )
          default_process = { id: pid }
          await lf.setItem("default_process", default_process)
        }
        g.welcome()
        setProc(ao.mem.env[default_process.id])
      })()
    }
  }, [init])
  const modmap = indexBy(prop("txid"))(modules ?? [])

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
        const _file = {
          name: file.name,
          update: DateMS.now(),
          id,
          ext: fileext,
        }
        const _files = prepend(_file, files)
        await lf.setItem("files", _files)
        await lf.setItem(`file-${id}`, txt)
        setFiles(_files)
        setFile(_file)
        event.target.value = ""
        g.setType(fileext.ext)
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
  let selNetwork = null
  for (let v of networks) {
    if (v.tag === ctype) selNetwork = v
  }
  const isPreview = preview && file?.ext === "md"
  g.getDirs = () => {
    let dirs = []
    let dmap = {}
    const get = (pid, _path) => {
      for (let v of files) {
        if (v.dir !== true || v.pid !== pid) continue
        let p = filter(v => v !== "")(v.path.split("/"))
        dmap[v.pid] ??= {}
        if (_path !== v.path) continue
        dmap[v.pid][v.id] = {
          p,
          path: v.path,
          name: v.name,
          id: v.id,
          open: new RegExp(`^${v.path}${v.id}`).test(selDir.path),
          show:
            v.pid === selDir.pid && new RegExp(`^${v.path}`).test(selDir.path),
        }
        if (v.dir) get(pid, `${v.path}${v.id}/`)
      }
    }
    let pdirs = {}
    for (let v of projects) {
      if (includes(v.id, ["0", "3"])) continue
      get(v.id, "/")
      dirs.push({
        project: true,
        name: v.name,
        id: v.id,
        open: false,
      })
      pdirs[v.id] ??= []
      for (let k in dmap[v.id] ?? {}) {
        pdirs[v.id].push(dmap[v.id][k])
      }
    }
    return map(v => {
      const sel = v.id === selDir?.pid
      return (
        <>
          <Flex
            h="25px"
            px={4}
            align="center"
            bg={sel ? "#5137c5" : "#eee"}
            color={sel ? "#ddd" : "#222"}
            onClick={() => setSelDir({ pid: v.id, path: "/" })}
            css={{ cursor: "pointer", _hover: { opacity: 0.75 } }}
          >
            <Icon size="sm" color={sel ? "#ddd" : "#5137c5"} mr={2}>
              {sel ? <FaAngleDown /> : <FaAngleRight />}
            </Icon>
            <Box>{v.name}</Box>
            <Box flex={1} />
          </Flex>
          {map(v2 => {
            return !v2.show ? null : (
              <Flex
                bg={v2.open ? "#5137C5" : "white"}
                color={v2.open ? "#ddd" : "#222"}
                h="25px"
                px={4}
                align="center"
                css={{
                  cursor: "pointer",
                  _hover: { opacity: 0.75 },
                }}
                onClick={() => {
                  if (v2.open) {
                    setSelDir({ pid: v.id, path: v2.path })
                  } else {
                    setSelDir({ pid: v.id, path: v2.path + `${v2.id}/` })
                  }
                }}
              >
                <Box pl={20 * (v2.p.length + 1) + "px"} />
                <Icon
                  size="sm"
                  color={!v2.open ? "#5137C5" : "#ddd"}
                  mr={2}
                  key={v2.path + "-icon"}
                >
                  {v2.open ? <FaRegFolderOpen /> : <FaRegFolder />}
                </Icon>
                <Box>{v2.name}</Box>
              </Flex>
            )
          })(pdirs[v.id] ?? [])}
        </>
      )
    }, dirs)
  }

  const _projects = [
    ...(localFS
      ? [{ id: "2", name: "Local Computer", open: localFSOpen }]
      : []),
    ...projects,
  ]

  const pmap = indexBy(prop("id"), _projects)
  const _files = [...(localFS ?? []), ...files]
  let dirmap = {}
  for (let v of _files) {
    if (!v.dir) continue
    dirmap[`${v.pid}${v.path}${v.id}/`] = v.open ?? false
  }
  const isShow = v => {
    if (!pmap[v.pid]) return false
    const path = `${v.pid}${v.path}`
    let sp = path.split("/")
    sp.pop()
    sp.shift()
    let ok = true
    if (sp.length > 0) {
      for (let i = 0; i < sp.length; i++) {
        const p = `${v.pid}/${sp.slice(0, sp.length - i).join("/")}/`
        if (!dirmap[p]) {
          ok = false
          break
        }
      }
    }
    return pmap[v.pid].open && ok
  }
  const getFiles = () => {
    let dirs = []
    let dmap = {}
    const get = (pid, _path) => {
      for (let v of _files) {
        if (v.pid !== pid) continue
        let p = filter(v => v !== "")(v.path.split("/"))
        dmap[v.pid] ??= {}
        if (_path !== v.path) continue
        dmap[v.pid][v.id] = mergeLeft({ p, show: isShow(v) }, v)
        get(pid, `${v.path}${v.id}/`)
      }
    }
    let pdirs = {}
    for (let v of _projects) {
      get(v.id, "/")
      dirs.push({ project: true, name: v.name, id: v.id, open: false })
      pdirs[v.id] ??= []
      for (let k in dmap[v.id] ?? {}) pdirs[v.id].push(dmap[v.id][k])
    }
    return pdirs
  }
  const pfiles = getFiles()
  const addLog = (desc, opt = {}, toast) => {
    let log = { desc, ...opt }
    log.date ??= DateMS.now()
    setLogs([...logs, log])
    if (toast) toaster.create(toast)
  }
  const top = (
    <Flex w="100%" bg="white" css={{ borderBottom: "1px solid #ddd" }}>
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
      <Flex w="315px" justify="center">
        <Input
          fontSize="12px"
          h="30px"
          align="center"
          w="315px"
          css={{ border: "1px solid #5137C5", borderRadius: "0px" }}
          placeholder="search modules / processes / messages"
          onKeyDown={e => {
            if (e.code === "Enter") alert("Search is coming!")
          }}
        />
      </Flex>
      <Flex fontSize="10px" px={4} align="center">
        <Flex
          align="center"
          css={{
            cursor: "pointer",
            _hover: { opacity: 0.75 },
          }}
          onClick={() => setTab("Networks")}
        >
          <Box mr={2} css={{ borderRadius: "3px" }}>
            {cache}
          </Box>
        </Flex>
        {!module ? null : (
          <>
            <Icon size="sm" color="#5137C5" mb="1px">
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
              <Box mr={2} css={{ borderRadius: "3px" }}>
                {modmap[module.id].name}
              </Box>
            </Flex>
          </>
        )}
        {!proc ? null : (
          <>
            <Icon size="sm" color="#5137C5" mb="1px">
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
        {!message ? null : (
          <>
            <Icon size="sm" color="#5137C5" mb="1px">
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
                <Box px={2} bg="#bbb" css={{ borderRadius: "3px 0 0 3px" }}>
                  {message.slot}
                </Box>
                <Box px={2} bg="#ddd" css={{ borderRadius: "0 3px 3px 0" }}>
                  {act}
                </Box>
                <Box ml={3}>{message?.id}</Box>
              </Flex>
            </Flex>
          </>
        )}
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
            g.walletRef.current = null
            await lf.removeItem("wallet")
            addLog(
              `Wallet Disconnected: ${wallet.address}`,
              {},
              {
                type: "warning",
                description: "Wallet Disconnected!",
              }
            )
            if (g.prompt) {
              await g.prompt(
                await g.stats(
                  chalk.red(
                    `wallet disconnected, using the preset default wallet now...\n\n`
                  )
                )
              )
            }
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
              g.walletRef.current = { address: userAddress }
              await lf.setItem("wallet", { address: userAddress })
              addLog(
                `Wallet Connected: ${userAddress}`,
                {},
                {
                  type: "success",
                  description: "Wallet Connected!",
                }
              )
              if (g.prompt) {
                await g.prompt(
                  await g.stats(
                    chalk.green(`wallet connected!\n\n`),
                    arweaveWallet
                  )
                )
              }
            } catch (e) {
              alert("Arweave wallet not found")
            }
          }}
        >
          Connect Wallet
        </Flex>
      )}
    </Flex>
  )
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
            px={4}
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
              ;({ pid, p } = await ao.deploy({ module: module.id, jwk }))
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
              for (let k in ao.mem.modules) mmap[ao.mem.modules[k]] = k
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
              px={4}
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
                  const p = ao.p(proc.id)
                  const lua = editorRef.current.getValue()
                  const jwk = await g.getWallet()
                  //if (!jwk) return alert("wallet not connected")
                  const res = await p.msg("Eval", { data: lua, jwk })
                  console.log(res)
                }
              }}
            >
              Eval
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
            px={4}
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
              ref={fileInputRef}
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
              FS : 9090
            </Flex>
          ) : (
            <Flex
              ml={3}
              fontSize="10px"
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
                hub1 = new Hub("ws://localhost:9090")
                g.hub1 = hub1
                hub1.onMsg = async obj => {
                  console.log("New FS Msg:", obj)
                  if (obj.subtype === "content") {
                    const ext = obj.path.split(".").pop()
                    const file = fileRef.current
                    const id = md5(
                      `2${obj.path}`.replace(new RegExp("/", "g"), "#")
                    )
                    await lf.setItem(`file-${id}`, obj.content)
                    if (file?.id === id) {
                      setTimeout(() => {
                        g.setType(ext)
                        editorRef.current.setValue(obj.content)
                      }, 100)
                    }
                  } else if (obj.subtype === "dir_change") {
                    updateDir(obj.dir)
                  }
                }
                hub1.onClose = () => setWSID(null)

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
              HUB : 8080
            </Flex>
          ) : (
            <Flex
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
              onClick={async () => {
                const processes = keys(ao.mem.env)
                if (processes.length === 0) {
                  const { p, pid, err } = await ao.deploy({
                    src_data: src_data_lua,
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
                            date: DateMS.now(),
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
              PROXY localhost:7070
            </Flex>
          ) : (
            <Flex
              ml={3}
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
              PROXY
            </Flex>
          )}
        </Flex>
      ) : null}
    </Flex>
  )
  const leftpane = !init ? null : (
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
            <Box fontSize="10px" color={v.txid !== proc?.id ? "#222" : "#ddd"}>
              {v.txid}
            </Box>
          </Flex>
        ))(procs)
      ) : tab === "Modules" ? (
        map(v => (
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
              for (let k in ao.mem.modules) mmap[ao.mem.modules[k]] = k
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
            <Box
              fontSize="10px"
              color={v.txid !== module?.id ? "#222" : "#ddd"}
            >
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
                  await lf.setItem("projects", pr)
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
                                  hub1.socket.send(
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
                                    () => editorRef.current.setValue(txt),
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

  const middlepane = (
    <Flex w="100%">
      {!init ? null : tab === "Messages" ? (
        <Flex w="100%">
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
              })(proc?.tags || [])}
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
      ) : tab === "Projects" ? null : tab === "Tests" ? (
        <Flex w="100%">
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
                        if (confirm(`Would you like to switch to ${ctype}?`)) {
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
                                          date: DateMS.now(),
                                        },
                                        m2
                                      )
                                    )
                                  }
                                }
                                peer1.onConnectionStateChange = status => {
                                  if (status === "disconnected") {
                                    peer1.close()
                                    peer1 = null
                                    setSU(null)
                                  }
                                }

                                hub2.sendOffer(await peer1.createOffer(), v)
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
                            <Box mr={4}>Processes ( {processes.length} )</Box>
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
                <Box flex={1} p={6} css={{ borderLeft: "1px solid  #5137C5" }}>
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
  )

  return (
    <>
      <GlobalStyle />
      {!init ? <Logo /> : null}
      <Flex direction="column" h="100vh" w="100vw">
        <Flex h="30px">{top}</Flex>
        <Flex h="calc(100vh - 60px)" w="100vw" css={{ overflow: "hidden" }}>
          <Flex w="50px" h="calc(100vh - 60px)">
            <Sidebar />
          </Flex>
          <Flex w="315px" h="calc(100vh - 60px)">
            {leftpane}
          </Flex>
          <Box
            flex={1}
            h="calc(100vh - 60px)"
            w="100%"
            css={{ overflow: "hidden" }}
          >
            <PanelGroup direction="horizontal">
              {tab !== "Projects" ? (
                <>
                  <Panel defaultSize={30} minSize={20} order={1}>
                    <Box h="calc(100vh - 60px)">{middlepane}</Box>
                  </Panel>
                  <PanelResizeHandle />
                </>
              ) : null}

              <Panel minSize={30} order={2}>
                <PanelGroup
                  direction={tab !== "Projects" ? "vertical" : "horizontal"}
                >
                  <Panel maxSize={75}>
                    <Editor />
                  </Panel>
                  <PanelResizeHandle />
                  <Panel maxSize={75}>
                    <Terminal />
                  </Panel>
                </PanelGroup>
              </Panel>
            </PanelGroup>
          </Box>
        </Flex>
        <Footer />
      </Flex>
      <CreateFileModal />
      <LaunchNetworkModal />
      <CreateProjectModal />
      <CreateFolderModal />
      <Toaster />
    </>
  )
}
