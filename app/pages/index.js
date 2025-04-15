// react
import { useRef, useEffect } from "react"
import { PanelGroup, Panel, PanelResizeHandle } from "react-resizable-panels"
import use from "/lib/use"

// chakra-ui
import { Box, Flex, Icon } from "@chakra-ui/react"
import { Toaster, toaster } from "@/components/ui/toaster"
import { Tooltip } from "@/components/ui/tooltip"
import {
  FaRegFolder,
  FaRegFolderOpen,
  FaAngleDown,
  FaAngleRight,
} from "react-icons/fa6"

// styles
import GlobalStyle from "/components/GlobalStyle"

// components
import Header from "/components/Header"
import Left from "/components/Left"
import Middle from "/components/Middle"
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
import { hb_url } from "/lib/data"

// utils
import chalk from "chalk"

import lf from "localforage"

import {
  ftype,
  getPreview,
  useResizeObserver,
  resolvePath,
  DateMS,
} from "/lib/utils"

import g from "/lib/global"

import { prop, indexBy, includes, clone, map, filter, mergeLeft } from "ramda"

// wao sdk
import { AO, acc } from "wao/web"

// guide
import { bps, bfiles } from "/lib/guide"

// variables
const mod = "JArYBF-D8q2OmZ4Mok00sD2Y_6SYEQ7Hjx-6VZ_jl3g"
g.peer2 = {}

export default function Home({}) {
  const filesRef = useRef(null)
  g.filesRef = filesRef
  const openFilesRef = useRef(null)
  g.openFilesRef = openFilesRef
  const editorRef = useRef(null)
  g.editorRef = editorRef
  const fileRef = useRef(null)
  g.fileRef = fileRef
  const containerRef = useRef(null)
  g.containerRef = containerRef
  const walletRef = useRef(null)
  g.walletRef = walletRef
  const fileInputRef = useRef(null)
  g.fileInputRef = fileInputRef

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

  const [localFS, setLocalFS] = use("localFS")
  const [localFSOpen, setLocalFSOpen] = use("localFSOpen")

  const [subs, setSubs] = use("subs")
  const [clients, setClients] = use("clients")
  const [processes, setProcesses] = use("processes")
  const [prc, setPRC] = use("prc")
  const [msg, setMsg] = use("msg")
  const [msgs, setMsgs] = use("msgs")
  const [msg2, setMsg2] = use("msgs2")
  const [sus, setSUs] = use("sus")
  const [hbs, setHBs] = use("hbs")
  const [su, setSU] = use("su")
  const [suid, setSUID] = use("suid")
  const [wallet, setWallet] = use("wallet")
  const [wsid, setWSID] = use("wsid")
  const [psid, setPSID] = use("psid")
  const [cid, setCID] = use("cid")
  const [client, setClient] = use("client")

  const [modules, setModules] = use("modules")
  const [module, setModule] = use("module")
  const [procs, setProcs] = use("procs")

  const [messages, setMessages] = use("messages")
  const [message, setMessage] = use("message")
  const [files, setFiles] = use("files")
  const [openFiles, setOpenFiles] = use("openFiles")
  const [tests, setTests] = use("tests")
  const [test, setTest] = use("test")
  const [preview, setPreview] = use("preview")
  const [selDir, setSelDir] = use("selDir")
  const [networks, setNetworks] = use("networks")

  g._setModule = id => {
    let mmap = {}
    for (let k in g.ao.mem.modules) {
      mmap[g.ao.mem.modules[k]] = k
    }
    let _procs = []
    let mod = clone(g.ao.mem.txs[id])
    mod.processes = []
    for (let k in g.ao.mem.env) {
      for (let v of g.ao.mem.msgs[k]?.tags ?? []) {
        if (v.name === "Module" && v.value === mod.id) {
          mod.processes.push(k)
          const val = g.ao.mem.env[k]
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
    g.stats = async (txt, wallet) => {
      let owner = null
      txt += `Process:\t${proc.id}\n`
      owner = g.ao.mem.env[proc.id].owner
      txt += `Owner:\t\t${owner}\n`
      let addr = null
      if (wallet) {
        addr = await wallet.getActiveAddress()
        txt += `Wallet:\t\t${addr}\n\n`
      } else {
        addr = g.ao.ar.addr
        txt += `Wallet:\t\t${g.ao.ar.addr}\n\n`
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
      const { res } = await g.ao.dry({
        act: "Eval",
        pid: proc.id,
        data: "ao.id",
      })
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
      g.addLog(
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
      if (proc && g.ao) {
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
        g.ao = await new AO({ variant: cache, cache, hb_url }).init(acc[0])
      } catch (e) {
        g.ao = await new AO({ variant: cache, cache }).init(acc[0])
      }
      await g.ao.mem.init()
      let _modules = []
      let mmap = {}
      for (let k in g.ao.mem.modules) {
        _modules.push({ name: k, txid: g.ao.mem.modules[k] })
        mmap[g.ao.mem.modules[k]] = k
      }
      setModules(_modules)
      let _procs = []
      for (let k in g.ao.mem.env) {
        const val = g.ao.mem.env[k]
        _procs.push({ txid: k, module: mmap[val.module] })
      }
      setProcs(_procs)
      g._setModule(mod)
      setMessage(null)
      setMessages([])
      setInit(true)
    })()
  }, [cache])
  useEffect(() => {
    if (init && g.ao) {
      ;(async () => {
        let default_process = await lf.getItem("default_process")
        if (!default_process) {
          const { p, pid } = await g.ao.deploy({ module: mod })
          g.addLog(
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
        setProc(g.ao.mem.env[default_process.id])
      })()
    }
  }, [init])

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
  g.getFiles = () => {
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
  g.addLog = (desc, opt = {}, toast) => {
    let log = { desc, ...opt }
    log.date ??= DateMS.now()
    setLogs([...logs, log])
    if (toast) toaster.create(toast)
  }

  return (
    <>
      <GlobalStyle />
      {!init ? <Logo /> : null}
      <Flex direction="column" h="100vh" w="100vw">
        <Flex h="30px">
          <Header />
        </Flex>
        <Flex h="calc(100vh - 60px)" w="100vw" css={{ overflow: "hidden" }}>
          <Flex w="50px" h="calc(100vh - 60px)">
            <Sidebar />
          </Flex>
          <Flex w="315px" h="calc(100vh - 60px)">
            <Left />
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
                    <Box h="calc(100vh - 60px)">
                      <Middle />
                    </Box>
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
