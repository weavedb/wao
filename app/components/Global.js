// react
import { useRef, useEffect } from "react"
import use from "/lib/use"
import { AR } from "wao"

// chakra-ui
import { Box, Flex, Icon } from "@chakra-ui/react"
import { toaster } from "@/components/ui/toaster"

import {
  FaRegFolder,
  FaRegFolderOpen,
  FaAngleDown,
  FaAngleRight,
} from "react-icons/fa6"

//data
import { mod, hb_url } from "/lib/data"

// ao
import { Bundle } from "arbundles"

// utils
import chalk from "chalk"
import lf from "localforage"

import {
  ftype,
  getPreview,
  useResizeObserver,
  resolvePath,
  tags,
  toAddr,
  isWasm64,
} from "/lib/utils"

import g from "/lib/global"

import {
  prop,
  indexBy,
  includes,
  clone,
  map,
  filter,
  mergeLeft,
  addIndex,
  prepend,
} from "ramda"

// wao sdk
import { AO, acc } from "wao/web"

// guide
import { bps, bfiles } from "/lib/guide"

const tg = m => m?.msg?.Tags ?? m.tags ?? m.Tags ?? []

export default function Global({}) {
  g.filesRef = useRef(null)
  g.openFilesRef = useRef(null)
  g.editorRef = useRef(null)
  g.fileRef = useRef(null)
  g.containerRef = useRef(null)
  g.walletRef = useRef(null)
  g.fileInputRef = useRef(null)

  const { width, height } = useResizeObserver(g.containerRef)
  const [blocks, setBlocks] = use("blocks")
  const [entity, setEntity] = use("entity")
  const [projects, setProjects] = use("projects")
  const [tab, setTab] = use("tab")
  const [ctype, setCtype] = use("ctype")
  const [cache, setCache] = use("cache")
  const [init, setInit] = use("init")
  const [dryrun, setDryrun] = use("dryrun")
  const [proc, setProc] = use("proc")
  const [file, setFile] = use("file")
  const [previewContent, setPreviewContent] = use("previewContent")
  const [logs, setLogs] = use("logs")
  const [localFS, setLocalFS] = use("localFS")
  const [localFSOpen, setLocalFSOpen] = use("localFSOpen")
  const [wallet, setWallet] = use("wallet")
  const [modules, setModules] = use("modules")
  const [module, setModule] = use("module")
  const [procs, setProcs] = use("procs")
  const [messages, setMessages] = use("messages")
  const [message, setMessage] = use("message")
  const [files, setFiles] = use("files")
  const [openFiles, setOpenFiles] = use("openFiles")
  const [preview, setPreview] = use("preview")
  const [selDir, setSelDir] = use("selDir")
  const [networks, setNetworks] = use("networks")
  const [terminal, setTerminal] = use("terminal")
  const [wasm64, setWasm64] = use("wasm64")
  useEffect(() => {
    isWasm64().then(support => setWasm64(support))
    g.dryrun = true
    g.peer2 = {}
  }, [])

  useEffect(() => {
    ;(async () => {
      if (proc && g.ao && proc.id !== terminal) await g.connectProc(proc)
      //if (proc) g.updateMsgs()
    })()
  }, [proc])

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
          for (let v of g.filesRef.current) {
            if (v.pid === g.fileRef.current.pid) {
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
          resolvePath(idmap3[g.fileRef.current.id].dirname, href)
        )
        if (idmap2[p]) g.clickFile(idmap2[p])
      }
    }
    document.addEventListener("click", handler)
    return () => document.removeEventListener("click", handler)
  }, [])
  useEffect(() => {
    if (init) {
      switch (tab) {
        case "Processes":
          g.listProcesses()
        case "Modules":
          g.listModules()
        case "Blocks":
          g.listBlocks()
        case "Messages":
          g.listMessages()
      }
    }
  }, [tab, init])
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

  useEffect(() => {
    ;(async () => {
      const _prs = await lf.getItem("projects")
      if (_prs) setProjects(_prs)
      let _files = (await lf.getItem("files")) ?? []
      setFiles([...bfiles, ...bps, ..._files])
      const networks = await lf.getItem("networks")
      if (networks) setNetworks(networks)
    })()
  }, [])

  useEffect(() => {
    g.fileRef.current = file
  }, [file])

  useEffect(() => {
    g.openFilesRef.current = openFiles
  }, [openFiles])

  useEffect(() => {
    g.filesRef.current = [...(localFS ?? []), ...files]
  }, [files, localFS])

  useEffect(() => {
    ;(async () => {
      try {
        g.ao = await new AO({ variant: cache, cache, hb_url, log: false }).init(
          acc[0]
        )
      } catch (e) {
        g.ao = await new AO({ variant: cache, cache, log: false }).init(acc[0])
      }
      await g.ao.mem.init()
      g.listModules()
      g.listProcesses()
      g.listMessages()
      g.listBlocks()
      g._setModule(mod)
      setMessage(null)
      setInit(true)
    })()
  }, [cache])

  useEffect(() => {
    if (init && g.ao) {
      ;(async () => {
        let default_process = await lf.getItem("default_process")
        if (!default_process) {
          const { p, pid } = await g.ao.deploy({
            module: mod,
            tags: { Name: "Default Process" },
          })
          g.logSpawn(pid)
          default_process = { id: pid }
          await lf.setItem("default_process", default_process)
        }
        g.welcome()
        g._setProcess(default_process.id)
      })()
    }
  }, [init])

  g.msg = id => {
    let proc = g.ao.mem.env[id] ?? null
    if (proc) {
      proc = clone(proc)
      delete proc.memory
    }
    let msg = g.ao.mem.msgs[id] ?? null
    if (msg) msg = clone(msg)
    const _tags = clone(tg(msg))
    const t = tags(_tags)
    let tx = g.ao.mem.txs[id] ?? null
    if (tx.bundle) tx = g.ao.mem.txs[tx.bundle]
    if (tx) tx = clone(tx)
    let block = g.ao.mem.blockmap[tx.block] ?? null
    if (block) block = clone(block)
    return { proc, msg, tags: _tags, t, tx, block }
  }
  g.mmap = () => {
    let mmap = {}
    for (let k in g.ao.mem.modules) mmap[g.ao.mem.modules[k]] = k
    return mmap
  }
  g.getEOS = id => {
    let a = {
      type: "Account",
      id,
      outgoing: [],
      incoming: [],
      tokens: [{ ticker: "AO", balance: "0" }],
      spawn: [],
    }
    let mmap = g.mmap()
    for (let k in g.ao.mem.env) {
      const pr = g.ao.mem.env[k]
      let { proc, msg, tags, t, tx, block } = g.msg(k)
      if (pr.owner === id) {
        a.spawn.push({
          name: t.Name,
          id: k,
          module: mmap[pr.module],
          incoming: proc?.results.length,
          timestamp: block?.timestamp,
        })
      }
    }
    for (let k in g.ao.mem.msgs) {
      const msg = g.ao.mem.msgs[k]
      if (msg?.msg?.From === id) {
        let { proc, msg, tags, t, tx, block } = g.msg(k)
        a.outgoing.push({
          id: k,
          act: t.Action,
          from: msg?.msg?.From,
          to: msg?.msg?.Target ?? msg.target,
        })
      } else if (msg?.msg?.Target === id || msg.target === id) {
        let { proc, msg, tags, t, tx, block } = g.msg(k)
        a.incoming.push({
          id: k,
          act: t.Action,
          from: msg?.msg?.From,
          to: msg?.msg?.Target ?? msg.target,
        })
      }
    }
    setEntity(a)
    setTab("Entity")
  }

  g.getAssignment = id => {
    try {
      if (g.ao.mem.txs[id].bundle) {
        const tx = g.ao.mem.txs[g.ao.mem.txs[id].bundle]
        const items = new Bundle(tx.data).items
        for (let v of items) {
          if (v.id === id) {
            const t = tags(tg(v))
            const addr = toAddr(v.owner)
            const block = g.ao.mem.blockmap[tx.block]
            const a = {
              id,
              tags: tg(v),
              target: v.target,
              from: addr,
              timestamp: block.timestamp,
              type: "Assignment",
            }
            setEntity(a)
            setTab("Entity")
            break
          }
        }
      }
    } catch (e) {
      console.log(e)
    }
  }

  g.getTx = id => {
    setEntity({ ...g.ao.mem.txs[id], type: "Tx" })
    setTab("Entity")
  }

  g.getAccount = id => {
    if (g.ao.mem.wasms[id]) {
      g.getModule(id)
    } else if (g.ao.mem.env[id]) {
      g.getProcess(id)
    } else if (!g.ao.mem.msgs[id] && !g.ao.mem.txs[id]) {
      g.getEOS(id)
    } else if (g.ao.mem.msgs[id]) {
      g.getMessage(id)
    } else if (g.ao.mem.txs[id] && !g.ao.mem.txs[id].bundle) {
      g.getTx(id)
    } else {
      g.getAssignment(id)
    }
  }

  g.refmap = () => {
    let refs = {}
    let xrefs = {}
    for (let k in g.ao.mem.msgs) {
      const m = g.ao.mem.msgs[k]
      const t = tags(tg(m))
      const tar = m.target ?? m?.msg?.Target
      if (t.Reference) {
        refs[m.from] ??= {}
        refs[m.from][t.Reference] = { id: k, to: tar, from: m.from }
      }
      if (t["X-Reference"]) {
        xrefs[m.from] ??= {}
        xrefs[m.from][t["X-Reference"]] = { id: k, to: tar, from: m.from }
      }
    }
    return { refs, xrefs }
  }

  g.getMessage = id => {
    let { proc, msg, tags: _tags, t, tx, block } = g.msg(id)
    let m = {
      from: msg?.msg?.From,
      to: msg?.msg?.Target ?? msg.target,
      ...msg,
      name: t.Name ?? null,
      tags: _tags,
      id,
      type: "Message",
      timestamp: block?.timestamp ?? null,
      resulted: [],
      linked: [],
    }
    const r = g.refmap()
    const _getM = id => {
      let { t, msg, block } = g.msg(id)
      return {
        type: t.Type,
        from: msg?.msg?.From ?? msg.from,
        to: msg?.msg?.Target ?? msg.target,
        id,
        timestamp: block?.timestamp ?? 0,
        act: t.Action,
        res: msg.res,
      }
    }
    const getM = (v, pr) => {
      const t2 = tags(tg(v))
      const id = r.refs[pr]?.[t2.Reference]?.id
      if (id) return _getM(id)
      return null
    }
    if (m.res) {
      if (t.Type === "Process") m.process = id
      for (let v of m.res.Messages || []) {
        const m2 = getM(v, m.process)
        if (m2) m.resulted.unshift(m2)
      }
      for (let v of m.res.Assignments || []) {
        const m2 = getM(v, m.process)
        if (m2) m.resulted.unshift(m2)
      }
      for (let v of m.res.Spawns || []) {
        const m2 = getM(v, m.process)
        if (m2) m.resulted.unshift(m2)
      }
    }

    const getLinked = (_m, depth) => {
      if (depth > 4) return
      if (_m.res) {
        if (t.Type === "Process") _m.process = id
        for (let v of _m.res.Messages || []) {
          const m2 = getM(v, _m.process ?? _m.to)
          if (!m2) return
          m2.depth = depth
          if (m2) m.linked.push(m2)
          getLinked(m2, depth + 1)
        }
        for (let v of _m.res.Assignments || []) {
          const m2 = getM(v, _m.process ?? _m.to)
          if (!m2) return
          m2.depth = depth
          if (m2) m.linkedu.push(m2)
        }
        for (let v of _m.res.Spawns || []) {
          const m2 = getM(v, _m.process ?? _m.to)
          if (!m2) return
          m2.depth = depth
          if (m2) m.linked.push(m2)
          getLinked(m2, depth + 1)
        }
      }
    }
    let depth = 0
    if (t.Reference) {
      const from = t["From-Process"]
      const mid = t["Pushed-For"]
      let m2 = _getM(mid)
      m2.depth = 0
      depth = 1
      m.linked.push(m2)
    }
    let m2 = _getM(id)
    m2.depth = depth
    m.linked.push(m2)
    getLinked(m2, depth + 1)
    setEntity(m)
    setTab("Entity")
  }

  g.getProcess = id => {
    let { proc, msg, tags: _tags, t, tx, block } = g.msg(id)
    const p = {
      ...proc,
      name: t.Name ?? null,
      tags: _tags,
      id,
      type: "Process",
      timestamp: block?.timestamp ?? null,
      incoming: [],
      outgoing: [],
      spawn: [],
    }
    let i = 0
    for (let v of proc.results) {
      let { msg, tags, t, tx, block } = g.msg(v)
      p.incoming.push({
        slot: i,
        id: v,
        timestamp: block?.timestamp ?? 0,
        act: t.Action,
        from: msg?.msg?.From,
        to: msg?.msg?.Target ?? msg.target,
      })
      i++
    }
    const mmap = g.mmap()
    for (let k in g.ao.mem.msgs) {
      const m = g.ao.mem.msgs[k]
      const t = tags(tg(m))
      if (t["From-Process"] === id) {
        if (t.Type === "Message") {
          let { msg, tags, t, tx, block } = g.msg(k)
          p.outgoing.push({
            id: k,
            timestamp: block?.timestamp ?? 0,
            act: t.Action,
            to: msg?.msg?.Target ?? msg.target,
            from: msg?.msg?.From,
          })
        } else if (t.Type === "Process") {
          let { proc, msg, tags, t, tx, block } = g.msg(k)
          let name = null
          name = t.Name ?? null
          let timestamp = null
          if (tx.bundle) {
            const bdl = g.ao.mem.txs[tx.bundle]
            const block = g.ao.mem.blockmap[bdl.block]
            timestamp = block.timestamp
          }
          p.spawn.push({
            id: k,
            name,
            module: mmap[proc.module],
            timestamp,
            incoming: g.ao.mem.env[k]?.results.length,
          })
        }
      }
    }
    setEntity(p)
    setTab("Entity")
  }

  g.getModule = id => {
    let mmap = g.mmap()
    let _procs = []
    let mod = clone(g.ao.mem.txs[id])
    mod.name = mmap[id]
    mod.type = "Module"
    if (mod) {
      const tx = g.ao.mem.txs[id]
      let timestamp = null
      const bdl = g.ao.mem.txs[id]
      const block = g.ao.mem.blockmap[bdl.block]
      mod.timestamp = block?.timestamp ?? 0
      mod.processes = []
      for (let k in g.ao.mem.env) {
        for (let v of tg(g.ao.mem.msgs[k])) {
          if (v.name === "Module" && v.value === mod.id) {
            const val = g.ao.mem.env[k]
            let name = null
            if (g.ao.mem.msgs[k]) {
              name = tags(tg(g.ao.mem.msgs[k])).Name ?? null
            }
            const tx = g.ao.mem.txs[k]
            let timestamp = null
            if (tx.bundle) {
              const bdl = g.ao.mem.txs[tx.bundle]
              const block = g.ao.mem.blockmap[bdl.block]
              timestamp = block.timestamp
            }
            mod.processes.push({
              id: k,
              name,
              module: mmap[val.module],
              timestamp,
              outgoing: g.ao.mem.env[k]?.results.length,
            })
            _procs.push({
              id: k,
              module: mmap[val.module],
            })
          }
        }
      }
      setEntity(mod)
      setTab("Entity")
    }
  }

  g.getBlock = id => {
    const block = g.ao.mem.blockmap[id]
    let b = {
      ...block,
      type: "Block",
    }
    let txs = 0
    let msgs = 0
    let _txs = {}
    for (let v of block.txs) {
      const tx = g.ao.mem.txs[v]
      if (tx.bundle) {
        msgs++
        _txs[tx.bundle] ??= { type: "bundle", txs: [], id: tx.bundle }
        _txs[tx.bundle].txs.push(v)
        _txs[tx.bundle].type = "bundle"
      } else {
        _txs[v] ??= { type: "tx", txs: [], id: v }
        txs++
      }
    }
    b.transactions = []
    for (let k in _txs) {
      let tmap = {}
      try {
        const bd = new Bundle(g.ao.mem.txs[k].data)
        for (let v of bd.items) {
          const id = v.id
          const t = tags(tg(v))
          tmap[id] = t.Type
        }
      } catch (e) {}
      _txs[k].txs = map(v => ({ id: v, type: tmap[v] }))(_txs[k].txs)
      b.transactions.push(_txs[k])
    }

    b.tx_count = txs
    b.msg_count = msgs
    setEntity(b)
    setTab("Entity")
  }

  g.listBlocks = () => {
    let _blocks = []
    for (let v of g.ao.mem.blocks) {
      const block = g.ao.mem.blockmap[v]
      _blocks.push({
        id: v,
        timestamp: block.timestamp,
        txs: block.txs.length,
        height: block.height,
      })
    }
    setBlocks(_blocks)
  }

  g.listModules = () => {
    let _modules = []
    let mmap = {}
    for (let k in g.ao.mem.modules) {
      const txid = g.ao.mem.modules[k]
      const tx = g.ao.mem.txs[txid]
      const block = g.ao.mem.blockmap[tx.block]
      const t = tags(tg(tx))
      _modules.push({
        name: k,
        id: txid,
        timestamp: block.timestamp,
        memory: t["Memory-Limit"],
        format: t["Module-Format"],
        compute: t["Compute-Limit"],
      })
      mmap[g.ao.mem.modules[k]] = k
    }
    setModules(_modules)
  }

  g.listProcesses = () => {
    let _procs = []
    let mmap = g.mmap()
    for (let k in g.ao.mem.env) {
      let { proc, msg, tags, t, tx, block } = g.msg(k)
      _procs.unshift({
        name: t.Name,
        id: k,
        module: mmap[proc.module],
        timestamp: block?.timestamp ?? 0,
        incoming: proc.results.length,
      })
    }
    setProcs(_procs)
  }

  g.listMessages = () => {
    let msgs = []
    for (let k in g.ao.mem.msgs) {
      let { proc, msg, tags, t, tx, block } = g.msg(k)
      msgs.unshift({
        from: msg?.msg?.From,
        to: msg?.msg?.Target ?? msg.target,
        id: k,
        timestamp: block?.timestamp ?? 0,
        act: t.Action,
      })
    }
    setMessages(msgs)
  }

  g.updateMsgs = () => {
    if (proc) {
      setMessages(
        addIndex(map)((v, i) => {
          if (!v.http_msg) {
            return {
              http_msg: g.ao.mem.msgs[v],
              id: v,
              slot: i,
            }
          } else {
            return g.ao.mem.msgs[v]
          }
        })(proc.results)
      )
    }
  }

  g.addMsg = mid => {
    if (proc) {
      if (g.ao.mem.msgs[mid]?.process === proc.id) {
        let _proc = clone(proc)
        _proc.results.push(mid)
        setProc(_proc)
      }
    }
  }

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

  g._setModule = id => {
    let mmap = g.mmap()
    let _procs = []
    let mod = clone(g.ao.mem.txs[id])
    if (mod) {
      mod.processes = []
      for (let k in g.ao.mem.env) {
        for (let v of tg(g.ao.mem.msgs[k])) {
          if (v.name === "Module" && v.value === mod.id) {
            const val = g.ao.mem.env[k]
            let name = null
            if (g.ao.mem.msgs[k]) {
              name = tags(tg(g.ao.mem.msgs[k])).Name ?? null
            }
            mod.processes.push({ id: k, name, module: mmap[val.module] })
            _procs.push({ id: k, module: mmap[val.module] })
          }
        }
      }
      setModule(mod)
    }
  }

  g._setProcess = id => {
    let _proc = clone(g.ao.mem.env[id])
    if (_proc) {
      delete _proc.memory
      _proc.tags = clone(tg(g.ao.mem.msgs[id]))
      _proc.id = id
      setProc(_proc)
      g.listProcesses()
    }
  }

  g.clickFile = async v => {
    let opens = clone(g.openFilesRef.current)
    let exists = false
    for (let v2 of g.openFilesRef.current) {
      if (v2.id === v.id) {
        exists = true
        break
      }
    }
    if (!exists) {
      let _opens = []
      for (let v2 of g.openFilesRef.current) {
        _opens.push(v2.id === g.fileRef.current.id ? v : v2)
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
      setTimeout(() => g.editorRef.current.setValue(txt), 100)
      if (v.ext === "md" && preview) setPreviewContent(await getPreview(txt))
    }
  }

  g.connectProc = async proc => {
    setTerminal(proc.id)
    g.setDryrun = setDryrun
    g.proc = proc
    g.stats = async (txt, wallet) => {
      let owner = null
      txt += `Process:\t${proc.id}\n`
      owner = g.ao.mem.env[proc.id]?.owner
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
        g.term.write(txt)
        g.term.write(prompt)
        // Reprint current input
        g.term.write(g.inputRef.current)

        // Restore cursor position
        const tail = g.inputRef.current.slice(g.cur)
        if (tail.length > 0) g.term.write(`\x1b[${tail.length}D`)
        g.plen = g.term.buffer.active.cursorX
        setTimeout(() => (g.plen = g.term.buffer.active.cursorX), 0)
      }
    }
    await g.prompt()
    if (!proc && g.term) {
      g.term.write("\u001b[2K\r")
      g.term.write(`select a process......`)
      g.term.write(`${g.inputRef.current}`)
    } else {
      g.log(`Connected to Process: ${proc.id}`, {
        title: "Connected to Process!",
        type: "success",
        description: proc.id.slice(0, 30) + "...",
      })
    }
  }

  g.setType = fileext =>
    g.monacoRef.current.editor.setModelLanguage(
      g.editorRef.current.getModel(),
      ftype(fileext)
    )
  const _projects = [
    ...(localFS.length > 0
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

  g.log = (desc, toast, opt = {}) => {
    let log = { desc, ...opt }
    log.date ??= Date.now()
    setLogs([...logs, log])
    if (toast) toaster.create(toast)
  }

  g.logSpawn = pid => {
    g.log(`Process Spawned: ${pid}`, {
      title: "Process Spawned!",
      type: "success",
      description: pid.slice(0, 30) + "...",
    })
  }

  g.logMsg = mid => {
    g.log(`New Message: ${mid}`, {
      title: "New Message!",
      type: "success",
      description: mid.slice(0, 30) + "...",
    })
  }

  g.getWallet = async () => {
    if (!g.walletRef.current) return null
    arweaveWallet.connect(
      ["ACCESS_ADDRESS", "SIGN_TRANSACTION", "ACCESS_PUBLIC_KEY"],
      {
        name: "WAO LOCALNET",
      }
    )
    const userAddress = await arweaveWallet.getActiveAddress()
    if (g.walletRef.current.address !== userAddress) {
      return null
    } else {
      return userAddress ? arweaveWallet : null
    }
  }
  return null
}
