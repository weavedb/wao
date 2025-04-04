import { Link, ssr } from "arnext"
import { Icon } from "@chakra-ui/react"
import { Image, Box, Flex, Textarea } from "@chakra-ui/react"
import { useEffect, useState } from "react"
import dayjs from "dayjs"
import relativeTime from "dayjs/plugin/relativeTime"
import { FaAngleRight } from "react-icons/fa6"
dayjs.extend(relativeTime)
const wait = ms => new Promise(res => setTimeout(() => res(), ms))
import {
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
} from "ramda"
import { AO, acc } from "wao/web"
import { HB } from "wao"
import Hub from "../lib/hub"
import WebRTC from "../lib/webrtc"

let peer1 = null
let peer2 = {}

let hub1 = null
let hub2 = null
let ao = null

const src_data = `
-- Initialize a table to store messages
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
end)
`

export default function Home({}) {
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
  const [tab, setTab] = useState("Networks")
  const [init, setInit] = useState(false)
  const [modules, setModules] = useState([])
  const [module, setModule] = useState(null)
  const [procs, setProcs] = useState([])
  const [proc, setProc] = useState(null)
  const [messages, setMessages] = useState([])
  const [message, setMessage] = useState(null)
  const [ctype, setCtype] = useState("hb")

  const tabs = [
    "Networks",
    "Modules",
    "Processes",
    "Messages",
    "Accounts",
    "Tokens",
    "Storage",
    "Database",
  ]
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
  return (
    <>
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
          zIndex: 1,
        }}
      >
        <Flex w="100%" mx={4}>
          <Flex
            fontWeight="bold"
            color="#5137C5"
            fontSize="18px"
            align="center"
            mr={4}
          >
            WAO LOCALNET
          </Flex>
          <Flex flex={1} />
          {suid ? (
            <Flex
              fontSize="16px"
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
              fontSize="16px"
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
                    const tags = tags =>
                      fromPairs(map(v => [v.name, v.value])(tags))
                    const t = tags(obj.message.http_msg.tags)
                    if (t.Type === "Process") {
                      const pid = await ao.spawn(obj.message)
                      const val = ao.mem.env[pid]
                      let mmap = {}
                      for (let k in ao.mem.modules) mmap[ao.mem.modules[k]] = k
                      setProcs(
                        append({ txid: pid, module: [mmap[val.module]] }, procs)
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
          <Flex w="200px" bg="#eee" direction="column">
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
                  }}
                  onClick={() => {
                    if (
                      includes(v, ["Accounts", "Tokens", "Storage", "Database"])
                    ) {
                      return alert("Coming Soon!")
                    }
                    setTab(v)
                  }}
                  bg={v === tab ? "#5137C5" : ""}
                  color={
                    includes(v, ["Accounts", "Tokens", "Storage", "Database"])
                      ? "#999"
                      : v === tab
                        ? "white"
                        : ""
                  }
                  px={4}
                >
                  {v.toUpperCase()}
                </Flex>
              )
            })(tabs)}{" "}
          </Flex>
        )}
        <Flex direction="column" flex={1}>
          <Flex
            h="50px"
            align="center"
            p={4}
            fontSize="12px"
            css={{ borderBottom: "1px solid #ddd" }}
          >
            {!module ? null : (
              <Flex
                align="center"
                css={{
                  cursor: "pointer",
                  _hover: { opacity: 0.75 },
                }}
                onClick={() => setTab("Modules")}
              >
                <Box mr={2}>{modmap[module.id].name}</Box>
              </Flex>
            )}
            {!proc ? null : (
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
            {!message ? null : (
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
                      Eval
                    </Box>
                  </Flex>
                  <Box>{message.id}</Box>
                </Flex>
              </>
            )}
          </Flex>
          <Flex flex={1}>
            {!init ? (
              <Flex
                w="100%"
                h="100%"
                align="center"
                justify="center"
                fontSize="40px"
                bg="#5137C5"
                pb="60px"
                color="#9C89F6"
              >
                Booting Up...
              </Flex>
            ) : tab === "Messages" ? (
              <Flex>
                <Box w="370px" css={{ borderRight: "1px solid #ddd" }}>
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
                          setMessage(_msg)
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
                          <Box w="20px" mr={2}>
                            [{v.slot}]
                          </Box>
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
                  <Box px={4} py={2} fontSize="12px">
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
                    })(message.http_msg.tags)}
                    <Flex mt={4} fontWeight="bold" mb={2} color="#5137C5">
                      Data
                    </Flex>
                    <code>
                      <Box
                        as="pre"
                        bg="#eee"
                        p={4}
                        css={{ borderRadius: "3px" }}
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
                        css={{ borderRadius: "3px" }}
                      >
                        {JSON.stringify(message.res, undefined, 2)}
                      </Box>
                    </code>
                  </Box>
                )}
              </Flex>
            ) : tab === "Processes" ? (
              <Flex>
                <Box w="370px" css={{ borderRight: "1px solid #ddd" }}>
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
                        setMessages(map(v => ao.mem.msgs[v])(_proc.results))
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
                  <Box px={4} py={2} fontSize="12px">
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
                          setMessage(_msg)
                          setTab("Messages")
                        }}
                      >
                        <Flex color="#222">
                          <Box w="25px" mr={2}>
                            [{v.slot}]
                          </Box>
                          <Box>{v.id}</Box>
                        </Flex>
                      </Flex>
                    ))(messages)}
                  </Box>
                )}
              </Flex>
            ) : tab === "Modules" ? (
              <Flex>
                <Box w="370px" css={{ borderRight: "1px solid #ddd" }}>
                  {map(v => (
                    <Flex
                      h="50px"
                      bg={v.txid === module?.id ? "#5137C5" : "white"}
                      fontSize="12px"
                      p={4}
                      direction="column"
                      justify="center"
                      onClick={() => {
                        let mmap = {}
                        for (let k in ao.mem.modules) {
                          mmap[ao.mem.modules[k]] = k
                        }
                        let _procs = []
                        let mod = clone(ao.mem.txs[v.txid])
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
                  <Box px={4} py={2} fontSize="12px">
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
                          setMessages(map(v => ao.mem.msgs[v])(_proc.results))
                          setTab("Processes")
                        }}
                      >
                        <Box color="#222">{v}</Box>
                      </Flex>
                    ))(module.processes)}
                  </Box>
                )}
              </Flex>
            ) : (
              <>
                <Flex>
                  <Box w="370px" css={{ borderRight: "1px solid #ddd" }}>
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
                      <Box p={4}>
                        <Flex mt={4} mb={2} fontWeight="bold" color="#5137C5">
                          HyperBEAM Nodes ( {hbs.length} )
                        </Flex>
                        {map(v => {
                          return (
                            <Flex
                              py={2}
                              align="center"
                              css={{
                                borderBottom: "1px solid #ddd",
                                cursor: "pointer",
                                _hover: { opacity: 0.75 },
                              }}
                            >
                              <Box color="#222">{v.address}</Box>
                              <Box ml={4} color="#222">
                                {dayjs().fromNow(v.update)}
                              </Box>
                              {subs?.hb?.[v.address]?.["*"] ? (
                                <Flex
                                  fontSize="16px"
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
                                  fontSize="16px"
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
      </Flex>
    </>
  )
}
