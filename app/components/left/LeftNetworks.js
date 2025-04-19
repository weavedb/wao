import use from "/lib/use"
import { DataItem } from "arbundles"
import { Box, Flex, Icon } from "@chakra-ui/react"
import Hub from "/lib/hub"
import { Spinner } from "@chakra-ui/react"
import { FaNetworkWired } from "react-icons/fa6"
import g from "/lib/global"

import { hb_url, src_data_lua } from "/lib/scripts"
import { Adaptor } from "wao/web"
import WebRTC from "/lib/webrtc"

import {
  without,
  isNil,
  filter,
  append,
  prepend,
  map,
  keys,
  includes,
} from "ramda"
import { generateId, wait, tags } from "/lib/utils"
import lf from "localforage"
import { ctypes } from "/lib/data"

export default function Left() {
  const [cache, setCache] = use("cache")
  const [networks, setNetworks] = use("networks")
  const [message, setMessage] = use("message")
  const [subs, setSubs] = use("subs")
  const [hbs, setHBs] = use("hbs")
  const [msgs, setMsgs] = use("msgs")
  const [tab, setTab] = use("tab")
  const [wsid, setWSID] = use("wsid")
  const [ctype, setCtype] = use("ctype")
  const [modal2, setModal2] = use("modal2")
  const [proc, setProc] = use("proc")
  const [procs, setProcs] = use("procs")
  const [client, setClient] = use("client")
  const [clients, setClients] = use("clients")
  const [suid, setSUID] = use("suid")
  const [psid, setPSID] = use("psid")

  const buttons = (
    <Flex
      h="60px"
      align="center"
      p={4}
      fontSize="12px"
      css={{ borderBottom: "1px solid #ddd" }}
    >
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
                          date: Date.now(),
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
    </Flex>
  )

  const [init, setInit] = use("init")
  return (
    <Box
      fontSize="12px"
      w="315px"
      h="100%"
      css={{ borderRight: "1px solid #ddd", overflowY: "auto" }}
    >
      {buttons}
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
            <Box fontWeight="bold" color={v.key !== ctype ? "#5137C5" : "#ddd"}>
              {v.name}
            </Box>
            <Box color={v.key !== ctype ? "#222" : "#ddd"}>{v.desc}</Box>
          </Flex>
        ))(ctypes)}
      </>
    </Box>
  )
}
