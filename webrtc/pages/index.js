import { Link, ssr } from "arnext"
import { Image, Box, Flex, Textarea } from "@chakra-ui/react"
import { useEffect, useState } from "react"
import {
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

export default function Home({}) {
  const [clients, setClients] = useState([])
  const [processes, setProcesses] = useState([])
  const [prc, setPRC] = useState(null)
  const [msg, setMsg] = useState("")
  const [msgs, setMsgs] = useState([])
  const [msg2, setMsg2] = useState("")
  const [sus, setSUs] = useState([])
  const [su, setSU] = useState(null)
  const [suid, setSUID] = useState(null)
  const [cid, setCID] = useState(null)
  const [client, setClient] = useState(null)
  return (
    <>
      <Flex w="100%">
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
                      onClick={() => {
                        setSUID(null)
                        for (let k in peer2) peer2[k].close()
                        peer2 = {}
                        setClients([])
                        setClient(null)
                      }}
                      color="#5137C5"
                      css={{ textDecoration: "underline", cursor: "pointer" }}
                    >
                      Disconnect
                    </Box>{" "}
                    )
                  </Box>
                  <Flex align="center" mt={2}>
                    <Box mr={4}>Connected Clients ( {clients.length} )</Box>
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
                    onClick={async () => {
                      ao = await new AO().init(acc[0])
                      await ao.mem.init()
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
                      const processes = keys(ao.mem.env)
                      if (processes.length === 0) {
                        const { p, pid, err } = await ao.deploy({ src_data })
                        console.log(await p.d("Hello"))
                      }
                      hub1 = new Hub("ws://localhost:8080")
                      hub1.onMsg = async obj => {
                        if (obj.subtype === "dryrun") {
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
                        } else if (obj.subtype === "result") {
                          let { process, message } = obj
                          // check if recovery is ongoing and
                          if (isNil(ao.mem.env[process])) {
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
                              return
                            }
                          }
                          if (!/^--[0-9a-zA-Z_-]{43,44}$/.test(message)) {
                            message = ao.mem.env[process]?.results?.[message]
                          }
                          if (isNil(message)) {
                            hub1.socket.send(
                              JSON.stringify({
                                id: obj.id,
                                status: 404,
                                type: "msg",
                                error: `not found`,
                              })
                            )
                          }
                          const res2 = await ao.result({
                            message,
                            process,
                          })
                          hub1.socket.send(
                            JSON.stringify({
                              id: obj.id,
                              status: 200,
                              type: "msg",
                              msg: JSON.stringify(res2),
                            })
                          )
                        } else {
                          const tags = tags =>
                            fromPairs(map(v => [v.name, v.value])(tags))
                          const t = tags(obj.message.http_msg.tags)
                          if (t.Type === "Process") {
                            const pid = await ao.spawn(obj.message)
                          } else {
                            const m = await ao.message(obj.message)
                          }
                          hub1.socket.send(
                            JSON.stringify({
                              id: obj.id,
                              status: 200,
                              type: "msg",
                              msg: "success",
                            })
                          )
                        }
                      }
                      hub1.onRegister = id => {
                        hub1.registerSU()
                        setSUID(id)
                      }
                      hub1.onOffer = async (offer, id) => {
                        setClients(c => append(id, c))
                        peer2[id] = new WebRTC()
                        peer2[id].onDataChannelMessage = async msg => {
                          const _msg = JSON.parse(msg)
                          if (_msg.type === "msg") {
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
                        peer1.sendMessage(JSON.stringify({ type: "processes" }))
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
                        <Box fontSize="16px" fontWeight="bold" color="#5137C5">
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
      </Flex>

      <Flex justify="center" p={6}></Flex>
    </>
  )
}
