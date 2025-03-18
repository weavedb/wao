import { Link, ssr } from "arnext"
import { Image, Box, Flex, Textarea } from "@chakra-ui/react"
import { useEffect, useState } from "react"
import { prepend, reverse, append, map, without } from "ramda"
import { AO, acc } from "wao/web"
import Hub from "../lib/hub"
import WebRTC from "../lib/webrtc"

let peer1 = null
let peer2 = {}

let hub1 = null
let hub2 = null

export default function Home({}) {
  const [clients, setClients] = useState([])
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
                            await peer2[client].sendMessage(msg2)
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
                      hub1 = new Hub("ws://localhost:8080")
                      hub1.onRegister = id => {
                        hub1.registerSU()
                        setSUID(id)
                      }
                      hub1.onOffer = async (offer, id) => {
                        setClients(c => append(id, c))
                        peer2[id] = new WebRTC()
                        peer2[id].onDataChannelMessage = msg => {
                          setMsgs(m =>
                            prepend(
                              { type: "Client", msg, id, date: Date.now() },
                              m
                            )
                          )
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
                          setMsgs(m2 =>
                            prepend(
                              { id: v, msg, type: "SU", date: Date.now() },
                              m2
                            )
                          )
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
                  <Flex mb={2}>
                    <Box fontSize="16px" fontWeight="bold" color="#5137C5">
                      Send Message to SU ({su})
                    </Box>
                    <Box flex={1} />
                    <Box
                      onClick={() => {
                        setSU(null)
                        peer1.close()
                      }}
                      color="#5137C5"
                      css={{ textDecoration: "underline", cursor: "pointer" }}
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
                        await peer1.sendMessage(msg)
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
