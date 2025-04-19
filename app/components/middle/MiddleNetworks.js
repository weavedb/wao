import { Box, Flex, Textarea } from "@chakra-ui/react"
import { prepend, includes, map, clone } from "ramda"
import { tags } from "/lib/utils"
import g from "/lib/global"
import use from "/lib/use"
import { DataItem } from "arbundles"
import WebRTC from "/lib/webrtc"
import Hub from "/lib/hub"

export default function Middle() {
  const [hbs, setHBs] = use("hbs")
  const [subs, setSubs] = use("subs")
  const [su, setSU] = use("su")
  const [msg, setMsg] = use("msg")
  const [msg2, setMsg2] = use("msgs2")
  const [prc, setPRC] = use("prc")
  const [processes, setProcesses] = use("processes")
  const [init, setInit] = use("init")
  const [tab, setTab] = use("tab")
  const [msgs, setMsgs] = use("msgs")
  const [networks, setNetworks] = use("networks")
  const [ctype, setCtype] = use("ctype")
  const [cache, setCache] = use("cache")
  const [clients, setClients] = use("clients")
  const [client, setClient] = use("client")
  const [sus, setSUs] = use("sus")
  const [suid, setSUID] = use("suid")
  const [cid, setCID] = use("cid")

  let selNetwork = null
  for (let v of networks) if (v.tag === ctype) selNetwork = v

  return ctype === "hb" ? (
    suid ? (
      <Box flex={1} h="calc(100vh - 110px)" css={{ overflowY: "auto" }}>
        <Flex px={4} fontWeight="bold" color="#5137C5" h="50px" align="center">
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
                    g.hub1.socket.send(
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
                    g.hub1.socket.send(
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
        fontSize="11px"
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
                          await g.peer2[client].sendMessage(
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
                  g.hub2 = new Hub("ws://localhost:8080")
                  g.hub2.onRegister = id => {
                    g.hub2.getSUs()
                    setCID(id)
                  }

                  g.hub2.onSUs = ids => setSUs(ids)

                  g.hub2.onAnswer = async (answer, id) => {
                    await g.peer1.setAnswer(answer)
                    g.hub2.disconnect()
                    setTimeout(() => {
                      g.peer1.sendMessage(JSON.stringify({ type: "processes" }))
                    }, 1000)
                  }
                  g.hub2.connect()
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
                      g.peer1 = new WebRTC()
                      g.peer1.onDataChannelMessage = msg => {
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
                      g.peer1.onConnectionStateChange = status => {
                        if (status === "disconnected") {
                          g.peer1.close()
                          g.peer1 = null
                          setSU(null)
                        }
                      }

                      g.hub2.sendOffer(await g.peer1.createOffer(), v)
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
                        Send Message to SU ({su} : {prc ? prc.slice(0, 5) : ""})
                      </Box>
                      <Box flex={1} />
                      <Box
                        onClick={() => {
                          setSU(null)
                          g.peer1.close()
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
                          await g.peer1.sendMessage(
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
  )
}
