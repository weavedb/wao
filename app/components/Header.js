import { Input, Box, Flex, Icon } from "@chakra-ui/react"
import { DataItem } from "arbundles"
import { useState } from "react"
import { FaAngleRight } from "react-icons/fa6"
import use from "/lib/use"
import chalk from "chalk"
import g from "/lib/global"
import lf from "localforage"
import { prop, indexBy, map, clone } from "ramda"
import { getAct, tags } from "/lib/utils"

export default function Header() {
  const [proc, setProc] = use("proc")
  const [module, setModule] = use("module")
  const [modules, setModules] = use("modules")
  const [tab, setTab] = use("tab")
  const [cache, setCache] = use("cache")
  const [message, setMessage] = use("message")
  const [messages, setMessages] = use("messages")
  const [wallet, setWallet] = use("wallet")
  const modmap = indexBy(prop("id"))(modules ?? [])
  const [search, setSearch] = useState("")
  const [modal, setModal] = useState(false)
  const [results, setResults] = useState(null)
  const searchID = search => {
    let results = []
    if (g.ao.mem.wasms[search]) {
      const wasm = g.ao.mem.wasms[search]
      results.push({ id: search, type: "Module" })
    }
    if (g.ao.mem.env[search]) results.push({ id: search, type: "Process" })
    if (g.ao.mem.msgs[search]) results.push({ id: search, type: "Message" })
    const assignment = g.searchAssignment(search)
    if (assignment) results.push({ id: search, type: "Assignment" })
    if (g.ao.mem.txs[search] && !g.ao.mem.txs[search].bundle) {
      results.push({ id: search, type: "Tx" })
    }
    if (results.length === 0) results.push({ id: search, type: "Account" })
    setResults(results)
  }
  return (
    <>
      <Flex w="100%" bg="white" css={{ borderBottom: "1px solid #ddd" }}>
        <Flex
          fontWeight="bold"
          color="#5137C5"
          fontSize="12px"
          align="center"
          justify="center"
          w="60px"
        >
          WAO
        </Flex>
        <Flex w="315px" justify="center">
          <Input
            fontSize="10px"
            h="30px"
            align="center"
            w="315px"
            value={search}
            onClick={() => {
              if (!modal) {
                if (/\b[a-zA-Z0-9_-]{43}\b/.test(search)) {
                  setModal(true)
                  searchID(search)
                }
              }
            }}
            onChange={e => {
              setSearch(e.target.value)
              if (/\b[a-zA-Z0-9_-]{43}\b/.test(e.target.value)) {
                setModal(true)
                searchID(e.target.value)
              } else {
                setModal(false)
              }
            }}
            css={{ border: "1px solid #5137C5", borderRadius: "0px" }}
            placeholder="search modules / processes / messages222"
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
                    {getAct(message)}
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
              g.log(`Wallet Disconnected: ${wallet.address}`, {
                type: "warning",
                description: "Wallet Disconnected!",
              })
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
                g.log(`Wallet Connected: ${userAddress}`, {
                  type: "success",
                  description: "Wallet Connected!",
                })
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
      <>
        {!modal ? null : (
          <Flex
            align="center"
            justify="center"
            css={{
              position: "fixed",
              top: "30px",
              left: 0,
              zIndex: 5,
            }}
            bg="rgba(1,1,1,0.5)"
            w="100vw"
            h="calc(100vh - 30px)"
            onClick={() => setModal(false)}
          >
            <Box
              mb="60px"
              width="315px"
              bg="white"
              css={{ position: "absolute", top: 0, left: "60px" }}
              onClick={e => {
                e.stopPropagation()
              }}
            >
              {(results ?? []).length === 0 ? (
                <Flex
                  h="60px"
                  bg={"white"}
                  fontSize="12px"
                  p={4}
                  direction="column"
                  justify="center"
                  color="#5137C5"
                  css={{
                    borderBottom: "1px solid #ddd",
                    cursor: "pointer",
                    _hover: { opacity: 0.75 },
                  }}
                >
                  Not Found
                </Flex>
              ) : (
                map(v => {
                  return (
                    <Flex
                      h="60px"
                      bg={"white"}
                      fontSize="12px"
                      p={4}
                      direction="column"
                      justify="center"
                      onClick={() => {
                        switch (v.type) {
                          case "Module":
                            g.getModule(v.id)
                            break
                          case "Process":
                            g.getProcess(v.id)
                            break
                          case "Message":
                            g.getMessage(v.id)
                            break
                          case "Tx":
                            g.getTx(v.id)
                            break
                          case "Assignment":
                            g.getAssingment(v.id)
                            break
                          case "Account":
                            g.getEOS(v.id)
                            break
                        }
                        setModal(false)
                      }}
                      css={{
                        borderBottom: "1px solid #ddd",
                        cursor: "pointer",
                        _hover: { opacity: 0.75 },
                      }}
                    >
                      <Box fontWeight="bold" color="#5137C5" mb={1}>
                        {v.type}
                      </Box>
                      <Box fontSize="10px" color={"#222"}>
                        {search}
                      </Box>
                    </Flex>
                  )
                })(results || [])
              )}
            </Box>
          </Flex>
        )}
      </>
    </>
  )
}
