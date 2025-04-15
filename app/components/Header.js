import { Input, Box, Flex, Icon } from "@chakra-ui/react"
import { FaAngleRight } from "react-icons/fa6"
import { tags } from "/lib/utils"
import use from "/lib/use"
import chalk from "chalk"
import g from "/lib/global"
import lf from "localforage"
import { prop, indexBy } from "ramda"
export default function Header() {
  const [proc, setProc] = use("proc")
  const [module, setModule] = use("module")
  const [modules, setModules] = use("modules")
  const [tab, setTab] = use("tab")
  const [cache, setCache] = use("cache")
  const [message, setMessage] = use("message")
  const [wallet, setWallet] = use("wallet")
  const modmap = indexBy(prop("txid"))(modules ?? [])
  let act = null
  if (message) {
    const t = tags(message.http_msg.tags)
    act = t.Type === "Process" ? "Process" : t.Action
  }

  return (
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
            g.addLog(
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
}
