import { Box, Flex } from "@chakra-ui/react"
import { includes, map } from "ramda"
import { tags } from "/lib/utils"
import g from "/lib/global"
import use from "/lib/use"

export default function Middle() {
  const [message, setMessage] = use("message")
  const [init, setInit] = use("init")
  const [tab, setTab] = use("tab")

  let meta = []
  let act = null
  if (message) {
    const t = tags(message.http_msg.tags)
    act = t.Type === "Process" ? "Process" : t.Action
    meta.push({ name: "ID", value: message.id })
    meta.push({
      name: "Process",
      value: t.Type === "Process" ? message.id : message.http_msg.process,
    })
    meta.push({ name: "Slot", value: message.slot })
    const tx = g.ao?.mem.txs[message.id]
    if (tx?.bundle) {
      const _tx = g.ao.mem.txs[tx.bundle]
      meta.push({ name: "From", value: _tx.owner })
    }
  }

  return !message ? null : (
    <Box
      px={4}
      py={2}
      fontSize="11px"
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
            <Box flex={1} css={{ wordBreak: "break-all", whiteSpace: "wrap" }}>
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
            <Box flex={1} css={{ wordBreak: "break-all", whiteSpace: "wrap" }}>
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
          {!message.res ? "" : JSON.stringify(message.res, undefined, 2)}
        </Box>
      </code>
    </Box>
  )
}
