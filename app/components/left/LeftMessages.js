import use from "/lib/use"
import { Box, Flex } from "@chakra-ui/react"
import { DataItem } from "arbundles"
import g from "/lib/global"
import { clone, map } from "ramda"
import { tags } from "/lib/utils"

export default function Left() {
  const [message, setMessage] = use("message")
  const [tab, setTab] = use("tab")
  const [messages, setMessages] = use("messages")
  const buttons = (
    <Flex
      h="50px"
      align="center"
      p={4}
      fontSize="12px"
      css={{ borderBottom: "1px solid #ddd" }}
    >
      {null}
    </Flex>
  )

  return (
    <Box
      fontSize="12px"
      w="315px"
      h="100%"
      css={{ borderRight: "1px solid #ddd", overflowY: "auto" }}
    >
      {buttons}
      {map(v => {
        if (!v.http_msg?.tags && v.http_msg?.item) {
          v.http_msg.tags = new DataItem(v.http_msg.item.binary).tags
        }
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
              let _msg = clone(g.ao.mem.msgs[v.id])
              _msg.id = v.id
              if (_msg.http_msg) setMessage(_msg)
              else {
                if (!_msg.tags) {
                  _msg.tags = new DataItem(_msg.item.binary).tags
                }
                setMessage({
                  item: v.item,
                  res: _msg.res,
                  http_msg: _msg,
                  id: _msg.id,
                  slot: v.slot,
                })
              }
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
              <Flex w="20px" mr={2}>
                [{v.slot}]
              </Flex>
              <Box>{t.Type === "Process" ? "Process" : t.Action}</Box>
            </Flex>
            <Box color={v.id !== message?.id ? "#222" : "#ddd"} fontSize="10px">
              {v.id}
            </Box>
          </Flex>
        )
      })(messages)}
    </Box>
  )
}
