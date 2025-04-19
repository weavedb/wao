import { useState } from "react"
import { Box, Flex } from "@chakra-ui/react"
import { includes, map } from "ramda"
import { tags, short, fromNow } from "/lib/utils"
import g from "/lib/global"
import use from "/lib/use"

export default function MiddleMessages() {
  const [messages] = use("messages")
  const buttons = (
    <Flex
      h="60px"
      justify="center"
      fontSize="10px"
      css={{ borderBottom: "1px solid #ddd" }}
      direction="column"
    >
      <Flex
        fontSize="12px"
        h="30px"
        px={3}
        align="center"
        bg="#5137c5"
        color="#ddd"
        css={{ borderBottom: "1px solid #ddd" }}
      >
        <Box>Messages / {messages.length}</Box>
      </Flex>
      <Flex h="30px" bg="#eee" align="center">
        <Box px={3} w="120px" _groupHover={{ color: "white" }}>
          Action
        </Box>
        <Box px={3} fontSize="10px" w="300px">
          TxID
        </Box>
        <Box px={3} fontSize="10px" w="150px">
          From
        </Box>
        <Box px={3} fontSize="10px" w="150px">
          To
        </Box>
        <Box px={3} fontSize="10px" flex={1}>
          Timestamp
        </Box>
      </Flex>
    </Flex>
  )
  return (
    <Box w="100%">
      {buttons}
      <Box css={{ overflowY: "auto" }} h="calc(100vh - 120px)">
        {map(v => (
          <Flex
            fontSize="10px"
            h="30px"
            align="center"
            css={{
              borderBottom: "1px solid #ddd",
              cursor: "pointer",
              _hover: { color: "#ddd", bg: "#5137C5" },
            }}
            className="group"
            onClick={() => {
              g.getMessage(v.id)
            }}
          >
            <Box px={3} w="120px" _groupHover={{ color: "white" }}>
              {v.act}
            </Box>
            <Box px={3} fontSize="10px" w="300px">
              {v.id}
            </Box>
            <Box px={3} fontSize="10px" w="150px">
              {short(v.from)}
            </Box>
            <Box px={3} fontSize="10px" w="150px">
              {short(v.to)}
            </Box>
            <Box px={3} fontSize="10px" flex={1}>
              {fromNow(v.timestamp)}
            </Box>
          </Flex>
        ))(messages || [])}
      </Box>
    </Box>
  )
}
