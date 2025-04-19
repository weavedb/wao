import { Box, Flex } from "@chakra-ui/react"
import { map } from "ramda"
import g from "/lib/global"
import use from "/lib/use"
import { useState } from "react"
import { fromNow } from "/lib/utils"

export default function Middle() {
  const [blocks] = use("blocks")
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
        <Box>Blocks / {blocks.length}</Box>
      </Flex>
      <Flex h="30px" bg="#eee" align="center">
        <Box px={3} w="80px" _groupHover={{ color: "white" }}>
          Height
        </Box>
        <Box px={3} fontSize="10px" w="300px">
          ID
        </Box>
        <Box px={3} fontSize="10px" w="80px">
          Txs
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
            onClick={() => g.getBlock(v.id)}
          >
            <Box px={3} w="80px" _groupHover={{ color: "white" }}>
              {v.height}
            </Box>
            <Box px={3} fontSize="10px" w="300px">
              {v.id}
            </Box>
            <Box px={3} fontSize="10px" w="80px">
              {v.txs}
            </Box>
            <Box px={3} fontSize="10px" flex={1}>
              {fromNow(v.timestamp)}
            </Box>
          </Flex>
        ))(blocks)}
      </Box>
    </Box>
  )
}
