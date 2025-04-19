import { useState } from "react"
import { Box, Flex } from "@chakra-ui/react"
import { map } from "ramda"
import g from "/lib/global"
import use from "/lib/use"

export default function MiddleProcesses() {
  const [procs] = use("procs")
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
        <Box>Processes / {procs.length}</Box>
      </Flex>
      <Flex h="30px" bg="#eee" align="center">
        <Box px={3} w="120px" _groupHover={{ color: "white" }}>
          Name
        </Box>
        <Box px={3} fontSize="10px" w="300px">
          TxID
        </Box>
        <Box px={3} fontSize="10px" w="120px">
          Module
        </Box>
        <Box px={3} fontSize="10px" w="80px">
          Messages
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
            g.getProcess(v.id)
          }}
        >
          <Box px={3} w="120px" _groupHover={{ color: "white" }}>
            {v.name}
          </Box>
          <Box px={3} fontSize="10px" w="300px">
            {v.id}
          </Box>
          <Box px={3} fontSize="10px" w="120px">
            {v.module}
          </Box>
          <Box px={3} fontSize="10px" w="80px">
            {v.incoming}
          </Box>
          <Box px={3} fontSize="10px" flex={1}>
            {v.timestamp}
          </Box>
        </Flex>
      ))(procs || [])}
    </Box>
  )
}
