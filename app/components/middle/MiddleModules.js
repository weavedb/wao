import { Box, Flex } from "@chakra-ui/react"
import { map } from "ramda"
import g from "/lib/global"
import use from "/lib/use"
import { useState } from "react"

export default function Middle() {
  const [modules, setModules] = use("modules")
  const [subtab, setSubtab] = useState("Tags")
  const subtabs = ["Tags", "Processes"]
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
        <Box>Modules / {modules.length}</Box>
      </Flex>
      <Flex h="30px" bg="#eee" align="center">
        <Box px={3} w="120px" _groupHover={{ color: "white" }}>
          Name
        </Box>
        <Box px={3} fontSize="10px" w="300px">
          TxID
        </Box>
        <Box px={3} fontSize="10px" w="270px">
          Module Format
        </Box>
        <Box px={3} fontSize="10px" w="80px">
          Memory
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
            g.getModule(v.txid)
          }}
        >
          <Box px={3} w="120px" _groupHover={{ color: "white" }}>
            {v.name ?? v.module}
          </Box>
          <Box px={3} fontSize="10px" w="300px">
            {v.txid}
          </Box>
          <Box px={3} fontSize="10px" w="270px">
            {v.format}
          </Box>
          <Box px={3} fontSize="10px" w="80px">
            {v.memory}
          </Box>
          <Box px={3} fontSize="10px" flex={1}>
            {v.timestamp}
          </Box>
        </Flex>
      ))(modules)}
    </Box>
  )
}
