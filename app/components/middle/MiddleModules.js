import { Box, Flex } from "@chakra-ui/react"
import { map, clone } from "ramda"
import g from "/lib/global"
import use from "/lib/use"
import { useState } from "react"

export default function Middle() {
  const [tab, setTab] = use("tab")
  const [proc, setProc] = use("proc")
  const [module, setModule] = use("module")
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

function MiddleOld() {
  const [tab, setTab] = use("tab")
  const [proc, setProc] = use("proc")
  const [module, setModule] = use("module")
  const [subtab, setSubtab] = useState("Tags")
  const subtabs = ["Tags", "Processes"]
  const buttons = (
    <Flex
      h="50px"
      align="center"
      p={4}
      fontSize="10px"
      css={{ borderBottom: "1px solid #ddd" }}
    >
      {map(v => {
        return (
          <Box
            onClick={() => setSubtab(v)}
            mr={4}
            css={{
              cursor: "pointer",
              textDecoration: v === subtab ? "underline" : "",
              color: v === subtab ? "#5137C5" : "#222",
              _hover: {
                opacity: 0.75,
              },
            }}
          >
            {v}
          </Box>
        )
      })(subtabs)}
    </Flex>
  )
  return !module ? null : (
    <Box
      fontSize="11px"
      flex={1}
      h="calc(100vh - 110px)"
      css={{ overflowY: "auto" }}
    >
      {buttons}
      {subtab === "Tags" ? (
        <Box px={4} py={2}>
          {map(v => (
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
              <Box>{v.value}</Box>
            </Flex>
          ))(module.tags)}
        </Box>
      ) : (
        map(v => (
          <Flex
            px={4}
            fontSize="10px"
            direction="column"
            h="50px"
            justify="center"
            css={{
              borderBottom: "1px solid #ddd",
              cursor: "pointer",
              _hover: { color: "#ddd", bg: "#5137C5" },
            }}
            className="group"
            onClick={() => {
              let _proc = clone(g.ao.mem.env[v.id])
              delete _proc.memory
              _proc.tags = clone(g.ao.mem.msgs[v.id]?.tags ?? [])
              _proc.id = v.id
              setProc(_proc)
              setTab("Processes")
            }}
          >
            <Box
              fontWeight="bold"
              color="#5137c5"
              _groupHover={{ color: "white" }}
            >
              {v.name ?? v.module}
            </Box>
            <Box fontSize="10px">{v.id}</Box>
          </Flex>
        ))(module.processes)
      )}
    </Box>
  )
}
