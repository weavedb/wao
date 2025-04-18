import { Box, Flex } from "@chakra-ui/react"
import { map, clone } from "ramda"
import g from "/lib/global"
import use from "/lib/use"
import { useState } from "react"

export default function Middle() {
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
      fontSize="12px"
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
            fontSize="12px"
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
              {v.name ?? "Process"}
            </Box>
            <Box fontSize="10px">{v.id}</Box>
          </Flex>
        ))(module.processes)
      )}
    </Box>
  )
}
