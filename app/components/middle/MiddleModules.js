import { Box, Flex } from "@chakra-ui/react"
import { map, clone } from "ramda"
import g from "/lib/global"
import use from "/lib/use"

export default function Middle() {
  const [tab, setTab] = use("tab")
  const [proc, setProc] = use("proc")
  const [module, setModule] = use("module")

  return !module ? null : (
    <Box
      px={4}
      py={2}
      fontSize="11px"
      flex={1}
      h="calc(100vh - 110px)"
      css={{ overflowY: "auto" }}
    >
      <Flex my={2} fontWeight="bold" color="#5137C5">
        Tags
      </Flex>
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
      <Flex mt={4} mb={2} fontWeight="bold" color="#5137C5">
        Processes ( {module.processes.length} )
      </Flex>
      {map(v => (
        <Flex
          py={1}
          align="center"
          css={{
            borderBottom: "1px solid #ddd",
            cursor: "pointer",
            _hover: { opacity: 0.75 },
          }}
          onClick={() => {
            let _proc = clone(g.ao.mem.env[v])
            delete _proc.memory
            _proc.tags = clone(g.ao.mem.msgs[v]?.tags ?? [])
            _proc.id = v
            setProc(_proc)
            setTab("Processes")
          }}
        >
          <Box color="#222">{v}</Box>
        </Flex>
      ))(module.processes)}
    </Box>
  )
}
